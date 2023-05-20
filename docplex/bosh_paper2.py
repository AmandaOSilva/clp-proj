import itertools
import time

from docplex.cp.model import CpoModel
from families import *

SPACE_BETWEEN_SHELVES = 50

SHELVE_THICKNESS = 40
SHELVE_TOP_GAP = 15
SHELVE_LEFT_GAP = 10
SHELVE_INTER_GAP = 10
SHELVE_RIGHT_GAP = 10

BAY_WIDTH = 1200
BAY_HEIGHT = 2400
BAY_DEPTH = 650
BAY_AVAL_HEIGHT = 3000


# prodDim = [l, w, h]
def rotate(model, prodDim, i):  # TODO: permutation ???
    # print(prodDim)
    indexes = model.integer_var_list(3, 0, 2, f"indices_{i}")
    rotated = model.integer_var_list(3, name=f"rotated_{i}", domain=prodDim)
    model.add(model.all_diff(indexes))
    model.add([rotated[i] == model.element(prodDim, indexes[i]) for i in range(3)])

    return rotated


def group_products(model, products, chosen, maxH, avalHeight, topGap):
    groupedProds = []
    dropProds = []
    for i in range(len(products)):
        product = [_, q, l, w, h] = products[i]
        # if min(product[2:]) > avalHeight - topGap or \
        #         (q*l*w*h > (avalHeight-topGap)*BAY_DEPTH*(BAY_WIDTH-SHELVE_INTER_GAP - SHELVE_LEFT_GAP)):
        #     print("droped: ", product, avalHeight, q*l*w*h, (avalHeight-topGap)*BAY_DEPTH*(BAY_WIDTH-SHELVE_INTER_GAP - SHELVE_LEFT_GAP))
        #     dropProds.append(product)
        # else:
        groupedProds.append(group_product(model, product, chosen, i, maxH, topGap))

    return groupedProds, dropProds


def group_product(model, product, chosen, i, maxH, topGap):
    [_, q, l, w, h] = product
    # print(q, l, w, h)
    [nL, nH, nW] = model.integer_var_list(3, 1, q, f"numberProds{i}")
    [rL, rH, rW] = rotate(model, [l, w, h], i)

    model.add(chosen[i] * nL * rL + SHELVE_INTER_GAP + SHELVE_LEFT_GAP <= BAY_WIDTH)
    model.add(chosen[i] * nH * rH + topGap <= maxH)

    model.add(chosen[i] * nW * rW <= BAY_DEPTH)
    model.add((((nW + 1) * rW > BAY_DEPTH) | ((nH == 1) & (nL == 1) & (nW == q))))

    model.add(nL * nH * nW >= q)
    model.add((nL * nH * nW) * 3 <= q * 5)

    maxDim = q * max([l, w, h]) + 10  # todo
    minDim = min([l, w, h])  # todo
    # print(product, maxDim)
    [gL, gH, gW] = model.integer_var_list(3, minDim, maxDim, f"grouped{i}")

    model.add(gL == nL * rL + SHELVE_INTER_GAP)
    model.add(gH == nH * rH)
    model.add(gW == nW * rW)
    return [gL, gH, gW]


def getMaxH(model, avalHeight):
    minHDomain = SPACE_BETWEEN_SHELVES * 2
    if avalHeight == BAY_AVAL_HEIGHT:
        minHDomain = BAY_AVAL_HEIGHT - BAY_HEIGHT
    return model.integer_var(name="maxH", domain=range(minHDomain, avalHeight + 1, SPACE_BETWEEN_SHELVES))


# def boshKnapsack(chosen, groupedProducts, maxL):


def splitChosen(solution, products, groupedProds, dropProds, chosen):
    chosenProducts = []
    remainProducts = dropProds
    for i in range(len(chosen)):
        if solution.get_value(chosen[i]):
            chosenProducts.append([solution.get_value(v) for v in groupedProds[i]])
        else:
            remainProducts.append(products[i])
    return chosenProducts, remainProducts


def bosh_family(products, bayNumber, avalHeight):
    model = CpoModel()

    size = len(products)
    print(size, products[0][0], avalHeight)  # size, family number
    if avalHeight == BAY_AVAL_HEIGHT:
        topGap = SHELVE_TOP_GAP
    else:
        topGap = SHELVE_THICKNESS + SHELVE_TOP_GAP
    maxH = getMaxH(model, avalHeight)

    chosen = model.binary_var_list(size, "chosen")
    groupedProds, dropProds = group_products(model, products, chosen, maxH, avalHeight, topGap)
    groupedSize = len(groupedProds)
    if not groupedProds and dropProds:
        return next_bay(model, avalHeight, bayNumber, [], products, dropProds)

    remainLH = model.integer_var(0, BAY_WIDTH * BAY_AVAL_HEIGHT)
    maxLH = (BAY_WIDTH - SHELVE_LEFT_GAP) * maxH - remainLH

    # model.add(model.scal_prod([gL for [gL, _, _] in groupedProds], chosen) == maxL)
    # model.add(sum([groupedProds[i][0] * chosen[i] for i in range(groupedSize)]) == maxL)
    model.add(model.sum([chosen[i] * groupedProds[i][1] * groupedProds[i][0] for i in range(groupedSize)]) == maxLH)

    # objective = model.count_different(baysIds)
    # objective = model.sum(maxHs) #+ model.count_different(baysIds )*10
    # objective = model.max(baysIds)
    # model.maximize(maxL-maxH*10)
    # one = model.integer_var(0, 1, "one")
    # one =
    model.minimize(remainLH + (1-model.max(chosen)) * 10000000000)
    # model.minimize(remainL)
    # vs = model.integer_var_list(size, 1, 2, "vs")
    # for i in range(size):
    #     model.add((vs[i] == 1) - (chosen[i] == 1) != 0)
    # model.add(model.search_phase(vs+list(itertools.chain(*groupedProds))))
    #  for g in groupedProds:
    #      model.add(model.search_phase(g))

    # vars_search = list(itertools.chain(*[[chosen[i]] + groupedProds[i][::-1] for i in range(groupedSize)]))

    #vars_search = list(itertools.chain(*[groupedProds[i] + [chosen[i]] for i in range(groupedSize)]))[::-1]

    # #print([x.name for x in vars_search])
    # model.add(model.search_phase(vars_search))
    # model.add(model.search_phase(chosen))

    # model.add(model.search_phase(vars=vars_search, varchooser=model.select_smallest(model.var_index(vars_search)),
    #                              valuechooser=model.select_smallest(model.value())))

    # vars_search = chosen+ list(itertools.chain(*groupedProds))
    # model.add(model.search_phase([maxH]))
    # model.add(model.search_phase(vars=vars_search, varchooser=model.select_smallest(model.var_index(chosen)),
    #                              valuechooser=model.select_largest(model.value())))
    # model.search_phase(vars=chosen, varchooser=model.select_largest(model.var_index(chosen)),
    #                     valuechooser=model.select_largest(model.value()))

    # model.add(model.search_phase(list(itertools.chain(*groupedProds))))
    # model.add(model.pack(loads, packIds, weights, nonZero))
    # model.add(model.minimize(nonZero))

    # starting_point = model.create_empty_solution()
    #
    # for c in chosen[:size//2]:
    #     starting_point.add_integer_var_solution(c, 1)
    # # starting_point.add_integer_var_solution(maxL, 1190)
    # # for e in e2[:(len(e2))]:
    # #     starting_point.add_integer_var_solution(e, 1)
    #
    # model.set_starting_point(starting_point)
    # print(model.refine_conflict())
    solution = model.solve(LogVerbosity='Quiet', SearchType='Auto', Workers=16, TimeLimit=120)
    # solutions = model.start_search(SearchType='DepthFirst', Workers=1, LogVerbosity='Terse', TimeLimit=120)
    # 'DepthFirst', 'Restart', 'MultiPoint', 'IterativeDiving', 'Neighborhood', 'Auto')
    # solutions = model.start_search(TemporalRelaxation="Off", LogVerbosity='Terse', SearchType='Auto', Workers=8,
    # TimeLimit=600)
    # for solution in solutions:
    status = solution.get_solve_status()
    print("maxH: {}, remainL: {}, bay: {}".format(solution.get_value(maxH), solution.get_value(remainLH), bayNumber))
    print([solution.get_value(c) for c in chosen])
    # print([solution.get_value(v) for v in vs])
    # print(sum([solution.get_value(groupedProds[i][0])* solution.get_value(chosen[i]) for i in range(size)]))
    if status == "Feasible" or status == "Optimal":
        chosenProducts, remainProducts = splitChosen(solution, products, groupedProds, dropProds, chosen)
        # print([[solution.get_value(gL), solution.get_value(gH), solution.get_value(gW)] for [gL, gH, gW] in
        #        chosenProducts])

        if not remainProducts:
            return True, bayNumber, avalHeight, groupedProds, remainProducts
        elif not chosenProducts:
            return next_bay(model, avalHeight, bayNumber, chosenProducts, products, remainProducts)
        else:
            newAvalHeight = avalHeight - solution.get_value(maxH)
            newBayNumber = bayNumber
            if newAvalHeight < topGap + SPACE_BETWEEN_SHELVES:
                newAvalHeight = BAY_AVAL_HEIGHT
                newBayNumber = bayNumber + 1
            model = None
            return bosh_family(remainProducts, newBayNumber, newAvalHeight)
    else:
        print(status)
        return next_bay(model, avalHeight, bayNumber, [], products, products)

    # solution.write()
    # if solution.isOptmal():
    #     model.end_search()()

    # if solution:
    #     solution.print_solution()


def next_bay(model, avalHeight, bayNumber, chosenProducts, products, remainProducts):
    model = None
    if avalHeight == BAY_AVAL_HEIGHT:
        return False, bayNumber, avalHeight, chosenProducts, remainProducts
    else:
        # result, chosenProducts1, remainProducts1 =bosh_family(products, bayNumber + 1, BAY_AVAL_HEIGHT)
        return bosh_family(products, bayNumber + 1, BAY_AVAL_HEIGHT)


#    print([solution.get_value(e[i]) for i in range(2 * 2)])

def bosh(bayNumber, avalHeight, products):
    # print(Fs[family])
    # products = Fs[family]  # [::-1]  # [:200]  # [[1, 2, 10, 620, 70], [1, 2, 10, 62, 70]]
    print(products)
    return bosh_family(products, bayNumber, avalHeight)


def main():
    bayNumber = 1
    avalHeight = BAY_AVAL_HEIGHT
    # for products in [[[65, 2, 2150, 165, 167]]]:  # Fs[64:-1]: #[66:67]: # [0:2]:  #

    allBeginTime = time.time()
    for products in Fs[0:1]:
        beginTime = time.time()
        isOk, newBayNumber, newAvalHeight, chosenProducts, remainProducts = bosh(bayNumber, avalHeight, products)
        print("Family shelved: ", products[0][0], time.time() - beginTime, "s")
        if not isOk:
            break
        bayNumber = newBayNumber + 1
        if newAvalHeight == 0:
            avalHeight = BAY_AVAL_HEIGHT
        else:
            avalHeight = newAvalHeight

    print(bayNumber, remainProducts)
    print("Total time: ", time.time() - allBeginTime)



if __name__ == '__main__':
    main()

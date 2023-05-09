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


def group_products(model, products, maxH, topGap):
    grouped_prods = []
    for i in range(len(products)):
        grouped_prods.append(group_product(model, i, maxH, products, topGap))

    return grouped_prods


def group_product(model, i, maxH, products, topGap):
    [_, q, l, w, h] = products[i]
    # print(q, l, w, h)
    [nL, nH, nW] = model.integer_var_list(3, 1, q, f"numberProds{i}")
    [rL, rH, rW] = rotate(model, [l, w, h], i)

    model.add(nL * rL + SHELVE_INTER_GAP + SHELVE_LEFT_GAP <= BAY_WIDTH)
    model.add(nH * rH + topGap <= maxH)

    model.add(nW * rW <= BAY_DEPTH)
    model.add((((nW + 1) * rW > BAY_DEPTH) | ((nH == 1) & (nL == 1) & (nW == q))))

    model.add(nL * nH * nW >= q)
    model.add((nL * nH * nW) * 3 <= q * 5)

    maxDim = q * max(products[i][2:]) + 10  # todo
    minDim = min(products[i][2:])  # todo
    # print(products[i], maxDim)
    [gL, gH, gW] = model.integer_var_list(3, minDim, maxDim, f"grouped{i}")

    model.add(gL == nL * rL + SHELVE_INTER_GAP)
    model.add(gH == nH * rH)
    model.add(gW == nW * rW)

    return [gL, gH, gW]


def getMaxH(model, avalHeight):
    minHDomain = SPACE_BETWEEN_SHELVES
    if (avalHeight == BAY_AVAL_HEIGHT):
        minHDomain = BAY_AVAL_HEIGHT - BAY_HEIGHT
    return model.integer_var(name="maxH", domain=range(minHDomain, avalHeight + 1, SPACE_BETWEEN_SHELVES))


# def boshKnapsack(chosen, groupedProducts, maxL):


def splitChosen(solution, products, grouped_prods, chosen):
    chosenProducts = []
    remainProducts = []
    for i in range(len(chosen)):
        if solution.get_value(chosen[i]):
            chosenProducts.append([solution.get_value(v) for v in grouped_prods[i]])
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
    grouped_prods = group_products(model, products, maxH, topGap)

    chosen = model.binary_var_list(size, "chosen")
    maxL = model.integer_var(0, BAY_WIDTH - SHELVE_LEFT_GAP)

    model.add(model.scal_prod([gL for [gL, _, _] in grouped_prods], chosen) == maxL)

    # objective = model.count_different(baysIds)
    # objective = model.sum(maxHs) #+ model.count_different(baysIds )*10
    # objective = model.max(baysIds)
    model.maximize(maxL-maxH*10)
    #model.minimize(maxH)

    model.add(model.search_phase([maxH]))
    # model.add(model.search_phase(weights))
    # model.add(model.search_phase(packIds))
    # model.add(model.search_phase(baysIds))
    # model.add(model.search_phase(e1))
    #  for g in grouped_prods:
    #      model.add(model.search_phase(g))
    # model.search_phase(chosenvarchooser=model.select_smallest(),
    #                    valuechooser=model.select_largest())

    # model.add(model.pack(loads, packIds, weights, nonZero))
    # model.add(model.minimize(nonZero))

    starting_point = model.create_empty_solution()

    for c in chosen:
        starting_point.add_integer_var_solution(c, 1)
    #starting_point.add_integer_var_solution(maxL, 1190)
    # for e in e2[:(len(e2))]:
    #     starting_point.add_integer_var_solution(e, 1)

    model.set_starting_point(starting_point)

    solution = model.solve(LogVerbosity='Quiet', TimeLimit=3)
    # solutions = model.start_search(SearchType='DepthFirst', Workers=1, LogVerbosity='Terse', TimeLimit=120)
    # 'DepthFirst', 'Restart', 'MultiPoint', 'IterativeDiving', 'Neighborhood', 'Auto')
    # solutions = model.start_search(TemporalRelaxation="Off", LogVerbosity='Terse', SearchType='Auto', Workers=8,
    # TimeLimit=600)
    # for solution in solutions:
    status = solution.get_solve_status()
    print([solution.get_value(maxH), solution.get_value(maxL), bayNumber])
    if status == "Feasible" or status == "Optimal":
        chosenProducts, remainProducts = splitChosen(solution, products, grouped_prods, chosen)
        if not remainProducts:
            return True, grouped_prods, remainProducts
        elif not chosenProducts:
            return next_bay(avalHeight, bayNumber, chosenProducts, products, remainProducts)
        else:
            return bosh_family(remainProducts, bayNumber, avalHeight - solution.get_value(maxH))
    else:
        return next_bay(avalHeight, bayNumber, [], products, products)
    # print([[solution.get_value(gL), solution.get_value(gH), solution.get_value(gW)] for [gL, gH, gW] in
    #        grouped_prods])
    # print([solution.get_value(c) for c in chosen])

    # solution.write()
    # if solution.isOptmal():
    #     model.end_search()()

    # if solution:
    #     solution.print_solution()


def next_bay(avalHeight, bayNumber, chosenProducts, products, remainProducts):
    if avalHeight == BAY_AVAL_HEIGHT:
        return False, chosenProducts, remainProducts
    else:
        # result, chosenProducts1, remainProducts1 =bosh_family(products, bayNumber + 1, BAY_AVAL_HEIGHT)
        return bosh_family(products, bayNumber + 1, BAY_AVAL_HEIGHT)


#    print([solution.get_value(e[i]) for i in range(2 * 2)])

def bosh(family):
    products = Fs[family]  # [:200]  # [[1, 2, 10, 620, 70], [1, 2, 10, 62, 70]]
    print(bosh_family(products, 1, BAY_AVAL_HEIGHT))


if __name__ == '__main__':
    bosh(1)

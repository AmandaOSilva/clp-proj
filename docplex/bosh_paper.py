import itertools
import argparse
import sys, os

PATH_DIRNAME = os.path.dirname(os.path.realpath(__file__))
import time
from operator import itemgetter
from itertools import groupby

import numpy as np
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
def rotate(model, prodDim, i):
    # print(prodDim)
    indexes = model.integer_var_list(3, 0, 2, f"indices_{i}")
    rotated = model.integer_var_list(3, name=f"rotated_{i}", domain=prodDim)
    model.add(model.all_diff(indexes))
    model.add([rotated[i] == model.element(prodDim, indexes[i]) for i in range(3)])

    return rotated


def group_products(model, products, maxH, avalHeight, topGap):
    groupedProds = []
    dropProds = []
    for i in range(len(products)):
        product = [_, q, l, w, h] = products[i]
        if min(product[2:]) > avalHeight - topGap or (
                q * l * h > (avalHeight - topGap) * (BAY_WIDTH - SHELVE_INTER_GAP - SHELVE_LEFT_GAP)):
            # print("droped: ", product, avalHeight, q*l*w*h, (avalHeight-topGap)*BAY_DEPTH*(BAY_WIDTH-SHELVE_INTER_GAP - SHELVE_LEFT_GAP))
            dropProds.append(product)
        else:
            groupedProds.append(group_product(model, product, i, maxH, topGap))

    return groupedProds, dropProds


def group_product(model, product, i, maxH, topGap):
    [_, q, l, w, h] = product
    # print(q, l, w, h)
    [nL, nH, nW] = model.integer_var_list(3, 1, q, f"numberProds{i}")
    [rL, rH, rW] = rotate(model, [l, w, h], i)

    model.add(nL * rL + SHELVE_INTER_GAP + SHELVE_LEFT_GAP <= BAY_WIDTH)
    model.add(nH * rH + topGap <= maxH)

    model.add(nW * rW <= BAY_DEPTH)
    model.add((((nW + 1) * rW > BAY_DEPTH) | ((nH == 1) & (nL == 1) & (nW == q))))

    # model.add((nL * nH * nW == q) | ((nL * nH * nW > q) & ((nL * nH * nW) * 4 < q * 6)))
    model.add(nL * nH * nW >= q)
    model.add((nL * nH * nW) * 31 <= q * 50)

    maxDim = q * max([l, w, h]) + 10  # todo
    minDim = min([l, w, h])  # todo
    # print(products[i], maxDim)
    [gL, gH, gW] = model.integer_var_list(3, minDim, maxDim, f"grouped{i}")

    model.add(gL == nL * rL + SHELVE_INTER_GAP)
    model.add(gH == nH * rH)
    model.add(gW == nW * rW)
#    return [gL,  gW, gH, rL, rW, rH]#, nL, nH, nW]
    return [gL, gH, gW, rL, rH, rW]


#    return [gL, gH, gW, rL, rH, rW][::-1]


def getMaxH(model, avalHeight):
    minHDomain = SPACE_BETWEEN_SHELVES
    if avalHeight == BAY_AVAL_HEIGHT:
        minHDomain = BAY_AVAL_HEIGHT - BAY_HEIGHT
    return model.integer_var(name="maxH", domain=range(minHDomain, avalHeight + 1, SPACE_BETWEEN_SHELVES))


# def boshKnapsack(chosen, groupedProducts, maxL):


def splitChosen(solution, products, groupedProds, dropProds, chosen):
    chosenProducts = []
    remainProducts = dropProds
    for i in range(len(chosen)):
        if 1- solution.get_value(chosen[i]):
            chosenProducts.append([solution.get_value(v) for v in groupedProds[i]] + products[i][1:2])
        else:
            remainProducts.append(products[i])
    return chosenProducts, remainProducts


def bosh_family(products, bayNumber, shelvedProducts, avalHeight):
    model = CpoModel()

    size = len(products)
    print(size, products[0][0], avalHeight)  # size, family number
    if avalHeight == BAY_AVAL_HEIGHT:
        topGap = SHELVE_TOP_GAP
    else:
        topGap = SHELVE_THICKNESS + SHELVE_TOP_GAP
    maxH = getMaxH(model, avalHeight)

    groupedProds, dropProds = group_products(model, products, maxH, avalHeight, topGap)
    groupedSize = len(groupedProds)

    if not groupedProds and dropProds:
        return next_bay(model, avalHeight, bayNumber, [], products, shelvedProducts, dropProds)

    remainL = model.integer_var(0, BAY_WIDTH - SHELVE_LEFT_GAP)
    maxL = (BAY_WIDTH - SHELVE_LEFT_GAP) - remainL

    chosen = model.binary_var_list(groupedSize, "chosen")
    # model.add(model.scal_prod([gL for [gL, _, _, _, _, _] in groupedProds], chosen) == maxL)
    #   model.add(model.scal_prod([gL for [_, _, _, _,_, gL] in groupedProds], chosen) == maxL)
    model.add(model.sum([groupedProds[i][0] * (1- chosen[i]) for i in range(groupedSize)]) == maxL)

    # objective = model.count_different(baysIds)
    # objective = model.sum(maxHs) #+ model.count_different(baysIds )*10
    # objective = model.max(baysIds)
    # model.maximize(maxL-maxH*10)
    # model.minimize(maxH)
    model.minimize(maxH + remainL)# + (model.max(chosen)-1)*10000000)
    # vs = model.integer_var_list(size, 1, 2, "vs")
    # for i in range(size):
    #     model.add((vs[i] == 1) - (chosen[i] == 1) != 0)
    # model.add(model.search_phase(vs+list(itertools.chain(*groupedProds))))
    #  for g in groupedProds:
    #      model.add(model.search_phase(g))

    # vars_search = list(itertools.chain(*[ [chosen[i]] + groupedProds[i][::-1] for i in range(groupedSize)]))
    # #
    # # #vars_search = list(itertools.chain(*[groupedProds[i] + [chosen[i]] for i in range(groupedSize)]))
    # #
    # # # #print([x.name for x in vars_search])
    # # # model.add(model.search_phase(vars_search))
    # # # model.add(model.search_phase(chosen))
    # #
    # model.add(model.search_phase(vars=vars_search, varchooser=model.select_smallest(model.var_index(vars_search)),
    #                              valuechooser=model.select_smallest(model.value())))
    # model.add(model.search_phase([remainL, maxH]))

    # vars_search = chosen+ list(itertools.chain(*groupedProds))
    # model.add(model.search_phase(vars=vars_search, varchooser=model.select_smallest(model.var_index(chosen)),
    #                              valuechooser=model.select_largest(model.value())))
    # model.search_phase(vars=chosen, varchooser=model.select_largest(model.var_index(chosen)),
    #                     valuechooser=model.select_largest(model.value()))

    # model.add(model.search_phase(list(itertools.chain(*groupedProds))))
    # model.add(model.pack(loads, packIds, weights, nonZero))
    # model.add(model.minimize(nonZero))

    # starting_point = model.create_empty_solution()
    # # #
    # for c in chosen:  # [:len(chosen) // 2]:#[::-1]:
    #     starting_point.add_integer_var_solution(c, 1)
    # # starting_point.add_integer_var_solution(maxL, 1190)
    # # for e in e2[:(len(e2))]:
    # #     starting_point.add_integer_var_solution(e, 1)
    #
    # model.set_starting_point(starting_point)
    # print(model.refine_conflict())
    solution = model.solve(LogVerbosity='Quiet', SearchType='Auto', Workers=8, TimeLimit=60)
    # solutions = model.start_search(SearchType='DepthFirst', Workers=1, LogVerbosity='Terse', TimeLimit=120)
    # 'DepthFirst', 'Restart', 'MultiPoint', 'IterativeDiving', 'Neighborhood', 'Auto')
    # solutions = model.start_search(TemporalRelaxation="Off", LogVerbosity='Terse', SearchType='Auto', Workers=8,
    # TimeLimit=600)
    # for solution in solutions:
    status = solution.get_solve_status()
    print("maxH: {}, remainL: {}, bay: {}".format(solution.get_value(maxH), solution.get_value(remainL), bayNumber))
    print([solution.get_value(c) for c in chosen])
    # print([solution.get_value(v) for v in vs])
    # print(sum([solution.get_value(groupedProds[i][0])* solution.get_value(chosen[i]) for i in range(size)]))
    if status == "Feasible" or status == "Optimal":
        chosenProducts, remainProducts = splitChosen(solution, products, groupedProds, dropProds, chosen)
        # print([[solution.get_value(gL), solution.get_value(gH), solution.get_value(gW)] for [gL, gH, gW] in
        # chosenProducts])

        if not remainProducts:
            return True, bayNumber, avalHeight, groupedProds, remainProducts
        elif not chosenProducts:
            return next_bay(model, avalHeight, bayNumber, chosenProducts, products, shelvedProducts, remainProducts)
        else:
            newAvalHeight = avalHeight - solution.get_value(maxH)
            newBayNumber = bayNumber
            shelvedProducts.append([bayNumber, products[0][0], newAvalHeight, chosenProducts])
            if newAvalHeight < topGap + SPACE_BETWEEN_SHELVES:
                newAvalHeight = BAY_AVAL_HEIGHT
                newBayNumber = bayNumber + 1

            model = None

            # [(N, NF, ShelveH) - CPs | CPsTail]
            return bosh_family(remainProducts, newBayNumber, shelvedProducts, newAvalHeight)
    else:
        print(status)
        return next_bay(model, avalHeight, bayNumber, [], products, shelvedProducts, products)

    # solution.write()
    # if solution.isOptmal():
    #     model.end_search()()

    # if solution:
    #     solution.print_solution()


def next_bay(model, avalHeight, bayNumber, chosenProducts, products, shelvedProducts, remainProducts):
    model = None
    if avalHeight == BAY_AVAL_HEIGHT:
        return False, bayNumber, avalHeight, chosenProducts, remainProducts
    else:
        # result, chosenProducts1, remainProducts1 =bosh_family(products, bayNumber + 1, BAY_AVAL_HEIGHT)
        return bosh_family(products, bayNumber + 1, shelvedProducts, BAY_AVAL_HEIGHT)


#    print([solution.get_value(e[i]) for i in range(2 * 2)])

def bosh(bayNumber, avalHeight, shelvedProducts, products):
    # print(Fs[family])
    # products = Fs[family]  # [::-1]  # [:200]  # [[1, 2, 10, 620, 70], [1, 2, 10, 62, 70]]
    print(products)
    return bosh_family(products, bayNumber, shelvedProducts, avalHeight)


# process_shelve([], [], [], []).
# process_shelve([(_, _, SH) - CPs | GPs], [(0, 0, ShPosH) | Ps], [(1200, 650, 40) | Sizes], ['"y"' | Colors]): -
# ShPosH is SH - 40,
# process_product(SH, 10, CPs, Pos, Size, Color),
# process_shelve(GPs, Ps1, Sizes1, Colors1),
# append(Pos, Ps1, Ps),
# append(Size, Sizes1, Sizes),
# append(Color, Colors1, Colors).
#
# same_bay((N1, _, _) - _, (N2, _, _) - _): - N2 = N1.
#
# process_bay([], [], [], []).
# process_bay([BayCPs | CPs], [Ps | PsTail], [Sizes | SizesTail], [Colors | ColorsTail]): -
# process_shelve(BayCPs, Ps, Sizes, Colors),
# process_bay(CPs, PsTail, SizesTail, ColorsTail).


# shelved_product_position(OH, OL, grouped(GL, GW, GH, RL, RW, RH), (PL,PW, PH)) :-
#     QL is (GL-10) // RL - 1, QW is GW // RW - 1, QH is GH // RH - 1,
#     NL in 0.. QL, NH in 0.. QH, NW in 0.. QW,
#     PL #= OL + NL * RL,
#     PW #= NW * RW,
#     PH #= OH + NH * RH,
#     labeling([], [PL,PH,PW]).
#
# process_product(_, _, [], [], [], []).
# process_product(OH, OL, [product(_F, Q, _L, _H, _W)-grouped(GL, GW, GH, RL, RW, RH)|SPs],
#             Ps, Sizes, Colors) :-
#     findall(P, shelved_product_position(OH, OL, grouped(GL, GW, GH, RL, RW, RH), P), Pos1),
#     length(Pos, Q),
#     append(Pos,_, Pos1), % TODO: rever processo de empacotamento ()
#     (foreach(_, Pos), foreach(Sz, Size), param([RL, RW, RH]) do Sz = (RL, RW, RH)),
#     (foreach(_, Pos), foreach(C, Color) do C = '"g"'),
#     OL1 is OL + GL,
#     process_product(OH, OL1, SPs, Ps1, Sizes1, Colors1),
#     append(Pos, Ps1, Ps),
#     append(Size, Sizes1, Sizes),
#     append(Color, Colors1, Colors).
#


def process_products(oH, oL, products):
    positions = []
    sizes = []
    colors = []
    # print("======================")
    for [gL, gH, gW, rL, rH, rW, q] in products:
        # print(oH, oL, [gL, gH, gW, rL, rH, rW, q])
        # lista = [(oL + l, oH + h, w) for w in range(rW, gW+1, rW) for h in range(rH, gH+1, rH) for l in range(rL, gL+1, rL)]

        lista = [(l, w, h) for l in
                 range(oL, oL + gL, rL) for h in range(oH, oH + gH, rH) for w in range(0, gW, rW)]
        print([gL, gH, gW, rL, rH, rW, q], lista)
        positions += lista[0:q]
        print(positions)
        sizes += [(rL, rW, rH)] * q
        colors += ["g"] * q
        oL += gL
    # print(positions,sizes, colors)
    return positions, sizes, colors


def process_shelve(shelve):
    print(shelve)
    positions = []
    sizes = []
    colors = []
    [_, _, sH, shelvedProducts] = shelve
    shPosH = sH - 40
    positions1, sizes1, colors1 = process_products(sH, 10, shelvedProducts)
    positions += [(0, 0, shPosH)] + positions1
    sizes += [(1200, 650, 40)] + sizes1
    colors += ["y"] + colors1
    return [positions, sizes, colors]


def process_bay(shelvedProducts):
    res = []
    positions = []
    sizes = []
    colors = []
    for bay in shelvedProducts:
        positions1 = []
        sizes1 = []
        colors1 = []
        for shelve in bay:
            p, s, c = process_shelve(shelve)
            positions1 += p
            sizes1 += s
            colors1 += c
        positions.append(positions1)
        sizes.append(sizes1)
        colors.append(colors1)

    # return [[process_shelve(shelve) for shelve in bay]  for bay in shelvedProducts]
    #
    #
    #
    return [positions, sizes, colors]


def export(shelvedProducts):
    print(shelvedProducts)
    [print("bay =", k, "shelves: ", [x for x in z], "\n") for k, z in groupby(shelvedProducts, key=itemgetter(0))]
    shelvedProductsByBay = [[x for x in z] for k, z in groupby(shelvedProducts, key=itemgetter(0))]
    print(shelvedProductsByBay)
    res = process_bay(shelvedProductsByBay)
    print(res)
    # print(res[0][0])
    # print(res[0][1])
    # print(res[0][2])
    with open(PATH_DIRNAME + '/../visualizer/output/bosh_result.py', 'w') as f:
        f.write("RES = " + str(res))


def main(families, mustExport):
    bayNumber = 1
    avalHeight = BAY_AVAL_HEIGHT
    stats = []
    shelvedProducts = []
    #    for products in Fs[65:67]: # [0:2]:  #
    for products in families:  # [1:2]:  #
        start_time = time.time()
        isOk, newBayNumber, newAvalHeight, chosenProducts, remainProducts = bosh(bayNumber, avalHeight, shelvedProducts,
                                                                                 products[::-1])
        exec_time = time.time() - start_time
        stats.append([products[0][0], len(products), round(exec_time, 2), newBayNumber, len(shelvedProducts)])
        print(shelvedProducts)
        if not isOk:
            break
        if newAvalHeight <= SPACE_BETWEEN_SHELVES:
            avalHeight = BAY_AVAL_HEIGHT
            bayNumber = newBayNumber + 1
        else:
            avalHeight = newAvalHeight
            bayNumber = newBayNumber
    if mustExport:
        export(shelvedProducts)
    print(stats)

    with open(PATH_DIRNAME + '/output/bosh_result_docplex.py', 'w') as f:
        f.write("MsDocPlex = " + str(stats))

    # print(bayNumber, remainProducts)


if __name__ == '__main__':
    sys.argv
    ap = argparse.ArgumentParser()
    ap.add_argument("-a", "--all", required=False, action='store_true',
                    help="shelve all families products")
    ap.add_argument("-f", "--family", required=False,
                    help="shelve family number (1-67)")
    ap.add_argument("-e", "--export", required=False, action='store_true',
                    help="export results for visualization")
    args = ap.parse_args()

    if args.family:
        try:
            family = int(args.family)
        except:
            ap.print_usage()
            exit()
        main([Fs[family - 1]], args.export)
    elif args.all:
        main(Fs, args.export)
    else:
        ap.print_usage()
        exit()

    # export([[1, 1, 2400, [[730, 449, 466, 360, 449, 233, 3]]],
    #         [1, 1, 2000, [[   312, 200, 640, 302, 200, 320, 2], [217, 300, 290, 207, 300, 145, 2]]],
    #         [2, 1, 2400, [[730, 449, 466, 360, 449, 233, 3]]], ])

#  x = [[[1, 1, 2400, [[730, 449, 466, 360, 449, 233, 3]]], [1, 1, 2300, [[312, 20, 640, 302, 20, 320, 2], [217, 30, 290, 207, 30, 145, 2]]]], [[2, 1, 2400, [[730, 449, 466, 360, 449, 233, 3]]]]]

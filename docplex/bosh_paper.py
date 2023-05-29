import itertools
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
        # if min(product[2:]) > avalHeight - topGap or (
        #         q* l * w * h > (avalHeight - topGap) * BAY_DEPTH * (BAY_WIDTH - SHELVE_INTER_GAP - SHELVE_LEFT_GAP)):
        #     # print("droped: ", product, avalHeight, q*l*w*h, (avalHeight-topGap)*BAY_DEPTH*(BAY_WIDTH-SHELVE_INTER_GAP - SHELVE_LEFT_GAP))
        #     dropProds.append(product)
        # else:
        groupedProds.append(group_product(model, product, chosen, i, maxH, topGap))

    return groupedProds, dropProds


def group_product(model, product, chosen, i, maxH, topGap):
    [_, q, l, w, h] = product
    # print(q, l, w, h)
    [nL, nH, nW] = model.integer_var_list(3, 1, q, f"numberProds{i}")
    [rL, rH, rW] = rotate(model, [l, w, h], i)

    model.add(nL * rL + SHELVE_INTER_GAP + SHELVE_LEFT_GAP <= BAY_WIDTH)
    model.add(chosen[i]* nH * rH + topGap <= maxH)

    model.add(nW * rW <= BAY_DEPTH)
    model.add((((nW + 1) * rW > BAY_DEPTH) | ((nH == 1) & (nL == 1) & (nW == q))))

    model.add(nL * nH * nW >= q)
    model.add((nL * nH * nW) * 3 <= q * 5)

    maxDim = q * max([l, w, h]) + 10  # todo
    minDim = min([l, w, h])  # todo
    # print(products[i], maxDim)
    [gL, gH, gW] = model.integer_var_list(3, minDim, maxDim, f"grouped{i}")

    model.add(gL == nL * rL + SHELVE_INTER_GAP)
    model.add(gH == nH * rH)
    model.add(gW == nW * rW)
    return [gL, gH, gW][::-1]
#    return [gL, gH, gW, rL, rH, rW][::-1]


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


def bosh_family(products, bayNumber, shelvedProducts, avalHeight):
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

    remainL = model.integer_var(0, BAY_WIDTH - SHELVE_LEFT_GAP - 1)
    maxL = (BAY_WIDTH - SHELVE_LEFT_GAP) - remainL

    model.add(model.scal_prod([gL for [_, _, gL] in groupedProds], chosen) == maxL)
 #   model.add(model.scal_prod([gL for [_, _, _, _,_, gL] in groupedProds], chosen) == maxL)
    #model.add(sum([groupedProds[i][-1] * chosen[i] for i in range(groupedSize)]) == maxL)

    # objective = model.count_different(baysIds)
    # objective = model.sum(maxHs) #+ model.count_different(baysIds )*10
    # objective = model.max(baysIds)
    # model.maximize(maxL-maxH*10)
    # model.minimize(maxH)
    model.minimize(maxH+remainL*10)
    # vs = model.integer_var_list(size, 1, 2, "vs")
    # for i in range(size):
    #     model.add((vs[i] == 1) - (chosen[i] == 1) != 0)
    # model.add(model.search_phase(vs+list(itertools.chain(*groupedProds))))
    #  for g in groupedProds:
    #      model.add(model.search_phase(g))

    # vars_search = list(itertools.chain(*[[chosen[i]] + groupedProds[i][::-1] for i in range(groupedSize)]))

    # vars_search = list(itertools.chain(*[groupedProds[i] + [chosen[i]] for i in range(groupedSize)]))[::-1]

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
    # for c in chosen[:len(chosen) // 2][::-1]:
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
            if newAvalHeight < topGap + SPACE_BETWEEN_SHELVES:
                newAvalHeight = BAY_AVAL_HEIGHT
                newBayNumber = bayNumber + 1
            model = None

            # [(N, NF, ShelveH) - CPs | CPsTail]
            shelvedProducts.append([bayNumber, products[0][0], newAvalHeight, chosenProducts])
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
def export(shelvedProducts):
    print(shelvedProducts)
    res = [[x for x in z] for k,z in groupby(shelvedProducts, key=itemgetter(0))]
    print(res)

def main():
    bayNumber = 1
    avalHeight = BAY_AVAL_HEIGHT
    stats = []
    shelvedProducts = []
    #    for products in Fs[65:67]: # [0:2]:  #
    for products in Fs[0:2]:  #
        start_time = time.time()
        isOk, newBayNumber, newAvalHeight, chosenProducts, remainProducts = bosh(bayNumber, avalHeight, shelvedProducts,
                                                                                 products)
        exec_time = time.time() - start_time
        stats.append([products[0][0], len(products), round(exec_time, 2), newBayNumber, len(shelvedProducts)])
        print(shelvedProducts)
        if not isOk:
            break
        if newAvalHeight <= 50:
            avalHeight = BAY_AVAL_HEIGHT
            bayNumber = newBayNumber + 1
        else:
            avalHeight = newAvalHeight
            bayNumber = newBayNumber
    export(shelvedProducts)
    print(stats)

    with open('output/bosh_result_docplex.py', 'w') as f:
        f.write("MsDocPlex = " + str(stats))

    #print(bayNumber, remainProducts)


if __name__ == '__main__':

#    export([[1, 1, 2400, [[380, 454, 185], [278, 198, 570], [269, 275, 412], [358, 198, 400], [321, 76, 618], [236, 315, 180], [72, 185, 608], [398, 41, 470], [245, 258, 79], [83, 100, 580], [30, 302, 640], [49, 152, 534], [74, 156, 310], [40, 145, 414], [139, 43, 315], [54, 128, 303], [66, 98, 270], [92, 106, 132], [137, 61, 136], [56, 72, 285], [73, 29, 500], [50, 107, 157], [171, 67, 62], [80, 96, 98], [71, 155, 62], [42, 49, 254], [45, 58, 116], [57, 44, 113], [113, 53, 41], [25, 85, 94], [108, 118, 10], [25, 28, 76]]], [1, 1, 1950, [[634, 241, 500], [722, 172, 627], [704, 198, 260], [418, 285, 40]]], [1, 1, 1500, [[1153, 388, 355], [318, 174, 604], [172, 123, 540], [181, 28, 399], [70, 94, 220]]], [1, 1, 1150, [[1080, 158, 459], [426, 199, 620], [260, 241, 528], [197, 174, 576], [415, 98, 374], [129, 182, 501]]], [1, 2, 3000, [[790, 782, 481], [174, 471, 520], [314, 206, 440], [258, 282, 442], [352, 153, 489], [194, 199, 421], [331, 157, 295], [251, 144, 402], [99, 414, 466], [395, 95, 316], [296, 88, 341], [435, 116, 159], [115, 194, 357], [135, 100, 544], [62, 302, 386], [115, 106, 440], [44, 380, 494], [144, 120, 252], [426, 19, 628], [39, 172, 516], [106, 116, 172], [124, 61, 245], [207, 99, 52], [65, 197, 92], [149, 49, 136], [66, 68, 228], [99, 54, 159], [61, 93, 122], [46, 33, 268], [41, 14, 234], [24, 61, 80], [49, 14, 67]]], [2, 2, 1650, [[754, 380, 577], [555, 752, 369], [625, 288, 584], [341, 452, 630], [444, 210, 618], [722, 114, 344], [193, 364, 618], [218, 210, 596], [303, 386, 70], [134, 170, 270], [138, 238, 165], [176, 161, 153], [92, 124, 309], [100, 69, 350], [88, 154, 146], [57, 71, 524], [64, 54, 335], [82, 121, 111], [50, 82, 261], [51, 40, 507], [93, 81, 118], [144, 28, 198], [100, 90, 68]]], [2, 2, 1350, [[426, 149, 519], [231, 189, 608], [312, 156, 470], [306, 110, 576], [378, 54, 552], [206, 86, 573], [177, 98, 232], [303, 127, 99], [104, 96, 249], [133, 122, 149], [264, 29, 219], [103, 67, 201], [52, 90, 306], [39, 210, 155], [62, 55, 210], [43, 132, 132], [111, 77, 32], [77, 30, 92]]], [2, 2, 850, [[1182, 388, 371], [174, 248, 180], [97, 125, 504], [157, 95, 294], [166, 76, 528], [139, 121, 184], [360, 28, 282], [90, 78, 224], [119, 79, 162], [86, 108, 166], [88, 11, 130]]], [2, 2, 700, [[242, 61, 360], [263, 80, 197], [87, 78, 594], [121, 81, 395], [92, 83, 510], [101, 90, 320], [100, 94, 284], [151, 92, 185], [79, 70, 495], [95, 40, 572], [121, 61, 236], [93, 78, 228], [81, 56, 270], [113, 49, 202], [75, 31, 460], [58, 49, 369], [65, 72, 212], [64, 54, 268], [121, 71, 98], [69, 70, 122], [76, 35, 200], [65, 71, 114], [121, 19, 195], [77, 72, 85], [77, 34, 147], [49, 67, 104], [77, 56, 54]]], [2, 2, 550, [[156, 80, 510], [474, 29, 416], [188, 87, 336], [186, 36, 648], [216, 34, 546], [88, 80, 444], [115, 93, 280], [160, 57, 305], [148, 48, 342], [155, 41, 332], [181, 76, 150], [133, 92, 141], [110, 25, 615], [92, 40, 531], [71, 92, 240], [65, 68, 208], [69, 78, 160], [65, 82, 114], [63, 71, 106], [105, 82, 48], [51, 41, 162], [67, 57, 69], [121, 55, 25], [30, 61, 114]]], [2, 2, 200, [[344, 230, 456], [238, 268, 340], [306, 144, 526], [176, 195, 414], [353, 176, 180], [138, 208, 528], [259, 182, 177], [110, 197, 288], [123, 110, 320], [34, 159, 250], [43, 34, 546], [123, 52, 54], [77, 114, 41]]], [2, 2, 3000, [[907, 124, 602], [765, 101, 443], [79, 70, 580], [121, 114, 198], [58, 96, 582], [68, 58, 592], [91, 99, 240], [80, 68, 258], [47, 38, 500], [44, 35, 429], [69, 24, 175]]], [3, 2, 2400, [[468, 452, 344], [396, 407, 408], [495, 97, 571], [373, 192, 294], [386, 258, 184], [201, 197, 357], [406, 38, 567], [223, 104, 297], [200, 89, 261], [40, 162, 480], [99, 88, 212], [97, 112, 156]]], [3, 2, 2200, [[845, 120, 490], [422, 81, 482], [218, 107, 418], [144, 134, 390], [145, 104, 399], [124, 115, 264], [157, 77, 246], [106, 97, 226], [105, 77, 98]]], [3, 2, 3000, [[966, 1974, 455], [183, 806, 385], [550, 182, 590], [301, 403, 340], [211, 359, 522], [314, 154, 620], [258, 175, 186], [134, 126, 492], [182, 115, 342], [122, 119, 336], [74, 22, 90]]], [4, 2, 1500, [[505, 1250, 493], [472, 409, 532], [302, 412, 628], [223, 382, 412], [142, 496, 646], [189, 346, 604], [126, 304, 564], [215, 205, 332], [151, 268, 516], [186, 171, 428], [163, 197, 614], [276, 302, 152], [168, 263, 249], [260, 308, 129], [187, 100, 543], [117, 244, 360], [199, 101, 305], [89, 160, 440], [183, 113, 238], [156, 143, 197], [151, 149, 188], [134, 134, 228], [46, 127, 585], [87, 103, 304], [144, 129, 138], [118, 113, 116], [127, 72, 99]]], [4, 2, 1250, [[296, 99, 244], [183, 178, 152], [159, 192, 162], [139, 126, 258], [124, 101, 312], [145, 125, 192], [214, 148, 94], [174, 159, 88], [105, 102, 204], [247, 185, 28], [142, 154, 39], [140, 138, 24]]], [4, 2, 600, [[590, 319, 329], [225, 446, 576], [499, 338, 371], [398, 211, 597], [243, 302, 462], [394, 146, 434], [291, 162, 604], [190, 182, 314], [132, 149, 258]]], [4, 2, 3000, [[750, 292, 584], [836, 292, 512], [481, 158, 376], [138, 405, 628]]], [5, 2, 2250, [[302, 692, 348], [356, 499, 356], [310, 249, 634], [264, 329, 448], [256, 243, 608], [181, 393, 396], [130, 307, 394], [144, 266, 337], [125, 369, 238], [72, 289, 558], [198, 81, 576], [215, 220, 183]]], [5, 2, 1950, [[532, 184, 542], [630, 194, 437], [488, 125, 510], [512, 80, 478], [213, 164, 518]]], [5, 2, 1800, [[469, 49, 486]]], [5, 3, 3000, [[271, 765, 477], [202, 1040, 421], [655, 312, 518], [518, 247, 514], [919, 172, 336], [110, 1540, 331], [640, 154, 596], [479, 237, 218], [264, 180, 522], [174, 166, 615], [153, 286, 591], [173, 162, 594], [288, 133, 406], [156, 226, 516], [188, 207, 516], [82, 344, 465], [508, 50, 482], [114, 238, 616], [250, 117, 495], [257, 190, 182], [49, 555, 558], [130, 129, 518], [212, 100, 567], [131, 173, 357], [123, 167, 330], [188, 190, 173], [84, 309, 250], [154, 241, 152], [61, 147, 612], [163, 199, 150], [79, 189, 312], [882, 12, 612], [236, 60, 266], [298, 49, 245], [123, 73, 390], [186, 86, 184], [316, 16, 648], [82, 209, 140], [115, 84, 228], [95, 74, 292], [90, 59, 494], [212, 29, 138], [91, 117, 77], [35, 150, 150], [19, 315, 177], [62, 24, 534], [43, 182, 70], [35, 117, 125], [157, 10, 188], [70, 83, 38], [43, 34, 99], [78, 103, 14], [30, 98, 49]]], [6, 3, 2400, [[554, 262, 371], [384, 283, 472], [253, 296, 464], [308, 233, 544], [108, 419, 515], [256, 353, 240], [127, 170, 600], [78, 302, 506], [174, 84, 627], [333, 180, 50], [133, 141, 146], [66, 112, 538], [112, 49, 388], [28, 156, 582], [248, 31, 136], [323, 12, 209], [86, 72, 120], [109, 19, 340], [29, 82, 57]]], [6, 3, 300, [[1152, 381, 519], [464, 574, 516], [137, 1948, 369], [919, 195, 596], [584, 350, 492], [1006, 170, 540], [386, 188, 650], [354, 183, 507], [426, 431, 124], [322, 156, 636], [262, 226, 452], [59, 1022, 365], [236, 166, 537], [356, 250, 150], [175, 157, 468], [250, 257, 174], [172, 162, 320], [126, 180, 354], [151, 152, 340], [235, 127, 246], [160, 291, 160], [211, 67, 596], [95, 172, 642], [184, 110, 171], [137, 83, 250], [83, 76, 378], [126, 118, 118], [55, 123, 288], [19, 290, 579], [60, 48, 508], [48, 184, 36], [45, 100, 36]]], [6, 3, 3000, [[606, 134, 621], [1046, 100, 327], [210, 85, 430], [180, 89, 483], [93, 96, 304], [194, 29, 392], [41, 92, 310]]], [7, 3, 2400, [[615, 575, 384], [534, 509, 486], [284, 137, 388], [254, 154, 161], [111, 95, 210]]], [7, 3, 1950, [[778, 310, 412], [568, 184, 576], [323, 384, 316], [179, 236, 576], [54, 250, 510], [76, 132, 486]]], [7, 3, 1500, [[474, 315, 532], [663, 304, 452], [496, 239, 354], [189, 202, 516], [916, 21, 624], [70, 124, 646]]]])
    main()

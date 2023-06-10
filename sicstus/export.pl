shelved_product_position(OH, OL, grouped(GL, GW, GH, RL, RW, RH), (PL,PW, PH)) :-
    QL is (GL-10) // RL - 1, QW is GW // RW - 1, QH is GH // RH - 1, 
    NL in 0.. QL, NH in 0.. QH, NW in 0.. QW, 
    PL #= OL + NL * RL,
    PW #= NW * RW,
    PH #= OH + NH * RH,
    labeling([], [PL,PH,PW]).

process_product(_, _, [], [], [], []).
process_product(OH, OL, [product(_F, Q, _L, _H, _W)-grouped(GL, GW, GH, RL, RW, RH)|SPs], 
            Ps, Sizes, Colors) :-
    findall(P, shelved_product_position(OH, OL, grouped(GL, GW, GH, RL, RW, RH), P), Pos1),    
    length(Pos, Q),
    append(Pos,_, Pos1), % TODO: rever processo de empacotamento ()
    (foreach(_, Pos), foreach(Sz, Size), param([RL, RW, RH]) do Sz = (RL, RW, RH)),
    (foreach(_, Pos), foreach(C, Color) do C = '"g"'),
    OL1 is OL + GL,
    process_product(OH, OL1, SPs, Ps1, Sizes1, Colors1),
    append(Pos, Ps1, Ps),
    append(Size, Sizes1, Sizes),
    append(Color, Colors1, Colors).


process_shelves([], [], [], []).
process_shelves([(_, _, SH)-CPs|GPs], [(0, 0, ShPosH)|Ps], [(SL, SW, THICK)|Sizes], ['"y"'|Colors]) :-
    bay(SL, _, SW, _),
    shelf(THICK, _TG, LG, _IG, _RG),
    ShPosH is SH - THICK ,
    process_product(SH, LG, CPs, Pos, Size, Color),
    process_shelves(GPs, Ps1, Sizes1, Colors1),
    append(Pos, Ps1, Ps),
    append(Size, Sizes1, Sizes),
    append(Color, Colors1, Colors).
    
same_bay((N1,_, _)-_, (N2,_, _)-_) :- N2 = N1.

process_bays([], [], [], []).
process_bays([BayCPs|CPs], [Ps|PsTail], [Sizes|SizesTail], [Colors|ColorsTail]) :-
    bay(SL, SH, SW, AvalH),
    process_shelves(BayCPs, Ps1, Sizes1, Colors1),
    append([(0,0,AvalH),(-20, 0, 0), (SL, 0, 0)], Ps1, Ps),
    append([(SL, SW, 40), (20, SW, SH), (20, SW, SH)], Sizes1, Sizes),
    append(['"k"', '"grey"', '"grey"'], Colors1, Colors),
    process_bays(CPs, PsTail, SizesTail, ColorsTail).


go_vis(SearchOptions, N, Res) :-  
    fd_statistics, reset_timer, 
    families_sorted(Fs), print_time('Pre processing'), nth1(N, Fs, F), 
    bosh(SearchOptions, [F], Res), !, Res = res(GPs, _), 
    nl, length(GPs, L), print([L]), fd_statistics.%, statistics.
go_export(N) :-
    go_export([3, 1], N).
go_export(SearchOptions, N) :-
    go_vis(SearchOptions, N,res(CPs, _DPs)), !,
    group(same_bay, CPs, CPsByBay),
    process_bays(CPsByBay, Ps, Sizes, Colors),
    Res = [Ps, Sizes, Colors],
    write_matrix_in_file('../visualizer/output/bosh_result.py', 'RES', Res).

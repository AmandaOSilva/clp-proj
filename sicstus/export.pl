:- consult('bosh_paper.pl').



goVis(N, Res) :-  fd_statistics, reset_timer, families_sorted(Fs), print_time('Pre processing'), nth1(N, Fs, F), bosh([F], Res), !, print_time(Res), Res = res(NBays, GPs, _), nl, length(GPs, L) , print([L, NBays]), fd_statistics.%, statistics.


shelved_product_position(OH, OL, grouped(GL, GW, GH, RL, RW, RH), (PL,PW, PH)) :-
    QL is (GL-10) // RL - 1, QW is GW // RW - 1, QH is GH // RH - 1, 
    NL in 0.. QL, NH in 0.. QH, NW in 0.. QW, 
    PL #= OL + NL * RL,
    PW #= NW * RW,
    PH #= OH + NH * RH,
    labeling([], [PL,PH,PW]).

process_shelved_products(_, _, [], [], [], []).
process_shelved_products(OH, OL, [product(_F, Q, _L, _H, _W)-grouped(GL, GW, GH, RL, RW, RH)|SPs], 
            Ps, Sizes, Colors) :-
    findall(P, shelved_product_position(OH, OL, grouped(GL, GW, GH, RL, RW, RH), P), Pos1),    
    length(Pos, Q),
    append(Pos,_, Pos1), % TODO: processo de empacotamento precisa ser revisto
    (foreach(_, Pos), foreach(Sz, Size), param([RL, RW, RH]) do Sz = (RL, RW, RH)),
    (foreach(_, Pos), foreach(C, Color) do C = '"green"'),
    OL1 is OL + GL,
    process_shelved_products(OH, OL1, SPs, Ps1, Sizes1, Colors1),
    append(Pos, Ps1, Ps),
    append(Size, Sizes1, Sizes),
    append(Color, Colors1, Colors).


process_shelve([], [], [], []).
process_shelve([SH-CPs|GPs], [(0, 0, ShPosH)|Ps], [(1200, 650, 40)|Sizes], ['"yellow"'|Colors]) :-
    ShPosH = SH - 40 ,
    process_shelved_products(SH, 0, CPs, Pos, Size, Color),
    process_shelve(GPs, Ps1, Sizes1, Colors1),
    append(Pos, Ps1, Ps),
    append(Size, Sizes1, Sizes),
    append(Color, Colors1, Colors).
    

exporter(N) :-
    goVis(N,res(_NBays, GPs, _DPs)), !,
    process_shelve(GPs, Ps, Sizes, Colors),
    Res = [Ps, Sizes, Colors],
    write_matrix_in_file('../visualizer/bosh_result.py', 'RES', Res).




%-------------------------------------------------------------------------
%  Unit tests
%-------------------------------------------------------------------------
/*
:- use_module(library(plunit)).

:- begin_tests(visu).

test(shelved-product-position0) :-
    shelved-product-position(0, 0, grouped(70, 60, 30, 10, 20, 30), (0,0,0)). 

test(shelved-product-position0) :-
    shelved-product-position(0, 0, grouped(70, 60, 30, 10, 20, 30), (20,20,30)). 

test(shelved-product-position-all) :-
    findall(Pos, shelved-product-position(0, 0, grouped(40, 40, 30, 10, 20, 30), Pos), _AllPos). 

:- end_tests(visu).    
*/
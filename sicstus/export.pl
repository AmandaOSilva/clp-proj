:- consult('bosh_paper.pl').



%print_time(Res), 
goVis(N, Res) :-  
    fd_statistics, reset_timer, 
    families_sorted(Fs), print_time('Pre processing'), nth1(N, Fs, F), 
    bosh([F], Res), !, Res = res(GPs, _), 
    nl, length(GPs, L), print([L]), fd_statistics.%, statistics.


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


process_shelve([], [], [], []).
process_shelve([(_, _, SH)-CPs|GPs], [(0, 0, ShPosH)|Ps], [(1200, 650, 40)|Sizes], ['"y"'|Colors]) :-
    ShPosH is SH - 40 ,
    process_product(SH, 10, CPs, Pos, Size, Color),
    process_shelve(GPs, Ps1, Sizes1, Colors1),
    append(Pos, Ps1, Ps),
    append(Size, Sizes1, Sizes),
    append(Color, Colors1, Colors).
    
same_bay((N1,_, _)-_, (N2,_, _)-_) :- N2 = N1.

process_bay([], [], [], []).
process_bay([BayCPs|CPs], [Ps|PsTail], [Sizes|SizesTail], [Colors|ColorsTail]) :-
    process_shelve(BayCPs, Ps, Sizes, Colors),
    process_bay(CPs, PsTail, SizesTail, ColorsTail).

exporter(N) :-
    goVis(N,res(CPs, _DPs)), !,
    group(same_bay, CPs, CPsByBay),
    process_bay(CPsByBay, Ps, Sizes, Colors),
    Res = [Ps, Sizes, Colors],
    write_matrix_in_file('../visualizer/output/bosh_result.py', 'RES', Res).


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
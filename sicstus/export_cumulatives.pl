%:- consult('bosh_paper.pl').
:- consult('bosh_cumulatives.pl').



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
process_product(OH, OL, [(product(_F, Q, _L, _H, _W)-grouped(GL, GW, GH, RL, RW, RH))|SPs], 
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


process_shelve([], [], _, [], []).
process_shelve([(_, _, S)-CPs|GPs], [(0, 0, ShPosH)|Ps], AvalH, [(1200, 650, 40)|Sizes], ['"y"'|Colors]) :-
    SH is AvalH - S,
    ShPosH is SH - 40,
    process_product(SH, 10, CPs, Pos, Size, Color),
    process_shelve(GPs, Ps1, SH, Sizes1, Colors1),
    append(Pos, Ps1, Ps),
    append(Size, Sizes1, Sizes),
    append(Color, Colors1, Colors).
    
same_bay((B1, _, _)-_, (B2, _, _)-_) :- B2 = B1.
%same_shelve((_, S1, _)-_, (_, S2, _)-_) :- S2 = S1.

process_bay([], [], [], []).
process_bay([BayCPs|CPs], [Ps|PsTail], [Sizes|SizesTail], [Colors|ColorsTail]) :-
    process_shelve(BayCPs, Ps, 3000, Sizes, Colors),
    process_bay(CPs, PsTail, SizesTail, ColorsTail).



by_bay_shelve((B1, S1, H1)-_, (B2, S2, H2)-_) :- 
    B2 * 100 + S2 > B1 * 100 + S1
    ; B1 = B2, S1 = S2, H2 < H1.

%group_same_shelve([], []) :- !.
%group_same_shelve([BS1-CP1, BS2-CP2|CPs], CPsByShelve) :- 

%group_same_shelve([CPs, CPsByShelve) :- 


group_pairs_by_key([], []).
group_pairs_by_key([K-V|KVs0], [K-[V|Vs]|KVs]) :-
        same_key(K, KVs0, Vs, KVs1),
        group_pairs_by_key(KVs1, KVs).

same_key(K0, [K1-V|KVs0], [V|Vs], KVs) :-
        K0 == K1, !,
        same_key(K0, KVs0, Vs, KVs).
same_key(_, KVs, [], KVs).

exporter(N) :-
    goVis(N,res(CPs, _DPs)),
    samsort(by_bay_shelve, CPs, CPsSorted),
    group_pairs_by_key(CPsSorted, CPsByShelve),
    nl,nl,
    %print(CPsByShelve),
    nl,nl,    
    %group(same_shelve, CPsSorted, CPsByShelve1),
    %group_same_shelve(CPsByShelve1, CPsByShelve),
    %group(same_bay, CPsByShelve, CPsByBay),
    group(same_bay, CPsByShelve, CPsByBay),
    process_bay(CPsByBay, Ps, Sizes, Colors),
    Res = [Ps, Sizes, Colors],
    %print(Res),
    write_matrix_in_file('../visualizer/output/bosh_result.py', 'RES', Res).



% families_sorted(Fs),    exporter_stats(1, 3, Fs).
exporter_stats :-
    families_sorted(Fs),
    length(Fs, L),
    exporter_stats(1, L, Fs).

exporter_stats(NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    %length(FsFull, L), LPos = L - NF, 
    length(Fs, N), append(Fs,_, Ts),
    ( foreach(F, Fs),
      foreach(M, Ms) do
        %F = [product(N, _, _, _, _)|_],
        length(F, L),
        reset_timer,
        bosh([F], Res),
        get_time(T),
        %Res = res(CPs, UsedBays, []),
        Res = res(CPs, []),
        last(CPs, (UsedBays, N, _)-_),
        length(CPs, UsedShelves),
        M = [N, L, T, UsedBays, UsedShelves]
    ),
    write_matrix_in_file('../sicstus/output/result.py', 'Ms', Ms).



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

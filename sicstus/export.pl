:- consult('bosh_paper.pl').



%print_time(Res), 
goVis([VarsSelectionOption, LabelingOption], N, Res) :-  
    fd_statistics, reset_timer, 
    families_sorted(Fs), print_time('Pre processing'), nth1(N, Fs, F), 
    bosh([VarsSelectionOption, LabelingOption], [F], Res), !, Res = res(GPs, _), 
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

go_export([VarsSelectionOption, LabelingOption], N) :-
    goVis([VarsSelectionOption, LabelingOption], N,res(CPs, _DPs)), !,
    group(same_bay, CPs, CPsByBay),
    process_bays(CPsByBay, Ps, Sizes, Colors),
    Res = [Ps, Sizes, Colors],
    write_matrix_in_file('../visualizer/output/bosh_result.py', 'RES', Res).


go_all_options(N) :-
    families_sorted(Fs),
    %nth1(N, Fs, F),
    %( foreach(VarsSelectionOption, [10,9]),
    ( foreach(VarsSelectionOption, [1,2,3,4,9,10]),
      foreach(R1s, Rs),
      param(N, Fs) do
%      ( foreach(LabelingOption, [1,3]),
      ( foreach(LabelingOption, [1,2,3,4,5,6,7,8]),
        foreach(R, R1s),
        param(N, Fs, VarsSelectionOption) do
            ( catch(go_all_options([VarsSelectionOption, LabelingOption], N, 1, Fs, Time, NBay),
                _Ex, [Time, NBay] = [0,0]), 
              R = [VarsSelectionOption, LabelingOption, N, Time, NBay]
            ; R = [VarsSelectionOption, LabelingOption, N, 0, 0]
            ), !            
      )
%      append(R1s, R1sFlat)
    ), print(Rs),
    write_matrix_in_file('../sicstus/output/result_all_options.py', 'Rs', Rs).

go_all_options([VarsSelectionOption, LabelingOption], NI, N, FsFull, Time, NBay) :- 
    print([VarsSelectionOption, LabelingOption, NI]), 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    %set_prolog_flag(gc, off),
    %statistics, 
    reset_timer,
    bosh([VarsSelectionOption, LabelingOption], Fs, Res), !,
    get_time(Time),
    %Res = res(CPs, UsedBays, []),
    Res = res(CPs, []),
    last(CPs, (NBay, _, _)-_).





% families_sorted(Fs),    go_timer_stats(1, 3, Fs).
go_timer_stats([VarsSelectionOption, LabelingOption]) :-
    families_sorted(Fs),
    length(Fs, L),
    go_timer_stats([VarsSelectionOption, LabelingOption], 1, L, Fs).

go_timer_stats([VarsSelectionOption, LabelingOption], NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    %length(FsFull, L), LPos = L - NF, 
    length(Fs, N), append(Fs,_, Ts),
    ( foreach(F, Fs),
      foreach(M, Ms) do
        %F = [product(N, _, _, _, _)|_],
        length(F, L),
        reset_timer,
        bosh([VarsSelectionOption, LabelingOption], [F], Res),
        get_time(T),
        %Res = res(CPs, UsedBays, []),
        Res = res(CPs, []),
        last(CPs, (UsedBays, N, _)-_),
        length(CPs, UsedShelves),
        M = [N, L, T, UsedBays, UsedShelves]
    ),
    write_matrix_in_file('../sicstus/output/result_stats.py', 'Ms', Ms).



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

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- consult('process_files.pl').

/*
For each product in turn:
   product family, quantity to be shelved, length, width, height

All orientations of each product are allowed and no product is allowed 
to overhang a shelf.

The two test problems have different bay files. 
These files are baytp1 and baytp2 and the format of these files is:

For each bay in turn:
   width, height, depth, available height
    1200   2400    650     3000
Bays must be shelved used in same sequence as given in each file.

Each bay can have a number of possible shelves.
These are given in the file shelves
The format of this file is:

For each shelf in turn:
    shelf number, thickness, position, top gap, left gap, inter gap, right gap
	37 				40 			1800 	15 			10 		10 			10 
*/

reset_timer:-
	statistics(walltime, _).
get_time(TS) :- 
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000.

print_time(Msg):-
	get_time(TS), nl, 
    write(Msg),
	write(TS),
	write('s'), nl.


% domain
%product(F, Q, L, H, W)

% bay(width, height, depth, available height)
bay(1200, 2400, 650, 3000).
max_available_height(H) :- bay(_,_,_, H).

% shelf(thickness, top gap, left gap, inter gap, right gap)
shelf(40, 15, 10, 10, 10).


% rotate(+ProductDimensions, -RotatedProductDimensions)
rotate([L, H, W], [RL, RH, RW]) :- 
    all_distinct([IL, IW, IH]),
    element(IL, [L,H,W], RL),
    element(IH, [L,H,W], RH),
    element(IW, [L,H,W], RW).

% group_product(+Product, +MaxH, +TopGap, -Variables, -GroupedProducts, -DropedProducts)
group_products([], _, _, [], [], []) :- !.
group_products([product(_F, Q, L, H, W)|Ps], MaxH, TopGap, [GL, GW, GH|Gs],
    [product(_F, Q, L, H, W)-grouped(GL, GW, GH, RL, RW, RH)|GPsTail], DPs) :-
    bay(SL, _, SW, _),
    shelf(_THICK, _TG, LG, IG, _RG),
    rotate([L, H, W], [RL, RH, RW]), 
	NL in 1..Q, NH in 1..Q, NW in 1..Q,		

    GL #= NL * RL + IG,
    GH #= NH * RH,
    GW #= NW * RW,

    GL + IG + LG #=< SL ,
   	GH + TopGap #=< MaxH,
	GW #=< SW, ((NW+1) * RW #> SW #\/ (NH #= 1 #/\ NL #= 1 #/\ NW #= Q)),

    NL * NH * NW #>= Q,
	(NL * NH * NW) * 31 #=< Q * 50,
    !, group_products(Ps, MaxH, TopGap, Gs, GPsTail, DPs).

group_products([P|Ps], MaxH, TopGap, Gs, GPs, [P|DPsTail]) :-
    group_products(Ps, MaxH, TopGap, Gs, GPs, DPsTail), !.

maxH_domain(AvalH, MaxH) :-
    bay(_, BH, _, BAH),
	X is BAH - AvalH,
    Diff = BAH-BH,
	( X < (Diff) ->
	  MinHDomain is Diff - X
	; MinHDomain is 50), % 50 = space between shelves
	numlist(MinHDomain, 50, AvalH, _, MaxHDomain),
	list_to_fdset(MaxHDomain, FDS_MaxHDomain),
	MaxH in_set FDS_MaxHDomain.

chosen_constraints([], [], [], 0).
chosen_constraints([GL, _, _|Gs], [NotC|NotCs], [C|Cs], Sum) :-
	NotC #= 1 #<=> C,
    Sum #= Sum1 + C * GL,
	chosen_constraints(Gs, NotCs, Cs, Sum1).

split_chosen([], [], [], [], []).
split_chosen([P-_|GPsTail], [0|Cs], CPs, [P|RPs], Gs) :-
	split_chosen(GPsTail, Cs, CPs, RPs, Gs).
split_chosen([P-G|GPsTail], [1|Cs], [P-G|CPs], RPs, [GL, GW, GH|Gs]) :-
    G = grouped(GL, GW, GH, _, _, _),
	split_chosen(GPsTail, Cs, CPs, RPs, Gs).

append_vars([], [], []). 
append_vars([V|Vs], [GL, GW, GH|Gs], [GL, GW, GH, V|AllVs]) :-
	append_vars(Vs, Gs, AllVs).


vars_selection(1, Cs, Gs, MaxH, _NotCs, Vars) :-
    append(Cs, Gs, Vs1),
    append([MaxH], Vs1, Vars).

vars_selection(2, _Cs, Gs, MaxH, NotCs, Vars) :-
    append(NotCs, Gs, Vs1),
    append([MaxH], Vs1, Vars).


vars_selection(3, Cs, Gs, MaxH, _NotCs, Vars) :-
    append(Gs, Cs, Vs1),
    append([MaxH], Vs1, Vars).

% use NotCs, no Gs, Cs last
vars_selection(4, _Cs, Gs, MaxH, NotCs, Vars) :-
    append(Gs, NotCs, Vs1),
    append([MaxH], Vs1, Vars).

% weaving NotCs and Gs (BEST, with maxH as Cost funciont)
vars_selection(5, _Cs, Gs, MaxH, NotCs, Vars) :-
    append_vars(NotCs, Gs, Vs1),
	append([MaxH], Vs1, Vars).


labeling_options(1, []).
labeling_options(2, [down]).
labeling_options(3, [enum]).
labeling_options(4, [enum, down]).
labeling_options(5, [bisect]).
labeling_options(6, [bisect, down]).


bosh_labeling(OptNumber, Vars, Cost) :-
    labeling_options(OptNumber, Opt),
    append([Cost], Vars, VarsAll),
	labeling([minimize(Cost), time_out(30000, _Flag)|Opt], VarsAll).



bosh([VarsSelectionOption, LabelingOption], Fs, res(CPs, DPs)) :-
    max_available_height(AvalH),
    bosh([VarsSelectionOption, LabelingOption], Fs, AvalH, 1, CPs, DPs).


bosh(_, [], _, _, [], []).
bosh([VarsSelectionOption, LabelingOption], [[]|Fs], AvalH, Bay, CPs, DPs) :-
    bosh([VarsSelectionOption, LabelingOption], Fs, AvalH, Bay, CPs, DPs).

bosh([VarsSelectionOption, LabelingOption], [F|Fs], AvalH, Bay, [(Bay, NF, ShelveH)-CPs|CPsTail], DPs) :-
    length(F, Size), F = [product(NF,_,_,_,_)|_], write([NF, AvalH, Bay, Size]), nl,
    bay(MaxSL, _, _, MaxSH),
    shelf(THICK, TG, LG, _IG, _RG),
    (AvalH = 3000 ->
	  TopGap is TG % no shelve yet
	; TopGap is THICK + TG), % top gap 40+15

    maxH_domain(AvalH, MaxH),
    group_products(F, MaxH, TopGap, Gs, GPs, DPs1),

    chosen_constraints(Gs, NotCs, Cs, MaxL),
    RemainL #>= 0,
	RemainL #= MaxSL - LG - MaxL, 
    domain(NotCs, 1, 2),

    vars_selection(VarsSelectionOption, Cs, Gs, MaxH, NotCs, Vars),

    % maximum(CsMax, Cs),
    % Cost #= MaxH + RemainL + (1-CsMax)* 1000000,
    %Cost #= MaxH + RemainL,
    Cost #= MaxH,
    bosh_labeling(LabelingOption, Vars, Cost),
	%labeling([minimize(Cost), time_out(3000, _Flag)], Vars),
    %print([RemainL, Cost, MaxH]),
	split_chosen(GPs, Cs, CPs, RPs, _CPVars),
	%labeling([], [MaxH|CPVars]),

	length(CPs, L1),

    ( L1 > 0, !; false ),
 
 	ShelveH is AvalH - MaxH,
	append(DPs1, RPs, F1),
	( ShelveH > 0, bosh([VarsSelectionOption, LabelingOption], [F1|Fs], ShelveH, Bay, CPsTail, DPs), !;
	( NextBay is Bay + 1,
	  ( bosh([VarsSelectionOption, LabelingOption], [F1|Fs], MaxSH, NextBay, CPsTail, DPs2), DPs = DPs2; DPs = F1, nl, nl, write(['\'Family shelve not complete, dropped products: \'', DPs]), nl, nl)
	)).

go([VarsSelectionOption, LabelingOption], NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    %set_prolog_flag(gc, off),
    %statistics, 
    fd_statistics, reset_timer,
    bosh([VarsSelectionOption, LabelingOption], Fs, Res), print_time('Time: '),
    %set_prolog_flag(gc,on), 
    fd_statistics, Res = res(CPs, DPs), nl, length(CPs, L) , print([L, DPs]), 
    fd_statistics.
    %statistics.


go([VarsSelectionOption, LabelingOption], NI, N) :- families_sorted(Fs), go([VarsSelectionOption, LabelingOption], NI, N, Fs).
go([VarsSelectionOption, LabelingOption]) :-  families_sorted(Fs), length(Fs, L), go([VarsSelectionOption, LabelingOption], 1, L, Fs).
go([VarsSelectionOption, LabelingOption], N) :- families_sorted(Fs), go([VarsSelectionOption, LabelingOption], N, 1, Fs).

go(N) :- go([9,1], N).
go :- go([9,1]).



goAll([VarsSelectionOption, LabelingOption]) :-
    read_products(Ps),
    maplist(samsort(by_volume), [Ps], Fs),
     !, fd_statistics, reset_timer,
     bosh([VarsSelectionOption, LabelingOption], Fs, Res), print_time('Time: '),
     fd_statistics, Res = res(CPs, DPs), nl, length(CPs, L) , print([L, DPs]), fd_statistics.%, statistics.


goU([VarsSelectionOption, LabelingOption], N) :- families(Fs), go([VarsSelectionOption, LabelingOption], N, 1, Fs).




%-------------------------------------------------------------------------
%  Unit tests
%-------------------------------------------------------------------------
:- use_module(library(plunit)).

:- begin_tests(bosh).

test(rotate_grouded) :-
    rotate([1,2,3], [1,2,3]),
    rotate([1,2,3], [1,3,2]),
    rotate([1,2,3], [2,1,3]),
    rotate([1,2,3], [2,3,1]),
    rotate([1,2,3], [3,1,2]),
    rotate([1,2,3], [3,2,1]).

test(rotate_grouded) :-
    rotate([100,100,300], [300,100,100]).

test(rotate_vars, nondet) :-
    rotate([1,2,3], [RL,RH,RW]),
    permutation([1,2,3], [RL,RH,RW]).

% product(F, Q, L, H, W)
% group_product(product(F, Q, L, H, W))

test('group - grounded') :- group_products([product(1, 5, 100, 620, 700)],
    3000, 15, [710,500,620], [product(1, 5, 100, 620, 700)-grouped(710, 500, 620, 700, 100, 620)],[]).

test(group) :- 
    MaxH in 0..1000, 
    Ps = [product(1, 7, 100, 620, 700)],
    group_products(Ps, MaxH, 55, [BL,BW,BH],
        [product(1, 7, 100, 620, 700)-grouped(BL, BW, BH, RL, RW, RH)], []),
    MaxH in 155..1000,
    RL in{100}\/{620}\/{700},
    RH in{100}\/{620}\/{700},
    RW in{100}\/{620},
    BL in 110..4910,
    BH in 100..4900,
    BW in 100..3720.  

test('group - droped prod') :- 
    MaxH in 0..100,
    Ps = [product(1, 7, 100, 620, 700)],
    group_products(Ps, MaxH, 55, [], [], [product(1, 7, 100, 620, 700)]).

test('group - list 2 prod') :- 
    MaxH in 0..100,
    Ps = [product(1, 7, 100, 620, 700), product(1, 2, 10, 620, 70)],
    group_products(Ps, MaxH, 55, [BL,BW,BH],
        [product(1, 2, 10, 620, 70)-grouped(BL, BW, BH, RL, RW, RH)],
        [product(1, 7, 100, 620, 700)]),  
    RH = 10,
    MaxH in 65..100,
    RL in{70}\/{620},
    RW in{70}\/{620},
    BL in 80..1250,
    BH in 10..20,
    BW in 70..1240.

:- end_tests(bosh).
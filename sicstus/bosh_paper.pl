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
	statistics(total_runtime, _).
get_time(TS) :- 
	statistics(total_runtime,[_,T]),
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

% shelve(thickness, top gap, left gap, inter gap, right gap)
shelve(40, 15, 10, 10, 10).


% rotate(+ProductDimensions, -RotateProdutcDimensions)
rotate([L, H, W], [RL, RH, RW]) :- 
	Vs = [IL, IW, IH],
	%domain(Vs, 1, 3),
    all_distinct(Vs),
    element(IL, [L,H,W], RL),
    element(IH, [L,H,W], RH),
    element(IW, [L,H,W], RW).

% group_product(+Product, +MaxH, -GroupedProduct)
group_products([], _, _, [], [], []) :- !.
group_products([product(_F, Q, L, H, W)|Ps], MaxH, TopGap, [GL, GW, GH|Ls],
    [product(_F, Q, L, H, W)-grouped(GL, GW, GH, RL, RW, RH)|GPsTail], DPs) :-
    bay(SL, _, SW, _),
    shelve(_THICK, _TG, LG, IG, _RG),
    rotate([L, H, W], [RL, RH, RW]), 
	NL in 1..Q, NH in 1..Q, NW in 1..Q,		

    NL * RL + IG + LG #=< SL ,
   	NH * RH + TopGap #=< MaxH,
	NW * RW #=< SW, ((NW+1) * RW #> SW #\/ (NH #= 1 #/\ NL #= 1 #/\ NW #= Q)),

    NL * NH * NW #>= Q,
	NL * NH * NW #=< Q * 2, 
    GL #= NL * RL + IG,
    GH #= NH * RH,
    GW #= NW * RW, 
    group_products(Ps, MaxH, TopGap, Ls, GPsTail, DPs), !.

group_products([P|Ps], MaxH, TopGap, Ls, GPs, [P|DPsTail]) :- 
    group_products(Ps, MaxH, TopGap, Ls, GPs, DPsTail), !.

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
chosen_constraints([_-grouped(GL, GW, GH, _, _, _)|GPsTail], [V|Vs], [C|Cs], Sum) :-
	V #= 1 #<=> C,
	chosen_constraints(GPsTail, Vs, Cs, Sum1),
	Sum #= Sum1 + C * GL * GW * GH. 
	
split_chosen([], [], [], []).
split_chosen([P-_|GPsTail], [0|Cs], CPs, [P|RPs]) :-
	split_chosen(GPsTail, Cs, CPs, RPs).
split_chosen([P-G|GPsTail], [1|Cs], [P-G|CPs], RPs) :-
	split_chosen(GPsTail, Cs, CPs, RPs).

append_vars([], [], []).
append_vars([V|Vs], [_-grouped(GL, GW, GH, _, _, _)|GPsTail], [GL, GW, GH, V|AllVs]) :-
	append_vars(Vs, GPsTail, AllVs).

% TODO: add link to source
bosh-knapsack(Coeffs, GPs, Total) :-
        ( foreach(C, Coeffs),
          foreach(GP, GPs),
            fromto(0, S1, S2, Sum)
        do  
            GP = _-grouped(GL, _, _, _, _, _),
            S2 #= S1 + C * GL),
            Total #= Sum.

bosh(Fs, res(CPs, DPs)) :- 
    max_available_height(AvalH),
    bosh(Fs, AvalH, 1, CPs, DPs).

bosh([], _, _, [], []).
bosh([[]|Fs], AvalH, N, CPs, DPs) :- 
    bosh(Fs, AvalH, N, CPs, DPs).

% N is number of bays used by current family (acc), NT is current total number os bays used
bosh([F|Fs], AvalH, N, [(N, NF, ShelveH)-CPs|CPsTail], DPs) :- 
    length(F, Size), F = [product(NF,_,_,_,_)|_], write([NF, AvalH, N, Size]), nl,    
    bay(MaxSL, _, _, MaxSH),
    shelve(THICK, TG, LG, _IG, _RG),
    (AvalH = 3000 -> 
	  TopGap is TG % no shelve yet
	; TopGap is THICK + TG), % top gap 40+15

    maxH_domain(AvalH, MaxH),
    group_products(F, MaxH, TopGap, Ls, GPs, DPs1),

    chosen_constraints(GPs, Vs, Cs, FilledSum),
    domain(Vs, 1, 2),

	MaxL #=< MaxSL - LG,
	bosh-knapsack(Cs, GPs, MaxL),

    append_vars(Vs, GPs, Vs1),
	append(Vs1, [MaxH], Vars),
%	append(Vs1, [FilledSum, MaxH, MaxL], Vars),

 /*   
    MedUtil #> 0,
	MedUtil * MaxH * MaxL  #=< FilledSum,	
	(MedUtil+1) * MaxH * MaxL #> FilledSum,
	labeling([maximize(MedUtil), time_out(3000, _Flag)], Vars),

    Waste #= MaxH + (MaxSL - LG - MaxL),
	labeling([minimize(Waste), time_out(3000, _Flag)], Vars),
*/

	labeling([minimize(MaxH), time_out(3000, _Flag)], Vars),

	split_chosen(GPs, Cs, CPs, RPs),
	%append(CPs, CPVars),
	labeling([], Ls),

	length(CPs, L1),
	%length(RPs, L2),
	%print([L1, CPs, L2]), nl, 
    
    ( L1 > 0, !; false ),
	%check_chosen(CPs, MaxH),
	ShelveH is AvalH - MaxH,
	append(DPs1, RPs, F1),
	( ShelveH > 0, bosh([F1|Fs], ShelveH, N, CPsTail, DPs), !;
	( N1 is N + 1,
	  ( bosh([F1|Fs], MaxSH, N1, CPsTail, DPs2), DPs = DPs2; DPs = F1, nl, nl, write(['\'Family shelve not complete, dropped products: \'', DPs]), nl, nl)
	)).

go(NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    %length(FsFull, L), LPos = L - NF, 
    length(Fs, N), append(Fs,_, Ts),
    !, fd_statistics, reset_timer, 
    bosh(Fs, Res), print_time('Time: '), 
    fd_statistics, Res = res(CPs, DPs), nl, length(CPs, L) , print([L, DPs]), fd_statistics.%, statistics.

go(NI, N) :- families_sorted(Fs), go(NI, N, Fs).
go :-  families_sorted(Fs), length(Fs, L), go(1, L, Fs).
go(N) :- families_sorted(Fs), go(N,1, Fs).

goU(N) :- families(Fs), nth1(N, Fs, F), fd_statistics, reset_timer, bosh([F], Res), !, print_time(Res), Res = res(NBays, NT, CPs, _), nl, length(CPs, L) , print([L, NT, NBays]), fd_statistics.%, statistics.




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
    3000, 15, [product(1, 5, 100, 620, 700)-grouped(710, 500, 620, 700, 100, 620)],[]).

test(group) :- 
    MaxH in 0..1000, 
    Ps = [product(1, 7, 100, 620, 700)],
    group_products(Ps, MaxH, 55,  
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
    group_products(Ps, MaxH, 55, [], [product(1, 7, 100, 620, 700)]).  

test('group - list 2 prod') :- 
    MaxH in 0..100,
    Ps = [product(1, 7, 100, 620, 700), product(1, 2, 10, 620, 70)],
    group_products(Ps, MaxH, 55,
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
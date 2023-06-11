:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- consult('process_files.pl').
:- consult('bosh_labeling.pl').
:- consult('export.pl').

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


% bay(width, height, depth, available height)
bay(1200, 2400, 650, 3000).
max_available_height(H) :- bay(_,_,_, H).

% shelf(thickness, top gap, left gap, inter gap, right gap)
shelf(40, 15, 10, 10, 10).


% rotate(+ProductDimensions, -RotatedProductDimensions)
rotate(ProductDimensions, [RL, RH, RW]) :- 
    all_distinct([IL, IW, IH]),
    element(IL, ProductDimensions, RL),
    element(IH, ProductDimensions, RH),
    element(IW, ProductDimensions, RW).

% group_products(+Products, +MaxH, +TopGap, -Gs, -GroupedProducts, -DropedProducts)
% products       : products to be shelved
% MaxH           : height of on current shelf 
% TopGap         : gap on top of shelf (for first shelf on bay is different
% Gs             : varibles with dimentions of grouped products. used on labeling
% GPs            : all possible grouped-products, with information for shelving 
% DropedProducts : products that cannot be placed on the current shelf 
group_products([], _, _, [], [], []) :- !.
group_products([product(_F, Q, L, H, W)|Ps], MaxH, TopGap, [GL, GW, GH|Gs],
    [product(_F, Q, L, H, W)-grouped(GL, GW, GH, RL, RW, RH)|GPs], DPs) :-
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
    !, group_products(Ps, MaxH, TopGap, Gs, GPs, DPs).
group_products([P|Ps], MaxH, TopGap, Gs, GPs, [P|DPs]) :-
    group_products(Ps, MaxH, TopGap, Gs, GPs, DPs), !.

% maxH_domain(+AvalH, -MaxH)
% return the domain of possible height  of shelves
% AvalH : height aavaliable on bay for the next shelf
% MaxH  : variable with the max height of the current shelf, with his domain defined  
maxH_domain(AvalH, MaxH) :-
    bay(_, BH, _, BAH),
	( AvalH =< BH ->
	  MinHDomain is 100 
	; MinHDomain is BAH-BH),
    % 100 = minimal space between shelves
    % 50 = inteval between shelves heights
	numlist(MinHDomain, 50, AvalH, _, MaxHDomain),
	list_to_fdset(MaxHDomain, FDS_MaxHDomain),
	MaxH in_set FDS_MaxHDomain.
% chosen_constraints(+Gs, -AntiCs, Cs, MaxL)
% Gs     : dimentions of grouped products.
% AntiCs : auxiliary variables used to invert value selection order of Cs (Chosen)
%          needed on the best order strategy 
% Cs     : products chosen to be on current shelf
chosen_constraints([], [], [], 0).
chosen_constraints([GL, _, _|Gs], [AntiC|AntiCs], [C|Cs], MaxL) :-
	AntiC #= 1 #<=> C,
    MaxL #= MaxL1 + C * GL,
	chosen_constraints(Gs, AntiCs, Cs, MaxL1).

% split_chosen(GPs, Cs, CPs, RPs)
% GPs : all possible grouped-products, with information for shelving 
% Cs  : products chosen to be on current shelf
% CPs : properties of chosen products
% RPs : remain products, not chosen to be on current shelf
split_chosen([], [], [], []).
split_chosen([P-_|GPsTail], [0|Cs], CPs, [P|RPs]) :-
	split_chosen(GPsTail, Cs, CPs, RPs).
split_chosen([P-G|GPsTail], [1|Cs], [P-G|CPs], RPs) :-
	split_chosen(GPsTail, Cs, CPs, RPs).





bosh(SearchOptions, Fs, res(CPs, DPs)) :-
    max_available_height(AvalH),
    bosh(SearchOptions, Fs, AvalH, 1, CPs, DPs).


% bosh(+SearchOptions, +Fs, +AvalH, -Bay, -CPs, -DPs
% SearchOptions : options for order and labeling 
% Fs            : list of families of products
% AvalH         : space avaliable for the next shelf
% Bay           : current bay used
% CPs           : properties of chosen products
% DPs           : products that cannot be placed on any bay 
%                 (must be [] for actual test data and bays/shelves dimentions)
bosh(_, [], _, _, [], []).
bosh(SearchOptions, [[]|Fs], AvalH, Bay, CPs, DPs) :-
    bosh(SearchOptions, Fs, AvalH, Bay, CPs, DPs).
bosh(SearchOptions, [F|Fs], AvalH, Bay, [(Bay, NF, ShelveH)-CPs|CPsTail], DPs) :-
    length(F, Size), F = [product(NF,_,_,_,_)|_], 
    format('Family: ~p, Size: ~p, Bay: ~p, Avaliable height: ~p', [NF, Size, Bay, AvalH]), nl,

    bay(MaxSL, _, _, MaxSH),
    shelf(THICK, TG, LG, _IG, _RG),

    maxH_domain(AvalH, MaxH),

    (AvalH = MaxSH ->
	  TopGap is TG % first shelf on top
	; TopGap is THICK + TG), % top gap need to consider thickness of shelf above 

    group_products(F, MaxH, TopGap, Gs, GPs, DPs1),

    chosen_constraints(Gs, AntiCs, Cs, MaxL),
    domain(AntiCs, 1, 2),


    MaxL #=< MaxSL - LG, 

    SearchOptions = [VarsSelectionOption, LabelingOption],
    paper_vars_orders(VarsSelectionOption, Cs, Gs, MaxH, AntiCs, Vars),

    %Cost #= MaxH + MaxSL - LG - MaxL,
    Cost #= MaxH,
    bosh_labeling(LabelingOption, Vars, Cost, 10000),
	split_chosen(GPs, Cs, CPs, RPs),
	length(CPs, L1),
    ( L1 > 0, !; false ),
 	ShelveH is AvalH - MaxH,
	append(DPs1, RPs, F1),
	( ShelveH > 0, bosh(SearchOptions, [F1|Fs], ShelveH, Bay, CPsTail, DPs), !;
	( NextBay is Bay + 1, % no more space, get another bay
	  ( bosh(SearchOptions, [F1|Fs], MaxSH, NextBay, CPsTail, DPs2), DPs = DPs2; 
        % if fails on a new bay, there is no way to continue 
        DPs = F1, nl, nl, write(['\'Family shelve not complete, dropped products: \'', DPs]), nl, nl)
	)).

go(SearchOptions, NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    %statistics, 
    %fd_statistics, 
    reset_timer,
    bosh(SearchOptions, Fs, Res), print_time('Time: '),
    %fd_statistics, 
    %statistics,
    Res = res(CPs, _DPs), 
    last(CPs, (NBay, _, _)-_),
    nl, format('Number of bays used: ~p', NBay), nl, nl.


% shelve a N families, starting with family NI 
go(SearchOptions, NI, N) :- families_sorted(Fs), go(SearchOptions, NI, N, Fs).

% shelve one specify family
go(SearchOptions, N) :- families_sorted(Fs), go(SearchOptions, N, 1, Fs).

% shelve all families, one by one,
go(SearchOptions) :-  families_sorted(Fs), length(Fs, L), go(SearchOptions, 1, L, Fs).

% shelve one specify family, with best strategy
go(N) :- go([3, 5], N).
% shelve all families, one by one, with best strategy
go :- go([3, 5]).


% shelve one family with several SearchOption and write results in a file.
go_all_options(N) :-
    families_sorted(Fs),
    %( foreach(VarsSelectionOption, [1,2]),
    ( foreach(VarsSelectionOption, [3]),
      foreach(R1s, Rs),
      param(N, Fs) do
%      ( foreach(LabelingOption, [1,5]),
%      ( foreach(LabelingOption, [1,2,3,4,5,6]),
%      ( foreach(LabelingOption, [31,32,33,34,35,36]),
      ( foreach(LabelingOption, [1,2,3,4,5,6,11,12,13,14,15,16,21,22,23,24,25,26,31,32,33,34,35,36]),
        foreach(R, R1s),
        param(N, Fs, VarsSelectionOption) do
            ( catch(go_all_options([VarsSelectionOption, LabelingOption], N, 1, Fs, Time, NBay),
                _Ex, [Time, NBay] = [0,0]), 
              R = [VarsSelectionOption, LabelingOption, N, Time, NBay]
            ; R = [VarsSelectionOption, LabelingOption, N, 0, 0]
            ), !, print(R)
      )
    ), nl, print(Rs), nl,
    write_matrix_in_file('output/result_all_options.py', 'Rs', Rs).
go_all_options(SearchOptions, NI, N, FsFull, Time, NBay) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    reset_timer,
    bosh(SearchOptions, Fs, Res), !,
    get_time(Time),
    Res = res(CPs, []),
    last(CPs, (NBay, _, _)-_).


% shelve all families, one by one, and write time and bay results for each one.
go_timer_stats :-
    go_timer_stats([3, 5]).

go_timer_stats(SearchOptions) :-
    families_sorted(Fs),
    length(Fs, L),
    go_timer_stats(SearchOptions, 1, L, Fs).
go_timer_stats(SearchOptions, NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts),
    ( foreach(F, Fs),
      foreach(M, Ms),
      param(SearchOptions) do
        length(F, L),
        reset_timer,
        bosh(SearchOptions, [F], Res),
        get_time(T),
        Res = res(CPs, []),
        last(CPs, (UsedBays, N, _)-_),
        M = [N, L, T, UsedBays]
    ),
    write_matrix_in_file('output/result_stats.py', 'Ms', Ms),
    write_file('output/result_stats.csv', Ms).


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

% rotate give us a permutation of dimentions (without grounding the domain variables)
test(rotate, nondet) :-
    rotate([1,2,3], [RL,RH,RW]), 
    RL in{1}\/{2}\/{3},
    RH in{1}\/{2}\/{3},
    RW in{1}\/{2}\/{3},
    permutation([1,2,3], [RL,RH,RW]).


test('group - grounded') :- group_products([product(1, 5, 100, 620, 700)],
    3000, 15, [710,500,620], [product(1, 5, 100, 620, 700)-grouped(710, 500, 620, 700, 100, 620)],[]).

test(group) :- 
    MaxH in 0..1000, 
    Ps = [product(1, 7, 100, 620, 700)],
    group_products(Ps, MaxH, 55, [GL,GW,GH],
        [product(1, 7, 100, 620, 700)-grouped(GL, GW, GH, RL, RW, RH)], []),
    MaxH in 155..1000,
    RL in{100}\/{620}\/{700},
    RH in{100}\/{620}\/{700},
    RW in{100}\/{620},
    GL in 110..4910,
    GH in 100..4900,
    GW in 100..3720.  

test('group - droped prod') :- 
    MaxH in 0..100,
    Ps = [product(1, 7, 100, 620, 700)],
    group_products(Ps, MaxH, 55, [], [], [product(1, 7, 100, 620, 700)]).

test('group - list 2 prod') :- 
    MaxH in 0..100,
    Ps = [product(1, 7, 100, 620, 700), product(1, 2, 10, 620, 70)],
    group_products(Ps, MaxH, 55, [GL,GW,GH],
        [product(1, 2, 10, 620, 70)-grouped(GL, GW, GH, RL, RW, RH)],
        [product(1, 7, 100, 620, 700)]),  
    RH = 10,
    MaxH in 65..100,
    RL in{70}\/{620},
    RW in{70}\/{620},
    GL in 80..1250,
    GH in 10..20,
    GW in 70..1240.

:- end_tests(bosh).
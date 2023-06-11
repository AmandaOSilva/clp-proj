:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- consult('process_files.pl').
:- consult('bosh_labeling.pl').
:- consult('export_cumulative.pl').

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


%product(FamylyNumber, Quantity, Lenght, Height, Width)

% bay(width, height, depth, available height)
% add 50 to available height for dealing with top gap of first shelf, 
% in this approach top gap are treated as same for all shelves
bay(1200, 2400, 650, 3050). 
max_available_height(H) :- bay(_,_,_, H).

% shelf(thickness, top gap, left gap, inter gap, right gap)
shelf(40, 15, 10, 10, 10).


% rotate(+ProductDimensions, -RotatedProductDimensions)
rotate(ProductDimensions, [RL, RH, RW]) :- 
    all_distinct([IL, IW, IH]),
    element(IL, ProductDimensions, RL),
    element(IH, ProductDimensions, RH),
    element(IW, ProductDimensions, RW).

% group_product(+Product, +MaxH, +TopGap, -Variables, -GroupedProducts)
group_products([], _, _, [], [], []) :- !.
group_products([product(_F, Q, L, H, W)|Ps], MaxHs, TopGap,
        [V|Vs], [task(V,1,_,GL,1)|Tasks], 
        [product(_F, Q, L, H, W)-grouped(GL, GW, GH, RL, RW, RH)|GPs]) :-
    bay(SL, _, SW, _),
    shelf(_THICK, _TG, LG, IG, _RG),

    element(V, MaxHs, MaxH),

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
    !, group_products(Ps, MaxHs, TopGap, Vs, Tasks, GPs).

% get the doamin of shelves height (MaxHs)
maxH_domain(AvalH, NShelves, MaxHs) :-
    % 100 = minimal space between shelves
    % 50 = inteval between shelves heights
    numlist(100, 50, AvalH, _, MaxHDomain),
	list_to_fdset([0|MaxHDomain], FDS_MaxHDomain),	
	length(MaxHs, NShelves),
	( foreach(MaxH, MaxHs),
	  param(FDS_MaxHDomain)	do 
      MaxH in_set FDS_MaxHDomain
    ).

% shelve products, put then into shelves and shelves in to bays.
shelve([], [], _, _, []).
shelve([P-G|GPsTail], [V|Vs], MaxHs, Bays, [(Bay, V, MaxH)-(P-G)|CPs]) :-
    element(V, Bays, Bay),
    element(V, MaxHs, MaxH),
	shelve(GPsTail, Vs, MaxHs, Bays, CPs).

first_shelves(Bays, MaxHs, NBays) :-
    same_length(Fs, Bays),
    ( foreach(Bay, Bays),
      foreach(F, Fs),
      param(MaxHs) do 
      element(Bay, MaxHs, MaxH),
      MaxH #>= 650 #<=> B,
      F #= B * Bay
    ),
    nvalue(NBays, Fs). 

get_tasks_bays([], [], []).
get_tasks_bays([MaxH|MaxHs], [task(Bay, 1, _, MaxH, 1)|TasksBays], [Bay|Bays]):-
    get_tasks_bays(MaxHs, TasksBays, Bays).



bosh(SearchOptions, Fs, res(CPs, NBay)) :-
    max_available_height(AvalH),
    bosh(SearchOptions, Fs, AvalH, CPs, NBay).

bosh(_, [], _, [], 0).
bosh(SearchOptions, [F|Fs], AvalH, [CPs|CPsTails], NBay) :-
    bosh_family(SearchOptions, F, AvalH, CPs, NBay1),
    bosh(SearchOptions, Fs, AvalH, CPsTails, NBayTail),
    NBay is NBay1 + NBayTail.


bosh_family([VarsSelectionOption, LabelingOption], F, AvalH, CPs, NBay) :-
    nl,
    length(F, Size), F = [product(NF,_,_,_,_)|_], 
    format('Family: ~p, Size: ~p', [NF, Size]), nl,

    bay(MaxSL, _, _, MaxSH),
    shelf(THICK, TG, LG, _IG, _RG),
	
    % in this approach, top gap allways be the same;max avaliable height (MaxSH) one "degree" above (3050 in this case) 
    TopGap is THICK + TG, 
    
    maxH_domain(AvalH, 100, MaxHs),

    group_products(F, MaxHs, TopGap, Vs, Tasks, GPs),
    domain(Vs, 1, 100),

    MaxL #>0, MaxL #=< MaxSL - LG,
    MaxL is MaxSL - LG,
    cumulative(Tasks, [limit(MaxL), global(true)]),

    get_tasks_bays(MaxHs, TasksBays, Bays),
    domain(Bays, 1, 50),

    cumulative(TasksBays, [limit(MaxSH), global(true)]),

    cumulative_vars_orders(VarsSelectionOption, Vs, GPs, MaxHs, Bays, Vars),

    %sum(MaxHs, #=, Shelves),    
    maximum(NBay, Bays),

    first_shelves(Bays, MaxHs, NBay),

    %Cost * 50 #= Shelves,
    Cost  #= NBay,
 
    bosh_labeling(LabelingOption, Vars, Cost, 5000),
    shelve(GPs, Vs, MaxHs, Bays, CPs).

go(SearchOptions, NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    %statistics, 
    %fd_statistics, 
    reset_timer,
    %findall(Res, bosh(SearchOptions,Fs, Res), ResAll),
    bosh(SearchOptions, Fs, Res), print_time('Time: '),
    %fd_statistics, 
    Res = res(_CPs, NBay),
    nl, format('Number of bays used: ~p', NBay), nl, nl.
    %statistics.


% shelve a N families, starting with family NI 
go(SearchOptions, NI, N) :- families_sorted(Fs), go(SearchOptions, NI, N, Fs).

% shelve one specify family
go(SearchOptions, N) :- families_sorted(Fs), go(SearchOptions, N, 1, Fs).

% shelve all families, one by one,
go(SearchOptions) :-  families_sorted(Fs), length(Fs, L), go(SearchOptions, 1, L, Fs).

% shelve one specify family, with best strategy
go(N) :- go([1, 1], N).

% shelve all families, one by one, with best strategy
go :- go([1, 1]).





% shelve one family with several SearchOption and write results in a file.
go_all_options(N) :-
    families_sorted(Fs),
    ( foreach(VarsSelectionOption, [1, 2]),
%    ( foreach(VarsSelectionOption, [1]),
      foreach(R1s, Rs),
      param(N, Fs) do
%      ( foreach(LabelingOption, [6]),
%      ( foreach(LabelingOption, [1,2,3,4,5,6]),
%      ( foreach(LabelingOption, [1,2,3,4,5,6,11,12,13,14,15,16,21,22,23,24,25,26,31,32,33,34,35,36]),
      ( foreach(LabelingOption, [1,3,5,21,23,25]),
        foreach(R, R1s),
        param(N, Fs, VarsSelectionOption) do
            ( catch(go_all_options([VarsSelectionOption, LabelingOption], N, 1, Fs, Time, NBay),
                _Ex, [Time, NBay] = [0,0]), 
              R = [VarsSelectionOption, LabelingOption, N, Time, NBay]
            ; R = [VarsSelectionOption, LabelingOption, N, 0, 0]
            ), !, print(R)
      )
%      append(R1s, R1sFlat)
    ), print(Rs),
    write_matrix_in_file('../sicstus/output/result_all_options.py', 'Rs', Rs).
go_all_options(SearchOptions, NI, N, FsFull, Time, NBay) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    reset_timer,
    bosh(SearchOptions, Fs, Res), !,
    get_time(Time),
    Res = res(_CPs, NBay).


% shelve all families, one by one, and write time and bay results for each one.
go_timer_stats :-
  go_timer_stats([1, 1]).
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
        F = [product(N, _, _, _, _)|_],
        length(F, L),
        reset_timer,
        bosh(SearchOptions, [F], Res),
        get_time(T),
        Res = res(_CPs, NBay),
        M = [N, L, T, NBay]
    ),
    write_matrix_in_file('../sicstus/output/result_stats.py', 'Ms', Ms),
    write_file('../sicstus/output/result_stats.csv', Ms).



%-------------------------------------------------------------------------
%  Unit tests
%-------------------------------------------------------------------------
:- use_module(library(plunit)).

:- begin_tests(bosh_cumulative).

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

test('group - grounded') :- 
    group_products([product(1, 5, 100, 620, 700)], [3000], 15, [1], 
        [task(1, 1, 2, 710, 1)], [product(1, 5, 100, 620, 700)-grouped(710, 500, 620, 700, 100, 620)]).


test(group) :-     
    Ps = [product(1, 7, 100, 620, 700)],
    group_products(Ps, [MaxH], 55, [V], [task(V,1,_,GL,1)], [product(1, 7, 100, 620, 700)-grouped(GL, GW, GH, RL, RW, RH)]),
    MaxH in 155..1000,
    RL in{100}\/{620}\/{700},
    RH in{100}\/{620}\/{700},
    RW in{100}\/{620},
    GL in 110..4910,
    GH in 100..4900,
    GW in 100..3720.  

:- end_tests(bosh_cumulative).
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
% add 50 to available height for dealing with top gap of first shelf, 
% in this approach top gap are treated as same for all shelves
bay(1200, 2400, 650, 3050). 
max_available_height(H) :- bay(_,_,_, H).

% shelf(thickness, top gap, left gap, inter gap, right gap)
shelf(40, 15, 10, 10, 10).


% rotate(+ProductDimensions, -RotatedProductDimensions)
rotate([L, H, W], [RL, RH, RW]) :- 
    all_distinct([IL, IW, IH]),
    element(IL, [L,H,W], RL),
    element(IH, [L,H,W], RH),
    element(IW, [L,H,W], RW).

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


split_chosen([], [], _, _, []).
split_chosen([P-G|GPsTail], [V|Vs], MaxHs, Bays, [(Bay, V, MaxH)-(P-G)|CPs]) :-
    element(V, Bays, Bay),
    element(V, MaxHs, MaxH),
	split_chosen(GPsTail, Vs, MaxHs, Bays, CPs).


weaving_vars([], [], [], []). 
weaving_vars([V|Vs],[_-grouped(GL, GW, GH, RL, RW, RH)|GPs], [GL, RL, GH, RH, GW, RW, V|WeavingVars], [GL, RL, GH, RH, GW, RW |Ls]) :-
	weaving_vars(Vs, GPs, WeavingVars, Ls).


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


vars_order(1, Vs, _Gs, MaxHs, Bays, Ls, Vars) :-
	append(Ls, Vs, Vs2),
	append(Vs2, MaxHs, Vs3),
	append(Vs3, Bays, Vars).

% weaving 
vars_order(2, _Vs, Gs, MaxHs, Bays, _Ls, Vars) :-
	append(Gs, MaxHs, Vs3),
	append(Vs3, Bays, Vars).

get_tasks_bays([], [], []).
get_tasks_bays([MaxH|MaxHs], [task(Bay, 1, _, MaxH, 1)|TasksBays], [Bay|Bays]):-
    get_tasks_bays(MaxHs, TasksBays, Bays).


labeling_options(1, []).
labeling_options(2, [down]).
labeling_options(3, [enum]).
labeling_options(4, [enum, down]).
labeling_options(5, [bisect]).
labeling_options(6, [bisect, down]).

labeling_options(11, [ff]).
labeling_options(12, [ff, down]).
labeling_options(13, [ff, enum]).
labeling_options(14, [ff, enum, down]).
labeling_options(15, [ff, bisect]).
labeling_options(16, [ff, bisect, down]).

labeling_options(21, [impact]).
labeling_options(22, [impact, down]).
labeling_options(23, [impact, enum]).
labeling_options(24, [impact, enum, down]).
labeling_options(25, [impact, bisect]).
labeling_options(26, [impact, bisect, down]).

labeling_options(31, [ffc]).
labeling_options(32, [ffc, down]).
labeling_options(33, [ffc, enum]).
labeling_options(34, [ffc, enum, down]).
labeling_options(35, [ffc, bisect]).
labeling_options(36, [ffc, bisect, down]).

bosh_labeling(OptNumber, Vars, Cost) :-
    labeling_options(OptNumber, Opt),
    append(Vars, [Cost], VarsAll),
	labeling([minimize(Cost), time_out(10000, _Flag), all|Opt], VarsAll).

bosh([VarsSelectionOption, LabelingOption], Fs, res(CPs, NBay)) :-
    max_available_height(AvalH),
    bosh([VarsSelectionOption, LabelingOption], Fs, AvalH, CPs, NBay).

bosh(_, [], _, [], 0).
bosh([VarsSelectionOption, LabelingOption], [F|Fs], AvalH, [CPs|CPsTails], NBay) :-
    bosh_family([VarsSelectionOption, LabelingOption], F, AvalH, CPs, NBay1),
    bosh([VarsSelectionOption, LabelingOption], Fs, AvalH, CPsTails, NBayTail),
    NBay is NBay1 + NBayTail.


bosh_family([VarsSelectionOption, LabelingOption], F, AvalH, CPs, NBay) :-
    nl,
    length(F, Size), F = [product(NF,_,_,_,_)|_], write([NF, AvalH, Size]), 
    bay(MaxSL, _, _, MaxSH),
    shelf(THICK, TG, LG, _IG, _RG),
	
    TopGap is THICK + TG, % in this approah, top gap allways be 40+15

    maxH_domain(AvalH, 100, MaxHs),
    group_products(F, MaxHs, TopGap, Vs, Tasks, GPs),
    domain(Vs, 1, 100),

    MaxL #>0, MaxL #=< MaxSL - LG,
    MaxL is MaxSL - LG,
    cumulative(Tasks, [limit(MaxL), global(true)]),

    get_tasks_bays(MaxHs, TasksBays, Bays),
    domain(Bays, 1, 50),

    cumulative(TasksBays, [limit(MaxSH), global(true)]),

    weaving_vars(Vs, GPs, Gs, Ls),

    vars_order(VarsSelectionOption, Vs, Gs, MaxHs, Bays, Ls, Vars),

    %sum(MaxHs, #=, Shelves),    
    %maximum(NShelves, Vs),   
    maximum(NBay, Bays),

    first_shelves(Bays, MaxHs, NBay),

    %Cost * 50 #= Shelves,
    Cost  #= NBay,
    %labeling([],  Vars),
 
    bosh_labeling(LabelingOption, Vars, Cost),

    %print_time([NF, NBay, Shelves]),
    %print(MaxHs), print(Bays),
    
    split_chosen(GPs, Vs, MaxHs, Bays, CPs).

go([VarsSelectionOption, LabelingOption], NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    %set_prolog_flag(gc, off),
    %statistics, 
    fd_statistics, reset_timer,
    %findall(Res, bosh([VarsSelectionOption, LabelingOption],Fs, Res), ResAll),
    bosh([VarsSelectionOption, LabelingOption], Fs, Res), print_time('Time: '),
    %set_prolog_flag(gc,on), 
    fd_statistics, Res = res(CPs, DPs), nl, length(CPs, L) , print([L, DPs]), 
    fd_statistics.
    %statistics.



go([VarsSelectionOption, LabelingOption]) :-  families_sorted(Fs), length(Fs, L), go([VarsSelectionOption, LabelingOption], 1, L, Fs).
go(N) :- go([1,1], N).
go :- go([1,1]).

go([VarsSelectionOption, LabelingOption], N) :- families_sorted(Fs), go([VarsSelectionOption, LabelingOption], N, 1, Fs).
go([VarsSelectionOption, LabelingOption], NI, N) :- families_sorted(Fs), go([VarsSelectionOption, LabelingOption], NI, N, Fs).


go_all_options(N) :-
    families_sorted(Fs),
    %nth1(N, Fs, F),
    ( foreach(VarsSelectionOption, [2]),
%    ( foreach(VarsSelectionOption, [1]),
      foreach(R1s, Rs),
      param(N, Fs) do
%      ( foreach(LabelingOption, [6]),
%      ( foreach(LabelingOption, [1,2,3,4,5,6]),
      ( foreach(LabelingOption, [21,22,24,26]),
%      ( foreach(LabelingOption, [1,2,3,4,5,6,11,12,13,14,15,16,21,22,23,24,25,26,31,32,33,34,35,36]),
%      ( foreach(LabelingOption, [1,3,5,21,23,25]),
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

go_all_options([VarsSelectionOption, LabelingOption], NI, N, FsFull, Time, NBay) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts), !,
    %statistics, 
    reset_timer,
    bosh([VarsSelectionOption, LabelingOption], Fs, Res), !,
    get_time(Time),
    Res = res(_CPs, NBay).


go_timer_stats :-  go_timer_stats([1, 1]).

go_timer_stats([VarsSelectionOption, LabelingOption]) :-
    families_sorted(Fs),
    length(Fs, L),
    go_timer_stats([VarsSelectionOption, LabelingOption], 1, L, Fs).

go_timer_stats([VarsSelectionOption, LabelingOption], NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    length(Fs, N), append(Fs,_, Ts),

    ( foreach(F, Fs),
      foreach(M, Ms),
      param(VarsSelectionOption, LabelingOption) do
        F = [product(N, _, _, _, _)|_],
        length(F, L),
        reset_timer,
        bosh([VarsSelectionOption, LabelingOption], [F], Res),
        get_time(T),
        Res = res(_CPs, NBay),
        M = [N, L, T, NBay]
    ),
    write_matrix_in_file('../sicstus/output/result_stats.py', 'Ms', Ms),
    write_file('../sicstus/output/result_stats.csv', Ms).



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
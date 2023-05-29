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
group_products([], _, _, [], [], [], []) :- !.
group_products([product(_F, Q, L, H, W)|Ps], MaxHs, TopGap,
        [V|Vs], [task(1,1,2,GL,V)|Tasks], 
        [product(_F, Q, L, H, W)-grouped(GL, GW, GH, RL, RW, RH)|GPsTail], DPs) :-
    
    element(V, MaxHs, MaxH),
    bay(SL, _, SW, _),
    shelve(_THICK, _TG, LG, IG, _RG),
    rotate([L, H, W], [RL, RH, RW]), 
	NL in 1..Q, NH in 1..Q, NW in 1..Q,		
        
    NL * RL + IG + LG #=< SL ,
   	NH * RH + TopGap #=< MaxH,
	NW * RW #=< SW, ((NW+1) * RW #> SW #\/ (NH #= 1 #/\ NL #= 1 #/\ NW #= Q)),

    NL * NH * NW #>= Q,
	NL * NH * NW *3 #=< Q * 5, 
    
    GL #= NL * RL + IG,
    GH #= NH * RH,
    GW #= NW * RW, !, 
    group_products(Ps, MaxHs, TopGap, Vs, Tasks, GPsTail, DPs), !.

group_products([P|Ps], MaxH, TopGap, Vs, Tasks, GPs, [P|DPsTail]) :- 
    group_products(Ps, MaxH, TopGap, Vs, Tasks, GPs, DPsTail), !.


get_machines(MaxHs, Machines) :- get_machines(MaxHs, 1, Machines).
get_machines([], N, []).
get_machines([MaxH|MaxHs], N, [machine(N, MaxL)|Machines]) :-
    MaxL #>= 0, MaxL #=< 1200,
    N1 is N + 1,
    get_machines(MaxHs, N1, Machines).

/*
bay_shelves_limit([], AvalH, SumBay) :- SumBay #= 0.
bay_shelves_limit(MaxHs, AvalH, SumBay) :-
    length(Hs, 10),
    append(Hs, Ts, MaxHs),
    element(1, Hs, Hs1),
    numlist(600, 50, AvalH, _, MaxHDomain1),
	list_to_fdset([0|MaxHDomain1], FDS_MaxHDomain1),
    Hs1 in_set FDS_MaxHDomain1,
    sum(Hs, #=, SumBay),
    SumBay #=< AvalH,
    %SumBay in_set FDS_MaxHDomain1,
    %AvalH - SumBay #< SumBayNext #<=> B,
    %SumBayNext #> 0 #<=> B%, C #\= B,

   % sum(Hs, #=<, AvalH),
    bay_shelves_limit(Ts, AvalH, SumBayNext).
bay_shelves_limit(MaxHs, AvalH) :- bay_shelves_limit(MaxHs, AvalH, _).
*/
maxH_domain(AvalH, N, MaxHs) :-
    numlist(0, 50, AvalH, _, MaxHDomain),
	list_to_fdset(MaxHDomain, FDS_MaxHDomain),	
    NShelves is N // 10,
	length(MaxHs, NShelves),
    %sum(MaxHs, #=<, AvalH),
	( foreach(MaxH, MaxHs),
	  param(FDS_MaxHDomain)	
	do 
      MaxH in_set FDS_MaxHDomain).
      %,
    %bay_shelves_limit(MaxHs, AvalH).

chosen_constraints([], [], [], 0).
chosen_constraints([_-grouped(GL, GW, GH, _, _, _)|GPsTail], [V|Vs], [C|Cs], Sum) :-
	V #= 1 #<=> C,
	chosen_constraints(GPsTail, Vs, Cs, Sum1),
	Sum #= Sum1 + C * GL * GW * GH. 
	
split_chosen([], [], _, _, []).
split_chosen([P-G|GPsTail], [V|Vs], MaxHs, Bays, [(Bay, V, MaxH)-(P-G)|CPs]) :-
   % P = product(F, _Q, _L, _H, _W),
    element(V, Bays, Bay),
    element(V, MaxHs, MaxH),
%bosh([F|Fs], AvalH, N, [(N, NF, ShelveH)-CPs|CPsTail], DPs) :-
	split_chosen(GPsTail, Vs, MaxHs, Bays, CPs).

append_vars([], [], [], []).
append_vars([V|Vs], [_-grouped(GL, GW, GH, RL, RW, RH)|GPsTail], [V, GL, RL, GH, RH, GW, RW|AllVs], [GW, GH|Ls]) :-
	append_vars(Vs, GPsTail, AllVs, Ls).


get_tasks_bays([], [], []).
get_tasks_bays([MaxH|MaxHs], [task(1, 1, 2, MaxH, Bay)|TasksBays], [Bay|Bays]):-
    get_tasks_bays(MaxHs, TasksBays, Bays).

get_machines_bays(AvalH, N, N, []) :- !.
get_machines_bays(AvalH, N, MaxN, [machine(N, MaxH)|MachinesBays]) :-
    MaxH #>= 0, MaxH #=< AvalH,
    N < MaxN,
    N1 is N + 1,
    get_machines_bays(AvalH, N1, MaxN, MachinesBays).


bosh_labeling(Shelves, TimeOut, Vars, NBays, ShelvesV) :-
    labeling([minimize(NBays), time_out(TimeOut, _Flag), all],  Vars),
    print_time([NBays, Shelves, ShelvesV]).
	%labeling([time_out(120000, _Flag)],  Vars),
    %print([ShelvesV]).

bosh(Fs, res(CPs, DPs)) :- !,
    %max_available_height(AvalH),
    bosh(Fs, 3050, 1, CPs, DPs). % todo: 40 do primeiro topGAp

bosh([], _, _, [], []) :- !.
bosh([[]|Fs], AvalH, N, CPs, DPs) :- 
    bosh(Fs, AvalH, N, CPs, DPs).

% N is number of bays used by current family (acc), NT is current total number os bays used

bosh([F|Fs], AvalH, N, CPs, DPs) :- 
%bosh([F|Fs], AvalH, N, [(N, NF, ShelveH)-CPs|CPsTail], DPs) :-
    length(F, Size), F = [product(NF,_,_,_,_)|_], write([NF, AvalH, N, Size]), nl,    
    bay(MaxSL, _, _, MaxSH),
    shelve(THICK, TG, _LG, _IG, _RG),
    %(AvalH = 3000 -> 
	%  TopGap is TG % no shelve yet
	%; TopGap is THICK + TG), % top gap 40+15
	TopGap is THICK + TG, % top gap 40+15

    maxH_domain(MaxSH, Size, MaxHs),
    get_machines(MaxHs, 1, Machines),
    group_products(F, MaxHs, TopGap, Vs, Tasks, GPs, DPs1),
    cumulatives(Tasks, Machines, [bound(upper), task_intervals(true)]),

    get_tasks_bays(MaxHs, TasksBays, Bays),
    get_machines_bays(AvalH, 1, 60, MachinesBays),
    domain(Bays, 1, 60),
    cumulatives(TasksBays, MachinesBays, [bound(upper)]),

    %same_length(F, Vs),
    domain(Vs, 1, Size),
    
%    chosen_constraints(GPs, Vs, Cs, FilledSum),

%	MaxL #=< MaxSL - LG,
%	bosh-knapsack(Cs, GPs, MaxL),


    append_vars(Vs, GPs, Vs1, Ls),
	append(Bays, MaxHs, Vs2),
	append(Vs1, Vs2, Vs3),
	%append(Vs2, MaxHs, Vs3),
%	append(Vs3, [], Vars),
	append(Vs3, [Shelves, NBays], Vars),
   % print(Vars),

%	labeling([minimize(MaxH), time_out(3000, _Flag)], Vars),
    
    %Bays in 1..Size,
    sum(MaxHs, #=, Shelves),    
    %Bays * 10 #>= ShelvesV, 
    %(Bays-1) * 10 #=< ShelvesV, 
    maximum(ShelvesV, Vs),   
           
    %indomain(Bays),
    %print(Bays),
	%labeling([], Vs2),
	%findall(Bays, labeling([minimize(Bays), time_out(60000, _Flag), all],  Vars), AllBays),
    !,
    NBays #= 7, 
    maximum(NBays, Bays),
    
/*    findall(X, between(1,60, X),Is),
    (foreach(I, Is) do
        count(I, Bays, #=, XI),
        I #>= NBays #\/ XI #>=10
    ),
*/    
    %TimeOut is  Size*100,!,
    %NBays #> 1 #<=> NBayOne, 
    %Cost #= NBays*3050 + Shelves,
    Cost * 50 #= Shelves,
    labeling([minimize(Cost), time_out(60000, _Flag), all],  Vars),
    print(Vs),
	%findall([NBays, Shelves, ShelvesV], bosh_labeling(Shelves, TimeOut, Vars, NBays, ShelvesV), AllRes),
    %print([Bays, AllBays, ShelvesV, Shelves])    , !,
    %bosh([[]|Fs], AvalH, N, CPsTail, DPs).
    print_time([NBays, Shelves, ShelvesV]),
    print(MaxHs), print(Bays),
    print(Is),
    (foreach(I, Is) do
        count(I, Bays, #=, XI),
        print([I, XI])
    ),
    
    split_chosen(GPs, Vs, MaxHs, Bays, CPs).

    %print(CPs),
    
    %append(Vars, Ls, CPVars),
    %labeling([time_out(TimeOut, _Flag)],  CPVars),
	
    %labeling([], ), % finaliza labeling


%    print(AllRes), nl, 

	length(CPs, L1),
	%length(RPs, L2),
	%print([L1, CPs, L2]), nl, 
    
    ( L1 > 0, !; false ),
	%check_chosen(CPs, MaxH),
	%ShelveH is AvalH - 3000,
	%append(DPs1, RPs, F1).
	N1 is N + NBays,
	( bosh([DPs1|Fs], AvalH, N1, CPsTail, DPs2), DPs = DPs2; 
      DPs = F1, nl, nl, write(['\'Family shelve not complete, dropped pr oducts: \'', DPs]), nl, nl).

go(NI, N, FsFull) :- 
    NI > 0, NI1 is NI - 1,
    length(Pre, NI1), append(Pre, Ts, FsFull),
    %length(FsFull, L), LPos = L - NF, 
    length(Fs, N), append(Fs,_, Ts),
    !, fd_statistics, reset_timer, 
    %bosh(Fs, Res), 
    findall(Res, bosh(Fs, Res), ResAll),
    print_time('Time: '),
    fd_statistics, Res = res(CPs, DPs), nl, length(CPs, L) , print([L, _CPs,DPs]), fd_statistics.%, statistics.

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
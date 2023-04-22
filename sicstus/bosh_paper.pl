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

% domain
%product(F, Q, L, H, W)

% shelve(length, height, width, thickness, top gap, left gap, inter gap, right gap)
shelve(1200, 3000, 650, 40, 15, 10, 10, 10).
    
% rotate(+ProductDimensions, -RotateProdutcDimensions)
rotate([L, H, W], [RL, RH, RW]) :- 
	Vs = [IL, IW, IH],
	%domain(Vs, 1, 3),
    all_distinct(Vs),
    element(IL, [L,H,W], RL),
    element(IH, [L,H,W], RH),
    element(IW, [L,H,W], RW).

% group_product(+Product, +HMax, -GroupedProduct)
group_products([], _, _, [], []) :- !.
group_products([product(_F, Q, L, H, W)|Ps], MaxH, TopGap, 
    [product(_F, Q, L, H, W)-grouped(GL, GW, GH, RL, RW, RH)|GPsTail], DPs) :-
    shelve(SL, _SH, SW, _THICK, _TG, _LG, IG, _RG),
    rotate([L, H, W], [RL, RH, RW]), 
	NL in 1..Q, NH in 1..Q, NW in 1..Q,		

    NL * RL + IG #=< SL,
   	NH * RH + TopGap #=< MaxH,
	NW * RW #=< SW, ((NW+1) * RW #> SW #\/ (NH #= 1 #/\ NL #= 1 #/\ NW #= Q)),

    NL * NH * NW #>= Q,
	NL * NH * NW #=< Q * 2, 
    GL #= NL * RL + IG,
    GH #= NH * RH,
    GW #= NW * RW, 
    group_products(Ps, MaxH, TopGap, GPsTail, DPs), !.

group_products([P|Ps], MaxH, TopGap, GPs, [P|DPsTail]) :-
    group_products(Ps, MaxH, TopGap, GPs, DPsTail).


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
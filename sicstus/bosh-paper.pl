:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
%:- use_module('../data/products.pl').

% 
rotate([L,W,H], [RL,RW,RH]) :-    
    element(IL, [L,W,H], RL),
    element(IW, [L,W,H], RW),
    element(IH, [L,W,H], RH),
    all_distinct([IL,IW,IH]).

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
		rotate([1,2,3], [RL,RW,RH]),
        permutation([1,2,3], [RL,RW,RH]).

:- end_tests(bosh).
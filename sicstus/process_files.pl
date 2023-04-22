:- use_module(library(system)).
:- use_module(library(samsort)).
:- consult('file_utils.pl').


read_products(Ps) :- 
    read_file('../data/products.txt', Ps).

same_family([F|_],[F|_]).

families(Fs) :- 
    read_products(Ps), 
    group(same_family, Ps, Fs).  
families_sorted(FsSorted) :- 
    families(Fs), 
    maplist(samsort(by_volume), Fs, FsSorted).

by_volume([_F, Q1, L1, W1, H1],[_F, Q2, L2, W2, H2]) :-
	Q1 * L1 * W1 * H1 > Q2 * L2 * W2 * H2. % from largest to smallers products

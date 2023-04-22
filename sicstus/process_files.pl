:- use_module(library(system)).
:- use_module(library(samsort)).
:- consult('file_utils.pl').


to_product([], []).
to_product([[F, Q, L, W, H]|Ps], [product(F, Q, L, W, H)|ProdTail]) :-
    to_product(Ps, ProdTail).

read_products(Ps) :- 
    read_file('../data/products.txt', Ls),
    to_product(Ls, Ps).

same_family(product(F,_,_,_,_), product(F,_,_,_,_)).

families(Fs) :- 
    read_products(Ps), 
    group(same_family, Ps, Fs).  

families_sorted(FsSorted) :- 
    families(Fs), 
    maplist(samsort(by_volume), Fs, FsSorted).

by_volume(product(_, Q1, L1, W1, H1), product(_F, Q2, L2, W2, H2)) :-
	Q1 * L1 * W1 * H1 > Q2 * L2 * W2 * H2. % from largest to smallers products
volume([_F, Q, L, W, H], V) :- V is Q * L * W * H.


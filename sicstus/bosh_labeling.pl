:- use_module(library(clpfd)).

paper_weaving_vars([], [], []). 
paper_weaving_vars([C|Cs], [GL, GW, GH|Gs], [GL, GW, GH, C|AllVs]) :-
	paper_weaving_vars(Cs, Gs, AllVs).

paper_vars_orders(1, Cs, Gs, MaxH, _AntiCs, Vars) :-
    append(Gs, Cs, Vs1),
    append([MaxH], Vs1, Vars).

paper_vars_orders(2, Cs, Gs, MaxH, _AntiCs, Vars) :-
    append(Gs, Cs, Vs1),
    append(Vs1, [MaxH], Vars).

% weaving AntiCs and Gs (BEST, with maxH as Cost funciont)
paper_vars_orders(3, _Cs, Gs, MaxH, AntiCs, Vars) :-
    weaving_vars(AntiCs, Gs, Vs1),
	append(Vs1, [MaxH], Vars).


cumulative_weaving_vars([], [], []). 
cumulative_weaving_vars([Shelf|Shelves],[_-grouped(GL, GW, GH, RL, RW, RH)|GPs], [GL, RL, GH, RH, GW, RW, Shelf|WeavingVars]) :-
	cumulative_weaving_vars(Shelves, GPs, WeavingVars).

cumulative_vars_orders(1, Shelves, GPs, MaxHs, Bays, Vars) :-
    cumulative_weaving_vars(Shelves, GPs, Vs1),
	append(Vs1, MaxHs, Vs2),
	append(Vs2, Bays, Vars).


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

bosh_labeling(OptNumber, Vars, Cost, Timeout) :-
    labeling_options(OptNumber, Opt),
    append(Vars, [Cost], VarsAll),
	labeling([minimize(Cost), time_out(Timeout, _Flag)|Opt], VarsAll).



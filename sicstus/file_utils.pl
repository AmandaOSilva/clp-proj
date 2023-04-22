:- use_module(library(lists)).

read_number(X) :- 	read_number(X, true, 0).
read_number(X, Skipline) :- read_number(X, Skipline, 0).

read_number(Acc, true, Acc) :- peek_code(10), !, skip_line.
read_number(Acc, false, Acc) :- peek_code(10), !.
read_number(Acc, _, Acc) :- peek_code(32), !,  get_code(_).
read_number(X, Skipline, Acc) :- get_code(C),  Acc1 is 10*Acc+(C-48), read_number(X, Skipline, Acc1).

read_file_line(Line) :- read_file_line(Line, []).
read_file_line(Line, Acc) :- peek_code(10), skip_line, !, reverse(Acc, Line).
read_file_line(Line, Acc) :- read_number(N, false), !, read_file_line(Line, [N|Acc]). 

read_file_lines(Matrix) :- read_file_lines(Matrix, []).
read_file_lines(Matrix, Acc) :- peek_code(10), skip_line, !, reverse(Acc, Matrix).
read_file_lines(Matrix, Acc) :- read_file_line(Line), read_file_lines(Matrix,[Line|Acc]).

read_file(File, Matrix) :- 
	see(File), 
	%read_number(N),
	read_file_lines(Matrix),
	close(File).
	

write_file_line([]) :- !.	
write_file_line([H|[]]) :-
	write(H), !.
write_file_line([H|Ts]) :-
	write(H), write(' '), 
	write_file_line(Ts).	

write_file_lines([]) :- !.
write_file_lines([H|Ts]) :-
	write_file_line(H), nl,
	write_file_lines(Ts).	
	

write_file(File, Matrix) :- 
	tell(File),
	length(Matrix, L),
	write(L), nl,
	write_file_lines(Matrix),
	close(File).
	
write_matrix_in_file(File, Name, Matrix) :- 
	tell(File),
	write(Name), write(' = '), write(Matrix), nl,
	close(File).
	

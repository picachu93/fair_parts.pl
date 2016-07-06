read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).

read_and_return(File, N, Weights) :-
    open(File, read, Stream),
    read_line(Stream, [M, N]),
    read_line(Stream, Weights),
    length(Weights, L),
    ( L =:= M -> close(Stream)  %% just a check for for sanity
    ; format("Error: expected to read ~d weights but found ~d", [M, L]),
      close(Stream), fail
    ).

take(0, _, []) :- !.
 take(N, [H|TA], [H|TB]) :-
 	N > 0,
 	N2 is N - 1,
 	take(N2, TA, TB).

drop(0,LastElements,LastElements) :- !.
 drop(N,[_|Tail],LastElements) :-
 	N > 0,
 	N1 is N  - 1,
 	drop(N1,Tail,LastElements). 

my_finder([],_,_,Parts,Parts).
my_finder([H|T],Sum,Li,C,Parts) :- S1 is Sum + H,
	(S1 > Li -> C1 is C + 1,my_finder([H|T],0,Li,C1,Parts);
	 not(S1 > Li) -> my_finder(T,S1,Li,C,Parts)).

my_finder2(_,_,0,Li,_,_,Li).
my_finder2(W,K,Diff,Li,L,R,Res) :- my_finder(W,0,Li,1,Parts),
	(Parts > K -> L1 is Li + 1,Diff1 is R - L1,
		Li1 is ((L1 + R) div 2),my_finder2(W,K,Diff1,Li1,L1,R,Res);
	 not(Parts > K) -> R1 = Li,Diff1 is R1- L,
		Li1 is ((L + R1) div 2),my_finder2(W,K,Diff1,Li1,L,R1,Res)).

my_printer([],Lout,_,_,Tl,[Lout|Tl]).
my_printer([H|T],Lout,Sum,Li,Tl,Res) :- S1 is H + Sum,
	(S1 > Li -> my_printer([H|T],[],0,Li,[Lout|Tl],Res);
	 not(S1 > Li) -> my_printer(T,[H|Lout],S1,Li,Tl,Res) ).


fair(File,K,R1,R2,L1,L2,L,R,Li,Parts,Res,Parts1,Res1) :-
	read_and_return(File,K,W),sum_list(W,R1),max_list(W,L1),
	take(K-1,W,Part1),drop(K-1,W,Part2),sum_list(Part2,S),max_list(Part1,M),
	max_list([S,M],R2),L2 is R1/K,max_list([L1,L2],L),min_list([R1,R2],R),Li is (L + R)/2
	,my_finder(W,0,Li,1,Parts),Diff is R - L,my_finder2(W,K,Diff,Li,L,R,Res),
	my_finder(W,0,Res,1,Parts1),my_printer(W,[],0,Res,[],Res1).



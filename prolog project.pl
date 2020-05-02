length1(N, List) :- length(List, N).

gen_mat(C, R, M) :-
    length1(R, M),
    maplist(length1(C), M).

grid_build(N,M):-
	gen_mat(N,N,M).

grid_gen(N,Matrix):-
					grid_build(N,Matrix),
					maplist(maplist(seq(1,N)),Matrix).
					
					
seq(X,Y,X):- X=<Y.
seq(X,Y,Z):- X<Y,
			X1 is X+1,
			seq(X1,Y,Z).

num_gen(F,L,R):-
				setof(X,seq(F,L,X),R).
fill([H|T],L):-
				fillh([H|T],[],L).

fillh([],ACC,ACC).
fillh([H|T],ACC,L):-
				append(H,ACC,L1),
				fillh(T,L1,L).
check(MAX,L):-
			num_gen(1,MAX,R),
			subset(R,L).

check_num_grid([]).
check_num_grid([H|T]):-
			length([H|T],N),
			fill([H|T],L),
			get_max([H|T],Max),
			Max =< N,
			check(Max,L).

maxinlist([],0).
    maxinlist([A],A).
    maxinlist([A,B|L],Max) :-
        maximum(A,B,Max1),
        maxinlist([Max1|L],Max).


get_max([],0).
get_max([H|T],Max):-
			maxinlist(H,X1),
			get_max(T,X2),
			maximum(X1,X2,Max).


maximum(X,Y,X):- X>=Y.
maximum(X,Y,Y):- X<Y.

row_col_match(Matrix):-
				acceptable_distribution(Matrix),
				trans(Matrix,MT),
				row_col_match_h(Matrix,MT).
				
row_col_match_h([],_).
row_col_match_h([H|T],MT):-
				member(H,MT),
				row_col_match_h(T,MT).
				
acceptable_distribution(L):-
				trans(L,MT),
				acceptable_distribution_h(L,MT).
				
acceptable_distribution_h([],[]).
acceptable_distribution_h([H|T],[H1|T1]):-
				H \= H1 ,
				acceptable_distribution_h(T,T1).


trans(M,M1):- 
			trans_h(M,M1).

trans_h([], []).
trans_h([H|T], T2) :-
    trans_h(H, [H|T], T2).

trans_h([], _, []).
trans_h([_|A], B, [T1|T2]) :-
        helper(B, T1, C),
        trans_h(A, C, T2).

helper([], [], []).
helper([[H|T]|D], [H|T1], [T|E]) :-
        helper(D, T1, E).
		
distinct_rows([]).
distinct_rows([H|[]]).
distinct_rows([H1,H2|T]):-
			H1 \= H2,
			distinct_rows([H1|T]),
			distinct_rows([H2|T]).

distinct_columns(M):-
			trans(M,M1),
			distinct_rows(M1).

acceptable_permutation(L,R):-
			permutation(L,R1),
			check_permutation(L,R1,R).

check_permutation([],[],[]).
check_permutation([H|T],[H1|T1],[H1|T1]):-
			H \= H1 ,
			check_permutation(T,T1,T1).

helsinki(N,L):-
			grid_build(N,L),
			grid_gen(N,L),
			distinct_rows(L),
			distinct_columns(L),
			acceptable_distribution(L),
			check_num_grid(L),	
			row_col_match(L).
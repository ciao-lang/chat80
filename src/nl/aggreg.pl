:- module(aggreg, [aggregate/3, one_of/2, ratio/3, card/2],
    [assertions, isomodes]).

:- use_module(library(lists), [length/2]).

:- use_module(chat80(db/world0), [ratio/4]).

:- include(chat80(chatops)). 

% :- pred aggregate(+,+,?).
:- pred aggregate/3 : nonvar * nonvar * var => ground * ground * ground.
aggregate(Fn,Set,Val) :-
   dimensioned(Set), !,
   u_aggr(Fn,Set,Val).
aggregate(Fn,Set,Val) :-
   i_aggr(Fn,Set,Val).

% :- pred i_aggr(+,+,?).
:- pred i_aggr/3 : nonvar * nonvar * var => ground * ground * ground.
i_aggr(average,Set,Val) :-
   i_total(Set,T),
   length(Set,N),
   Val is T//N.
i_aggr(total,Set,Val) :-
   i_total(Set,Val).
i_aggr(max,Set,Val) :-
   i_maxs(Set,List),
   one_of(List,Val).
i_aggr(min,Set,Val) :-
   i_mins(Set,List),
   one_of(List,Val).
i_aggr(maximum,[V0:O|S],V) :-
   i_maxs0(S,V0,[O],_,V).
i_aggr(minimum,[V0:O|S],V) :-
   i_mins0(S,V0,[O],_,V).

% :- pred u_aggr(+,+,?).
:- pred u_aggr(A,B,C) : (nonvar(A), nonvar(B)) => ground * ground * ground.
u_aggr(average,Set,V--U) :-
   u_total(Set,T--U),
   length(Set,N),
   V is T//N.
u_aggr(total,Set,Val) :-
   u_total(Set,Val).
u_aggr(max,Set,Val) :-
   u_maxs(Set,List),
   one_of(List,Val).
u_aggr(min,Set,Val) :-
   u_mins(Set,List),
   one_of(List,Val).
u_aggr(maximum,[V0:O|S],V) :-
   u_maxs0(S,V0,[O],_,V).
u_aggr(minimum,[V0:O|S],V) :-
   u_mins0(S,V0,[O],_,V).

% :- pred i_total(+,?).
:- pred i_total(A,B) : nonvar(A) => nonvar * ground.
i_total([],0).
i_total([V:_|R],T) :-
   i_total(R,T0),
   T is V+T0.

% :- pred i_maxs(+,?).
:- pred i_maxs(A,B) : nonvar(A) => ground * ground.
i_maxs([V:X|Set],List) :-
   i_maxs0(Set,V,[X],List,_).

% :- pred i_maxs0(+,+,+,?,?).
:- pred i_maxs0(L1,V1,L2,L3,V2)
   : (nonvar(L1), nonvar(V1), nonvar(L2))
   => ground * ground * ground * ground * ground.
i_maxs0([],V,L,L,V).
i_maxs0([V0:X|R],V0,L0,L,V) :- !,
   i_maxs0(R,V0,[X|L0],L,V).
i_maxs0([U:X|R],V,_,L,W) :-
   U>V, !,
   i_maxs0(R,U,[X],L,W).
i_maxs0([_|R],V,L0,L,W) :-
   i_maxs0(R,V,L0,L,W).

% :- pred i_mins(+,-).
:- pred i_mins/2 : nonvar * var => ground * ground.
i_mins([V:X|Set],List) :-
   i_mins0(Set,V,[X],List,_).

% :- pred i_mins0(+,+,+,?,?).
:- pred i_mins0(L1,V1,L2,L3,V2)
   : (nonvar(L1), nonvar(V1), nonvar(L2))
   => ground * ground * ground * ground * ground.
i_mins0([],V,L,L,V).
i_mins0([V:X|R],V,L0,L,W) :- !,
   i_mins0(R,V,[X|L0],L,W).
i_mins0([U:X|R],V,_,L,W) :-
   U<V, !,
   i_mins0(R,U,[X],L,W).
i_mins0([_|R],V,L0,L,W) :-
   i_mins0(R,V,L0,L,W).

% :- pred u_total(+,?).
:- pred u_total(L,B) : nonvar(L) => list * nonvar.
u_total([],0--_U).
u_total([V:_|R],T) :-
   u_total(R,T0),
   u_sum(T0,V,T).

% :- pred u_sum(+,+,?).
:- pred u_sum(A,B,C) : (nonvar(A), nonvar(B))
   => ground * ground * ground.
u_sum(X--U,Y--U,Z--U) :- !,
   Z is X+Y.
u_sum(X--U,Y--U1,Z--U) :-
   ratio(U,U1,M,M1), M>M1, !,
   Z is X + (Y*M1)//M.
u_sum(X--U1,Y--U,Z--U) :-
   ratio(U,U1,M,M1), M>M1, !,
   Z is (X*M1)//M + Y.

% :- pred u_maxs(+,?).
:- pred u_maxs(A,B) : nonvar(A) => ground * ground.
u_maxs([V:X|Set],List) :-
   u_maxs0(Set,V,[X],List,_).

% :- pred u_maxs0(+,+,+,?,?).
:- pred u_maxs0(L1,V1,L2,L3,V2)
   : (nonvar(L1), nonvar(V1), nonvar(L2))
   => ground * ground * ground * ground * ground.
u_maxs0([],V,L,L,V).
u_maxs0([V0:X|R],V0,L0,L,V) :- !,
   u_maxs0(R,V0,[X|L0],L,V).
u_maxs0([U:X|R],V,_,L,W) :-
   u_lt(V,U), !,
   u_maxs0(R,U,[X],L,W).
u_maxs0([_|R],V,L0,L,W) :-
   u_maxs0(R,V,L0,L,W).

% :- pred u_mins(+,?).
:- pred u_mins(A,B) : nonvar(A) => ground * ground.
u_mins([V:X|Set],List) :-
   u_mins0(Set,V,[X],List,_).

% :- pred u_mins0(+,+,+,?,?).
:- pred u_mins0(L1,V1,L2,L3,V2)
   : (nonvar(L1), nonvar(V1), nonvar(L2))
   => ground * ground * ground * ground * ground.
u_mins0([],V,L,L,V).
u_mins0([V:X|R],V,L0,L,W) :- !,
   u_mins0(R,V,[X|L0],L,W).
u_mins0([U:X|R],V,_,L,W) :-
   u_lt(U,V), !,
   u_mins0(R,U,[X],L,W).
u_mins0([_|R],V,L0,L,W) :-
   u_mins0(R,V,L0,L,W).

% :- pred u_lt(+,+).
:- pred u_lt/2 : nonvar * nonvar => ground * ground.
u_lt(A,X--U) :-
   Y is -X,
   u_sum(A,Y--U,Z--_),
   Z<0.

% :- pred dimensioned(+).
:- pred dimensioned/1 : nonvar.
dimensioned([(_--_):_|_]).

% :- pred one_of(+,?).
:- pred one_of(L,X) : nonvar(L) => member(X,L).
one_of([X|_],X).
one_of([_|R],X) :-
   one_of(R,X).

ratio(N,M,R) :- R is (N*100)//M.

card(S,N) :- length(S,N).

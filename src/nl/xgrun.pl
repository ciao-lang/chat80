:- module(xgrun, [terminal/5, virtual/3], [assertions]).

% :- pred terminal(?,+,?,+,?).
:- pred terminal(A,B,C,D,E) : (nonvar(B), nonvar(C)).
terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

% :- pred gap(+).
:- pred gap/1 : nonvar.
gap(x(gap,_,_,_)).
gap([]).

% :- pred virtual(+,+,?).
:- pred virtual(A,B,C) : (nonvar(A), nonvar(B)).
virtual(NT,x(_,nonterminal,NT,X),X).

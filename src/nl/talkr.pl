:- module(talkr, [answer/2, dlst/3, satisfy/1, write_tree/1],[assertions, isomodes]).
/* Simplifying and executing the logical form of a NL query. */
% Changed answer to return output in an argument. M.H.

:- use_module(engine(io_basic)).
:- use_module(library(aggregates), [setof/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(write), [numbervars/3, write/1]).

:- use_module(chat80(db/world0), [database/1]).

:- include(chat80(chatops)).

% :- pred write_tree(+).
:- pred write_tree/1 : nonvar => ground.
write_tree(T):-
   numbervars(T,0,_),
   wt(T,0),
   fail.
write_tree(_).

% :- pred wt(+,+).
:- pred wt/2 : ground * nonvar => ground * ground.
wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt((P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put("&"), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put("{"), tab(1), wt(P,L1), tab(1), put("}").
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

% :- pred header(+).
:- pred header/1 : nonvar => ground.
header([]).
header([X|H]) :- write(X), tab(1), header(H).

% :- pred decomp(+,-,-).
:- pred decomp/3 : nonvar * var * var => ground * ground * ground.
decomp(setof(X,P,S),[S,=,setof,X],P).  
decomp(\+(P),[\+],P) :- complex(P).
decomp(numberof(X,P,N),[N,=,numberof,X],P).
decomp(X^P,[exists,X|XX],P1) :- othervars(P,XX,P1).

% :- pred othervars(+,-,-).
:- pred othervars/3 : nonvar * var * var => ground * ground * ground.
othervars(X^P,[X|XX],P1) :- !, othervars(P,XX,P1).
othervars(P,[],P).

% :- pred complex(+).
:- pred complex/1 : var => ground.
complex((_,_)).
complex({_}).
complex(setof(_,_,_)).
complex(numberof(_,_,_)).
complex(_^_).
complex(\+P) :- complex(P).

% Query execution.
respond([],"Nothing satisfies your question.").
respond([A|L],Ans) :- 
    respond_([A|L],Ans,[0'.]).
%% respond([]) :- write('Nothing satisfies your question.'), nl.
%% respond([A|L]) :- reply(A), replies(L).

respond_([A|L],Ans,Ansrs):-
    reply(A,Ans,Ans0), 
    replies(L,Ans0,Ansrs).

% :- pred answer(+,?).
:- pred answer(A,B) : nonvar(A) => ground * ground.
answer((answer([]):-E),A) :- !, holds(E,B), yesno(B,A).
answer((answer([X]):-E),A) :- !, seto(X,E,S), respond(S,A).
answer((answer(X):-E),A) :- seto(X,E,S), respond(S,A).

% :- pred seto(?,+,-).
:- pred seto/3 : ground * ground * var => ground * ground * ground.
seto(X,E,S) :- setof(X,satisfy(E),S), !.
seto(_X,_E,[]).

% :- pred holds(+,?).
:- pred holds(A,B) : nonvar(A) => ground * ground.
holds(E,true) :- satisfy(E), !.
holds(_E,false).

% :- pred yesno(+,?).
:- pred yesno(A,B) : nonvar(A) => ground * ground.
yesno(true,"Yes."). 
yesno(false,"No."). 
%% yesno(true) :- write('Yes.').
%% yesno(false) :- write('No.').

% :- pred replies(+,?,?).
:- pred replies(A,B,C) : nonvar(A) => ground * ground * ground.
replies([],R,R).
replies([A],Ans,Ansrs) :- 
    dlst(" and ",Ans,Ans1),
    reply(A,Ans1,Ansrs).
replies([A|X],Ans,Ansrs) :- 
    dlst(", ",Ans,Q1),
    reply(A,Q1,Q2), 
    replies(X,Q2,Ansrs).
%% replies([]) :- write('.').
%% replies([A]) :- write(' and '), reply(A), write('.').
%% replies([A|X]) :- write(', '), reply(A), replies(X).

% :- pred reply(+,?,?).
:- pred reply(A,B,C) : nonvar(A) => ground * ground * ground.
reply(N--U,A,As) :- 
    !, 
    name(N,Ns),
    dlst(Ns,A,[0' |Q1]),
    name(U,Us),
    dlst(Us,Q1,As).
reply(X,A,As) :- 
    atomic(X), !,
    name(X,Xs),
    dlst(Xs,A,As).
% PBC: What are the capitals of the countries bordering the Baltic?
% returns a list of terms [...]:[...]
reply([X|Xs],A,As) :- !,
    respond_([X|Xs],A,As).
reply(X:Y,A,As) :- !,
    dlst("for ",A,A0),
    reply(X,A0,[C1,C2|A1]),
    reply(Y,A1,As),
    name(': ',[C1,C2]).
reply(X,A,A) :- throw(no_parsing(X)).
%% reply(N--U) :- !, write(N), write(' '), write(U).
%% reply(X) :- write(X).

% :- pred satisfy(+).
:- pred satisfy/1 : nonvar => ground.
satisfy((P,Q)) :- !, satisfy(P), satisfy(Q).
satisfy({P}) :- !, satisfy(P), !.
satisfy(_X^P) :- !, satisfy(P).
satisfy(\+P) :- satisfy(P), !, fail.
satisfy(\+_P) :- !.
satisfy(numberof(X,P,N)) :- !, setof(X,satisfy(P),S), length(S,N).
satisfy(setof(X,P,S)) :- !, setof(X,satisfy(P),S).
satisfy(+P) :- exceptionto(P), !, fail.
satisfy(+_P) :- !.
satisfy(X<Y) :- !, X<Y.
satisfy(X=<Y) :- !, X=<Y.
satisfy(X>=Y) :- !, X>=Y.
satisfy(X>Y) :- !, X>Y.
satisfy(P) :- database(P).

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception(P1).

exception(P) :- database(P), !, fail.
exception(_P).

% :- pred pickargs(+,+,+).
:- pred pickargs/3 : nonvar * nonvar * nonvar => ground * ground * ground.
pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

% :- pred pick(+,?).
:- pred pick(A,B) : nonvar(A) => ground * ground.
pick([X|_S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).

dlst([X|Y],[X|R],E) :-
    !,
    dlst(Y,R,E).
dlst([],E,E).

%% PBC
put([C]):- !, put_code(C).
put(C):- put_code(C).

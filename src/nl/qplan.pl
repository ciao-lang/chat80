:- module(qplan, [qplan/2], [assertions]).
% qplan - supplies the control information (ie. sequencing and cuts) needed
%         for efficient execution of a query.

:- use_module(library(sort), [keysort/2]).
:- use_module(library(write), [numbervars/3]).

:- use_module(chat80(db/ndtabl), [nd/3, nd/4, nd/5]).

:- set_prolog_flag(multi_arity_warnings,off).

% :- pred qplan(+,-).
:- pred qplan/2 : nonvar * var.
qplan((P:-Q),(P1:-Q1)) :- qplan(P,Q,P1,Q1), !.
qplan(P,P).

% :- pred qplan(+,+,-,-).
:- pred qplan/4 : nonvar * nonvar * var * var.
qplan(X0,P0,X,P) :-
   numbervars(X0,0,I), variables(X0,0,Vg),
   numbervars(P0,I,N),
   mark(P0,L,0,Vl),
   schedule(L,Vg,P1),
   quantificate(Vl,0,P1,P2),
   functor(VA,$,N),
   variablise(X0,VA,X),
   variablise(P2,VA,P).

% :- pred mark(+,-,+,-).
:- pred mark/4 : nonvar * var * nonvar * var.
mark(X^P,L,Q0,Q) :- !, variables(X,Q0,Q1), mark(P,L,Q1,Q).
mark((P1,P2),L,Q0,Q) :- !,
   mark(P1,L1,Q0,Q1),
   mark(P2,L2,Q1,Q),
   recombine(L1,L2,L).
mark(\+P,L,Q,Q) :- !, mark(P,L0,0,Vl), negate(L0,Vl,L).
mark(SQ,[m(V,C,SQ1)],Q0,Q0) :- subquery(SQ,SQ1,X,P,N,Q), !,
   mark(P,L,0,Vl),
   L=[Q],   % Too bad about the general case!
   marked(Q,Vq,C0,_),
   variables(X,Vl,Vlx),
   setminus(Vq,Vlx,V0),
   setofcost(V0,C0,C),
   variables(N,V0,V).
mark(P,[m(V,C,P)],Q,Q) :-
   variables(P,0,V),
   cost(P,V,C).

% :- pred subquery(+,-,?,?,?,?).
:- pred subquery(A,B,C,D,E,F) : (nonvar(A), var(B)).
subquery(setof(X,P,S),setof(X,Q,S),X,P,S,Q).
subquery(numberof(X,P,N),numberof(X,Q,N),X,P,N,Q).

% :- pred negate(+,+,-).
:- pred negate/3 : nonvar * nonvar * var.
negate([],_,[]).
negate([P|L],Vl,[m(Vg,C,\+P)|L1]) :-
   freevars(P,V),
   setminus(V,Vl,Vg),
   negationcost(Vg,C),
   negate(L,Vl,L1).

% :- pred negationcost(+,-).
:- pred negationcost(A,B) : nonvar * var => ground(B).
negationcost(0,0) :- !.
negationcost(_V,1000).

% :- pred setofcost(+,+,-).
:- pred setofcost(A,B,C) : nonvar * nonvar * var => nonvar(C).
setofcost(0,_,0) :- !.
setofcost(_,C,C).

% :- pred variables(+,+,-).
:- pred variables/3 : nonvar * nonvar * var.
variables('$VAR'(N),V0,V) :- !, setplusitem(V0,N,V).
variables(T,V,V) :- atomic(T), !.
variables(T,V0,V) :- functor(T,_,N), variables(N,T,V0,V).

% :- pred variables(+,+,+,-).
:- pred variables/4 : nonvar * nonvar * nonvar * var.
variables(0,_,V,V) :- !.
variables(N,T,V0,V) :- N1 is N-1,
   arg(N,T,X),
   variables(X,V0,V1),
   variables(N1,T,V1,V).

% :- pred quantificate(+,+,?,-).
:- pred quantificate(A,B,C,D) : (nonvar(A), nonvar(B), var(D)).
quantificate(W-V,N,P0,P) :- !, N1 is N+18,
   quantificate(V,N,P1,P),
   quantificate(W,N1,P0,P1).
quantificate(0,_,P,P) :- !.
quantificate(V,N,P0,'$VAR'(Nr)^P) :-
   Vr is V /\ -(V),     % rightmost bit
   log2(Vr,I),
   Nr is N+I,
   N1 is Nr+1,
   V1 is V >> (I+1),
   quantificate(V1,N1,P0,P).

% :- pred log2(+,-).
:- pred log2/2 : nonvar * var => ground * ground.
log2(1,0) :- !.
log2(2,1) :- !.
log2(4,2) :- !.
log2(8,3) :- !.
log2(N,I) :- N1 is N>>4, N1=\=0, log2(N1,I1), I is I1+4.

% :- pred schedule(+,+,-).
:- pred schedule/3 : nonvar * nonvar * var.
schedule([P],Vg,Q) :- !, schedule1(P,Vg,Q).
schedule([P1|P2],Vg,(Q1,Q2)) :- !, schedule1(P1,Vg,Q1), schedule(P2,Vg,Q2).

% :- pred schedule1(+,+,-).
:- pred schedule1/3 : nonvar * nonvar * var.
schedule1(m(V,C,P),Vg,Q) :-
   maybe_cut(V,Vg,Q0,Q),
   plan(P,V,C,Vg,Q0).

% :- pred maybe_cut(+,+,?,-).
:- pred maybe_cut(A,B,C,D) : (nonvar(A), nonvar(B), var(D)).
maybe_cut(V,Vg,P,{P}) :- disjoint(V,Vg), !.
maybe_cut(_V,_Vg,P,P).

% :- pred plan(+,+,+,+,-).
:- pred plan/5 : nonvar * nonvar * nonvar * nonvar * var.
plan(\+P,Vg,_,_,\+Q) :- !, Vg = 0,
   marked(P,V,C,P1),
   plan(P1,V,C,Vg,Q1),
   quantificate(V,0,Q1,Q).
plan(SQ,Vg,_,_,SQ1) :- subquery(SQ,SQ1,X,P,_,Q), !,
   marked(P,V,C,P1),
   variables(X,Vg,Vgx),
   setminus(V,Vgx,Vl),
   quantificate(Vl,0,Q1,Q),
   plan(P1,V,C,Vgx,Q1).
plan(P,V,C,Vg,(Q,R)) :- is_conjunction(P), !,
   best_goal(P,V,C,P0,V0,PP),
   plan(P0,V0,C,Vg,Q),
   instantiate(PP,V0,L),
   add_keys(L,L1),
   keysort(L1,L2),
   strip_keys(L2,L3),
   schedule(L3,Vg,R).
plan(P,_,_,_,P).

% :- pred is_conjunction(+).
:- pred is_conjunction/1 : nonvar.
is_conjunction((_,_)).

% :- pred marked(+,?,?,?).
:- pred marked(A,B,C,D) : nonvar(A).
marked(m(V,C,P),V,C,P).

% :- pred freevars(+,?).
:- pred freevars(A,B) : nonvar(A).
freevars(m(V,_,_),V).

% :- pred best_goal(+,+,+,?,?,-).
:- pred best_goal(A,B,C,D,E,F) : (nonvar(A), nonvar(B), nonvar(C), var(F)).
best_goal((P1,P2),V,C,P0,V0,m(V,C,Q)) :- !,
   ( marked(P1,Va,C,Pa), Q=(Pb,P2) ; marked(P2,Va,C,Pa), Q=(P1,Pb) ), !,
   best_goal(Pa,Va,C,P0,V0,Pb).
best_goal(P,V,_C,P,V,true).

% :- pred instantiate(+,+,-).
:- pred instantiate/3 : nonvar * nonvar * var.
instantiate(true,_,[]) :- !.
instantiate(P,Vi,[P]) :- freevars(P,V), disjoint(V,Vi), !.
instantiate(m(V,_,P),Vi,L) :- instantiate0(P,V,Vi,L).

% :- pred instantiate0(+,+,-,?).
:- pred instantiate0(A,B,C,D) : (nonvar(A), nonvar(B)).
instantiate0((P1,P2),_,Vi,L) :-
   instantiate(P1,Vi,L1),
   instantiate(P2,Vi,L2),
   recombine(L1,L2,L).
instantiate0(\+P,V,Vi,L) :- !,
   instantiate(P,Vi,L0),
   freevars(P,Vf), setminus(Vf,V,Vl),
   negate(L0,Vl,L).
instantiate0(SQ,Vg,Vi,[m(V,C,SQ1)]) :- subquery(SQ,SQ1,X,P,_,Q), !,
   instantiate(P,Vi,L),
   L=[Q],   % Too bad about the general case!
   marked(Q,_Vq,C0,_),
   setminus(Vg,Vi,V),
   variables(X,0,Vx),
   setminus(V,Vx,V0),
   setofcost(V0,C0,C).
instantiate0(P,V,Vi,[m(V1,C,P)]) :-
   setminus(V,Vi,V1),
   cost(P,V1,C).

% :- pred recombine(+,+,-).
:- pred recombine/3 : nonvar * nonvar * var.
recombine(L,[],L) :- !.
recombine([],L,L).
recombine([P1|L1],[P2|L2],L) :-
   marked(P1,V1,C1,_), nonempty(V1),
   incorporate(P1,V1,C1,P2,L2,L3), !,
   recombine(L1,L3,L).
recombine([P|L1],L2,[P|L]) :- recombine(L1,L2,L).

% :- pred incorporate(+,+,+,+,+,-).
:- pred incorporate/6 : nonvar * nonvar * nonvar * nonvar * nonvar * var.
incorporate(P0,V0,C0,P1,L1,L) :-
   marked(P1,V1,C1,_),
   intersect(V0,V1), !,
   setplus(V0,V1,V),
   minimum(C0,C1,C),
   incorporate0(m(V,C,(P0,P1)),V,C,L1,L).
incorporate(P0,V0,C0,P1,[P2|L1],[P1|L]) :- incorporate(P0,V0,C0,P2,L1,L).

% :- pred incorporate0(+,+,+,+,-).
:- pred incorporate0/5 : nonvar * nonvar * nonvar * nonvar * var.
incorporate0(P0,V0,C0,[P1|L1],L) :- incorporate(P0,V0,C0,P1,L1,L), !.
incorporate0(P,_,_,L,[P|L]).

% :- pred minimum(+,+,-).
:- pred minimum(A,B,C) : nonvar * nonvar * var => nonvar(C).
minimum(N1,N2,N1) :- N1 =< N2, !.
minimum(_N1,N2,N2).

% :- pred add_keys(+,-).
:- pred add_keys(A,B) : nonvar * var => nonvar(B).
add_keys([],[]).
add_keys([P|L],[C-P|L1]) :- marked(P,_,C,_), add_keys(L,L1).

% :- pred strip_keys(+,-).
:- pred strip_keys(A,B) : nonvar * var => nonvar(B).
strip_keys([],[]).
strip_keys([X|L],[P|L1]) :- strip_key(X,P), strip_keys(L,L1).

% :- pred strip_key(+,?).
:- pred strip_key(A,B) : nonvar(A).
strip_key(_C-P,P).

% :- pred variablise(+,+,-).
:- pred variablise/3 : nonvar * nonvar * var.
variablise('$VAR'(N),VV,V) :- !, N1 is N+1, arg(N1,VV,V).
variablise(T,_,T) :- atomic(T), !.
variablise(T,VV,T1) :-
   functor(T,F,N),
   functor(T1,F,N),
   variablise(N,T,VV,T1).

% :- pred variablise(+,+,+,+).
:- pred variablise/4 : ground * nonvar * nonvar * nonvar.
variablise(0,_,_,_) :- !.
variablise(N,T,VV,T1) :- N1 is N-1,
   arg(N,T,X),
   arg(N,T1,X1),
   variablise(X,VV,X1),
   variablise(N1,T,VV,T1).

% :- pred cost(+,+,-).
:- pred cost/3 : nonvar * nonvar * var.
cost(+P,0,N) :- !, cost(P,0,N).
cost(+_P,_V,1000) :- !.
cost(P,V,N) :- functor(P,F,I), cost(I,F,P,V,N).

% :- pred cost(+,+,+,+,-).
:- pred cost(A,B,C,D,E) : ground * nonvar * nonvar * nonvar * var => ground(E).
cost(1,F,P,V,N) :-
   arg(1,P,X1), instantiated(X1,V,I1),
   nd(F,N0,N1),
   N is N0-I1*N1.
cost(2,F,P,V,N) :-
   arg(1,P,X1), instantiated(X1,V,I1),
   arg(2,P,X2), instantiated(X2,V,I2),
   nd(F,N0,N1,N2),
   N is N0-I1*N1-I2*N2.
cost(3,F,P,V,N) :-
   arg(1,P,X1), instantiated(X1,V,I1),
   arg(2,P,X2), instantiated(X2,V,I2),
   arg(3,P,X3), instantiated(X3,V,I3),
   nd(F,N0,N1,N2,N3),
   N is N0-I1*N1-I2*N2-I3*N3.

% :- pred instantiated(+,+,-).
:- pred instantiated(A,B,C) : nonvar * nonvar * var => ground(C).
instantiated([X|_],V,N) :- !, instantiated(X,V,N).
instantiated('$VAR'(N),V,0) :- setcontains(V,N), !.
instantiated(_,_,1).

/*-------------------------Put in reserve--------------------

sort_parts([],[]) :- !.
sort_parts([X],[X]) :- !.
sort_parts(L,R) :-
   divide(L,L1,L2),
   sort_parts(L1,R1),
   sort_parts(L2,R2),
   merge(R1,R2,R).

divide([X1|L0],[X1|L1],[X2|L2]) :- list(L0,X2,L), !, divide(L,L1,L2).
divide(L,L,[]).

list([X|L],X,L).

merge([],R,R) :- !.
merge([X|R1],R2,[X|R]) :- precedes(X,R2), !, merge(R1,R2,R).
merge(R1,[X|R2],[X|R]) :- !, merge(R1,R2,R).
merge(R,[],R).

precedes(G1,[G2|_]) :- goal_info(G1,_,N1), goal_info(G2,_,N2), N1 =< N2.

-------------------------------------------------------------*/

% :- pred nonempty(+).
:- pred nonempty/1 : nonvar.
nonempty(0) :- !, fail.
nonempty(_).

% :- pred setplus(+,+,-).
:- pred setplus/3 : nonvar * nonvar * var.
setplus(W1-V1,W2-V2,W-V) :- !, V is V1 \/ V2, setplus(W1,W2,W).
setplus(W-V1,V2,W-V) :- !, V is V1 \/ V2.
setplus(V1,W-V2,W-V) :- !, V is V1 \/ V2.
setplus(V1,V2,V) :- V is V1 \/ V2.

% :- pred setminus(+,+,-).
:- pred setminus/3 : nonvar * nonvar * var.
setminus(W1-V1,W2-V2,S) :- !, V is V1 /\ \(V2),
   setminus(W1,W2,W), mkset(W,V,S).
setminus(W-V1,V2,W-V) :- !, V is V1 /\ \(V2).
setminus(V1,_W-V2,V) :- !, V is V1 /\ \(V2).
setminus(V1,V2,V) :- V is V1 /\ \(V2).

% :- pred mkset(+,+,-).
:- pred mkset/3 : nonvar * nonvar * var.
mkset(0,V,V) :- !.
mkset(W,V,W-V).

% :- pred setplusitem(+,+,-).
:- pred setplusitem/3 : nonvar * nonvar * var.
setplusitem(W-V,N,W-V1) :- N < 18, !, V1 is V \/ 1<<N.
setplusitem(W-V,N,W1-V) :- !, N1 is N-18, setplusitem(W,N1,W1).
setplusitem(V,N,V1) :- N < 18, !, V1 is V \/ 1<<N.
setplusitem(V,N,W-V) :- N1 is N-18, setplusitem(0,N1,W).

% :- pred setcontains(+,+).
:- pred setcontains/2 : nonvar * nonvar.
setcontains(_W-V,N) :- N < 18, !, V /\ 1<<N =\= 0.
setcontains(W-_V,N) :- !, N1 is N-18, setcontains(W,N1).
setcontains(V,N) :- N < 18, V /\ 1<<N =\= 0.

% :- pred intersect(+,+).
:- pred intersect/2 : nonvar * nonvar.
intersect(W1-V1,W2-V2) :- !, ( V1 /\ V2 =\= 0 ; intersect(W1,W2) ), !.
intersect(_W-V1,V2) :- !, V1 /\ V2 =\= 0.
intersect(V1,_W-V2) :- !, V1 /\ V2 =\= 0.
intersect(V1,V2) :- V1 /\ V2 =\= 0.

% :- pred disjoint(+,+).
:- pred disjoint/2 : nonvar * nonvar.
disjoint(W1-V1,W2-V2) :- !, V1 /\ V2 =:= 0, disjoint(W1,W2).
disjoint(_W-V1,V2) :- !, V1 /\ V2 =:= 0.
disjoint(V1,_W-V2) :- !, V1 /\ V2 =:= 0.
disjoint(V1,V2) :- V1 /\ V2 =:= 0. 
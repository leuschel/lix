/*Generated by Lix*/
:- dynamic flag/2, memo_table/2.
:- use_module(library(lists)).
:- use_module(library(terms)).
:- op(1150, fx, type).
/*app__1(_6508,_6510)=app(_6508,'.'(b,[]),_6510)*/
app__1([], [b]).
app__1([A|B], [A|C]) :-
        app__1(B, C).
app([a,b,c,d], [b], A) :-
        app__1([a,b,c,d], A).

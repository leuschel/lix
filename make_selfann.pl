%:- module('make_safeann', [make_ann/1, make_ann/2, go/0]).
:- ensure_loaded('logen_source/sicstus.pl').

:- use_module('logen_source/annfile.pl').
:- use_module('logen_source/ann_db.pl').

go :-
	make_ann('lix.pl.ann', 'lix_main.pl').

make_ann(Filename,Save) :-
	tell(Save),
	print('/*Automatically generated do not change*/'),
	nl,
	print('/*Run '), print(make_ann(Filename,Save)), print(' to generate */'),
	%format('~N:- dynamic ann_clause/3.~n',[]),
	%format('~N:- dynamic filter/2.~n',[]),
	nl,	
	make_ann(Filename),
	told.

make_ann(Filename) :-
	see(Filename),
	readTerms,
	seen,
	annfile:load_annfile(Filename),	
	print_annclause,
	print_filter.
	




readTerms :-
	(read_term(Term, []) -> true ; (print_error('read_term failed in readTerms'),fail)),

	(Term == end_of_file ->
	    true
	;(
	  printTerm(Term),	  
	  readTerms)).
	 
printTerm((logen(_ID,Call) :- Body)) :-
        remove_annotation_body(Body,Result),
	portray_clause((Call :- Result)).

printTerm(logen(_ID,Call)) :- portray_clause(Call).
printTerm((:-use_module(M))) :- portray_clause((:-use_module(M))).
printTerm((:-dynamic(M))) :- portray_clause((:-dynamic(M))).
printTerm((:-op(A,B,C))) :- portray_clause((:-op(A,B,C))).


printTerm((:-filter(_X))).
printTerm((:-residual(_X))).
printTerm((:-residual_pred(_X))).
printTerm((:-include(_X))).
printTerm((:-module(_,_))).

printTerm(T) :- print('Unknown Term:'), print(T), nl.


%%% remove_annotation_body(Pattern, Result) :-
%%% 	logen_annotation_structure(Pattern, In, Out, Result),
%%% 	!,
%%% 	logen_map_remove(In,Out)
%%% 	.
%%% remove_annotation_body(Pattern, error) :-
%%% 	print(unknown_pattern(Pattern)).

%%% logen_map_remove([],[]).
%%% logen_map_remove([A|T],[B|T1]) :- remove_annotation_body(A,B),logen_map_remove(T,T1).	
	
print_annclause :-
	annfile:ann_clause(ID,H,B),
	portray_clause(ann_clause(ID,H,B)),
	%pass down level
        % portray_clause(ann_clause(ac, ann_clause(ac,H,B),true)),
	% pass down to silly level
%	portray_clause(unfold__12(ac,H,B,true)),
	fail.
print_annclause.

print_filter :-
	annfile:filter(A,B),
	portray_clause(filter(A,B)),
	portray_clause(ann_clause(filter,filter(A,B),true)),
%	portray_clause(unfold__4(A,B,true)),
	fail.
print_filter.

print_op :-
	annfile:op_def(A,B,C),
	portray_clause(':-'(op(A,B,C))).
print_op.
%print_residual :-
%	annfile:residual(A),
%	print('logen(residual,residual('),
%	numbervars(A,0,_),
%	print(A),
%	print(')).\n'),	     
%	fail.
%print_residual.


	


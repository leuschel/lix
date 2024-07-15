
:- module(annfile, [
		    %annotation file clauses
 		    ann_clause/3,
                    ann_directive/1,
                    this_module_name/1,
		    %clp_residual/1,
		    filter/2,
		    residual/1,		    
%		    static_consult/1,
		    dynamic_pred/1,
		    residual_pred/1,
		    gx_use_module/1, gx_use_module/2, op_def/3,
		    %loading and clearing of annotation files
		    clear_ann_clauses/0,
 		    load_annfile/1, 
		    %????
 		    table/1, user_type/2,
		    convert_ann_to_pl/1,
		   % logen_annotation_structure/4,
		   % remove_annotation_body/2,
		    print_ann_clause_status/0
		  		   
		   ]).

:-dynamic ann_clause/3,   
         filter/2, residual/1, table/1, user_type/2, dynamic_pred/1, residual_pred/1,
         gx_use_module/1, gx_use_module/2, op_def/3,gensym/1, ann_directive/1.





:- op(1150, fx, residual).
:- op(1150, fx, filter).
:- op(1150, fx, table).
:- op(1150, fx, type).
:- op(1150, fx, residual_pred).
:- op(500,yfx,--->).

%:- use_package(.(sicsignore)).
:- if(current_prolog_flag(dialect, ciao)).
:- use_package(sicsignore).
ciao((:- use_module(library(dec10_io)))).
:- endif.


%:- use_module('cogen-tools').
:- use_module('flags.pl').
:- use_module('ann_db.pl').
:- use_module('logen_messages.pl').


ciao(logen_annotation_structure(A,B,C,D)) :- ann_db:logen_annotation_structure(A,B,C,D).
%ciao(remove_annotation_body(A,B)) :- ann_db:remove_annotation_body(A,B).


valid_ann_pred(gensym(_)).
valid_ann_pred(ann_clause(_,_Head,_)).
valid_ann_pred(residual(_Head)).
valid_ann_pred(filter(_Head,_)).
valid_ann_pred(table(_)).
valid_ann_pred(user_type(_,_)).
valid_ann_pred(dynamic_pred(_)).
valid_ann_pred(gx_use_module(_)).
valid_ann_pred(gx_use_module(_,_)).
valid_ann_pred(residual_pred(_)).
valid_ann_pred(op_def(_,_,_)).


print_ann_clause_status :- print_message('Status of Annotated Clause Database:'),
    print_message('Filter Declarations:'),filter(Head,Type),
    print_message(filter(Head,Type)),
    fail.
print_ann_clause_status :- nl,print_message('Types:'),
    user_type(Head,Type),
    print_message(type(Head,Type)),
    fail. 
print_ann_clause_status :- print_message('Dynamic Preds:'), %print_short_msg('% '),
    dynamic_pred(Head),
    print_message(Head), %print_short_msg(' '),
    fail. 
print_ann_clause_status :- nl,print_message('Used Modules:'), %print_short_msg('% '),
    (gx_use_module(Head) ; gx_use_module(Head,_)),
    print_message(Head), %print_short_msg(' '),
    fail. 
print_ann_clause_status :- nl,print_message('Operator Declarations:'), %print_short_msg('% '),
    op_def(_,_,Op),
    print_message(Op), %print_short_msg(' '),
    fail. 
print_ann_clause_status :- nl.

%% Clear all terms in valid_ann_term clauses
clear_ann_clauses :-
	findall(Term, valid_ann_pred(Term), Bag),
	print_debug_message(deleting_annotations(Bag)),
	retractAllTerms(Bag),
        retractall(ann_directive(_)),
	setNum(0).

retractAllTerms([]).
retractAllTerms([Term|Tail]) :-
	       retractall(Term),
		retractAllTerms(Tail).

define_ops :-
	op(1150, fx, residual),
	op(1150, fx, filter),
	op(1150, fx, table),
	op(1150, fx, type),
	op(1150, fx, residual_pred).

:- dynamic module_info/1.

%% Load a new annotation file and assert clauses
load_annfile(Filename) :-
	% consult a new annotation file and remove any old clauses
	define_ops,
	clear_ann_clauses,
	see(Filename),
	read_annfile_and_assertTerms,!,
	seen.
load_annfile(FN) :-
    print_error('Loading annfile failed for:'),
    print_error(FN),
    print_error_line_number,
    seen,
    fail.





logen_translate_annfile_clause(logen(_OLD_ID,Head), ann_clause(ID,Head,true)) :- get_id(ID).
logen_translate_annfile_clause((logen(_OLD_ID,Head):-Body), ann_clause(ID,Head,Body)) :-  
   (logen_validate_body(Body) -> get_id(ID) ; print_error_line_number('Invalid Annotation')).

logen_translate_annfile_clause((:-filter(X:_ID)), filter(FilterCall,ETypes)) :- 
    makeFilter(X,FilterCall,Types),
    assert(residual(FilterCall)),  %% temp fix for not needing residual pred
    (l_expand_type_def(Types,ETypes) 
      -> true /* print_message(exp(FilterCall,Types,ETypes)) */
      ;  (print_error('Illegal filter type list'),
          print_error(FilterCall), print_error(X), ETypes = Types)
    ).
logen_translate_annfile_clause((:-filter(X)), Filter) :- logen_translate_annfile_clause((:-filter(X:X)), Filter).


%logen_translate_annfile_clause((:-residual(Func/Arity)), residual(NewCall)) :-
%     makeList(Arity, Args), NewCall =..[Func|Args].
logen_translate_annfile_clause((:-table(Func/Arity)), table(NewCall)) :-
     makeList(Arity, Args), NewCall =..[Func|Args],
     print_debug_message(table(NewCall)).
logen_translate_annfile_clause((:-op(Prio,Type,Symb)), op_def(Prio,Type,Symb)) :-
     print_debug_message(executing_op(Prio,Type,Symb)),
     op(Prio,Type,Symb).
logen_translate_annfile_clause((:-type(Type ---> Def)), user_type(Type,EDef)) :-
    print_debug_message(type(Type,Def)),
    (expand_type_def(Def,EDef) 
       -> print_debug_message(user_type(Type,EDef))
       ;  (print_error('Illegal type definition - substituting dynamic'),
           print_error(Type), print_error(Def), EDef = dynamic)
    ).
logen_translate_annfile_clause((:-type(Type = Def)), user_type(Type,EDef)) :-
    logen_translate_annfile_clause((:-type(Type ---> Def)), user_type(Type,EDef)).
logen_translate_annfile_clause((:-dynamic(Func/Arity)), dynamic_pred(Func/Arity)).

%logen_translate_annfile_clause((:-dynamic(Func/Arity)), dynamic_pred(NewCall))  :-
%     makeList(Arity, Args), NewCall =..[Func|Args],
%     true.

%     print_message(dynamic_pred(NewCall)). /* does this cater for multiple predicates on a line ??? */
logen_translate_annfile_clause((:-use_module(Mod)), gx_use_module(Mod)).
logen_translate_annfile_clause((:-use_module(Mod,List)), gx_use_module(Mod,List)).
logen_translate_annfile_clause((:-residual_pred(Func/Arity)), residual_pred(NewCall)) :-
     makeList(Arity, Args), NewCall =.. [Func|Args].
     

     
expand_type_def(X,X) :- var(X),!.    
expand_type_def(X,X) :- nonvar(X),base_type(X),!.
expand_type_def(X,X) :- nonvar(X),is_intersection_type(X),!.
expand_type_def((T1 ; T2), (ET1 ; ET2)) :- !, expand_type_def(T1,ET1), expand_type_def(T2,ET2).
expand_type_def(nonvar(T1), nonvar(ET1)) :- !, expand_type_def(T1,ET1).
expand_type_def(list(T1), type(list(ET1))) :- !, expand_type_def(T1,ET1).
expand_type_def(type(list(T1)), type(list(ET1))) :- !, expand_type_def(T1,ET1).
expand_type_def(type(X), type(EX)) :- nonvar(X),!, X=..[F|A],
    l_expand_type_def(A,Args),EX =..[F|Args].
expand_type_def(struct(F,A),struct(F,Args)) :- !,
       %print_error('Warning: use of struct/2 deprecated! Use term/1 instead:'),
       %print_error(struct(F,A)), 
       FA =.. [F|A], print_error(term(FA)),
       l_expand_type_def(A,Args).
expand_type_def(term(Struct),struct(F,Args)) :- nonvar(Struct),!, Struct =.. [F|A],
   l_expand_type_def(A,Args).
expand_type_def(Struct,struct(F,Args)) :- nonvar(Struct), Struct =.. [F|A],
   l_expand_type_def(A,Args).

base_type(static). base_type(dynamic).
base_type(static_or_free). base_type(free).
base_type(nonvar). base_type(nonnonvar). base_type(nonvar_nf).
base_type(static_nf).
base_type(semi).

is_intersection_type([]).
is_intersection_type([H|T]) :- is_single_intersection_type(H), is_intersection_type(T).

%%added by steve
is_single_intersection_type(A) :- var(A), !, true.

is_single_intersection_type([]).
is_single_intersection_type([H|T]) :- atomic(H),is_single_intersection_type(T).

l_expand_type_def([],[]).
l_expand_type_def([H|T],[EH|ET]) :- expand_type_def(H,EH), l_expand_type_def(T,ET).

logen_validate_body(Call) :-
	((nonvar(Call),ann_db:logen_annotation_structure(Call,Args,_,_))
	  ->	logen_map(Args)
	  ; (print_error_line_number('Unrecognised annotation structure'),
	     print_error(Call))
	).


logen_map([]).
logen_map([A|T]) :- logen_validate_body(A),logen_map(T).





   			 
makeFilter(X,FilterCall, Types) :-
	X =.. [Func|Types], freeArgs(Types,FreeArgs), FilterCall =.. [Func|FreeArgs].
freeArgs([],[]).
freeArgs([_|T], [_|T1]) :- freeArgs(T,T1).
makeList(0,[]).
makeList(N,[_|Tail]) :- N > 0, N1 is N -1, makeList(N1,Tail).

ignoreDirective(module(_, _)).
ignoreDirective(module(_, _, _)).
%ignoreDirective(use_module(M)) :- M \= library(_).
ignoreDirective(include(_File)).
ignoreDirective(set_logen_flag(Flag)) :- set_logen_flag(Flag).
ignoreDirective(assert_pre(_,_)).
ignoreDirective(assert_post(_,_)).
ignoreDirective(assert_must_succeed(_)).
ignoreDirective(assert_must_fail(_)).

%% assert terms if they are valid_ann_terms
read_annfile_and_assertTerms :-
	(read_term(Term, []) -> true ; (print_error('read_term failed in read_annfile_and_assertTerms'),fail)),

	(Term == end_of_file ->
	    true
	  ;(
	     ((Term = (:- Directive), ignoreDirective(Directive)) ->
		    (print_debug_message(assert_ann_directive(Directive)),
		     assert(ann_directive(Directive)))
	       ;
	       (
		    ((logen_translate_annfile_clause(Term, AssertTerm),assert(AssertTerm))->
		     true
		     ;
		       (print_message('Ignoring term: '), print_message(Term))
		    ))
		 ),
	     read_annfile_and_assertTerms
	  )
	).

this_module_name(Name) :- ann_directive(module(Name, _)), !.
this_module_name(Name) :- ann_directive(module(Name, _, _)), !.
this_module_name(user).


get_id(X) :-
	gensym(X),
	!,
	retract(gensym(X)),
	X1 is X +1,
	assert(gensym(X1)).
get_id(0) :-
	assert(gensym(1)).
	
	

convert_ann_to_pl(Filename) :-
	see(Filename),
	convert_ann,
	seen.

%%% this obviously needs to be changed ;-)


convert_ann :-
	read_term(Term, []),
	(Term = end_of_file ->
	    true
	;
	numbervars(Term, 0,_),
	convert_term(Term, NewTerm),	
	(NewTerm = [] ->
	    convert_ann
	;
	    write(NewTerm),write('.'), nl, convert_ann
	)).


convert_term(ann_clause(_Id, Head, Body), (Head :- NewBody)):-
                                    !,convert_body(Body, NewBody).
convert_term(_, []).
convert_body((A,B), (NA,NB)) :- convert_body(A,NA), convert_body(B,NB).
%convert_body(unfold(X), X).
convert_body(memo(X), X).
%convert_body(clpmemo(X),X).
convert_body(constraint(X), {NX}) :- commToList(X,NX).
convert_body(X,Arg) :- X =.. [_Func, Arg]. 
convert_body(X,X).

commToList([X], X).
commToList([X|T], (X,TX)) :- commToList(T,TX).






%%%%%% nothing needed should be down here now........ (if I was braver I'd delete them)







%%% these convert the new fancy emacs annotations :-) into boring old logen ones....

convertClauseToOld(logen(_ID,Head) , ann_clause(NewID,Head,true) ) :- get_id(NewID).
convertClauseToOld( (logen(_ID, Head) :- Body),
		     ann_clause(NewID,Head,NewBody)
		  ) :-get_id(NewID),convertToOldAnnotations(Body,NewBody).


%convertClauseToOld( (:-filter(X):_ID), Filter) :- convertClauseToOld( (:- filter(X)), Filter).
convertClauseToOld( (:-filter(X:_ID)), Filter) :- convertClauseToOld( (:- filter(X)), Filter).

convertClauseToOld( (:-filter(X)) ,
		    (:-filter(FilterCall, Types))
		  ) :- makeFilter(X, FilterCall, Types).
convertClauseToOld( (:-residual(Func/Arity))   ,
			 (:-residual(NX))
		       ) :- makeList(Arity, Args), NX =.. [Func|Args].
convertClauseToOld( (:-table(Func/Arity))   ,
			 (:-table(NX))
		       ) :- makeList(Arity, Args), NX =.. [Func|Args].

convertToOldAnnotations((A,B), (NA,NB)) :- !,convertToOldAnnotations(A,NA), convertToOldAnnotations(B,NB).
%convertToOldAnnotations(logen(X,when(Cond,Call)), NewBody) :- !, logen(NewBody =..[X,NCall].
convertToOldAnnotations(logen(X,Call), NewBody) :- !,NewBody =.. [X,Call].
convertToOldAnnotations(resif(A, B, C), resif(NA,NB,NC) ) :-
    convertToOldAnnotations(A,NA), convertToOldAnnotations(B,NB), convertToOldAnnotations(C,NC).
convertToOldAnnotations(if(A, B, C), if(NA,NB,NC) ) :-
    convertToOldAnnotations(A,NA), convertToOldAnnotations(B,NB), convertToOldAnnotations(C,NC).
convertToOldAnnotations(semif(A, B, C), semif(NA,NB,NC) ) :-
    convertToOldAnnotations(A,NA), convertToOldAnnotations(B,NB), convertToOldAnnotations(C,NC).
convertToOldAnnotations(resdisj(A, B), resdisj(NA,NB) ) :-
    convertToOldAnnotations(A,NA), convertToOldAnnotations(B,NB).
convertToOldAnnotations(';'(A, B), ';'(NA,NB) ) :-
    convertToOldAnnotations(A,NA), convertToOldAnnotations(B,NB).
convertToOldAnnotations(resnot(A), resnot(NA) ) :-
    convertToOldAnnotations(A,NA).
convertToOldAnnotations(not(A), not(NA) ) :-
    convertToOldAnnotations(A,NA).
convertToOldAnnotations(hide_nf(A), hide_nf(NA) ) :-
    convertToOldAnnotations(A,NA).

convertToOldAnnotations(hide(A), hide(NA) ) :-
    convertToOldAnnotations(A,NA).
convertToOldAnnotations(resfindall(A, B, C), resfindall(A,NB,C) ) :-
    convertToOldAnnotations(B,NB).
convertToOldAnnotations(findall(A, B, C), findall(A,NB,C) ) :-
    convertToOldAnnotations(B,NB).
convertToOldAnnotations(X,X) :- print_error('Warning: Unknown annotation: '), print_error(X).





/* Nobody seems to use that ???: */
%%% annTerm((:- Term), AssertTerm) :- annTerm(Term, AssertTerm).
%%% annTerm(ann_clause(Head, Body), AssertTerm) :- !,
%%%       getNum(X),annTerm(ann_clause(X,Head,Body), AssertTerm).
%%% annTerm(Term, Term) :- valid_ann_term(Term).

%%% assertAnn(ann_clause(Head,Body)) :- getNum(X), assert(ann_clause(X,Head,Body)), !.
%%% assertAnn(Term) :- assert(Term).

:- dynamic gnum/1.
setNum(X) :- retractall(gnum(_)), assert(gnum(X)). 
getNum(X) :- gnum(X), X1 is X +1, setNum(X1).



%% List of all valid terms for annotation files
%% These are the terms that will be read and asserted
%% and cleared.
%%% valid_ann_term(ann_clause(_,Head,_)) :- nonvar(Head).
%%% valid_ann_term(ann_clause(_,Head)) :- nonvar(Head).
%%% valid_ann_term(residual(Head)) :- nonvar(Head).
%%% valid_ann_term(clp_residual(Head)) :- nonvar(Head).
%%% valid_ann_term(filter(Head,_)) :- nonvar(Head).
%%% valid_ann_term(table(Table)) :- nonvar(Table).
%%% valid_ann_term(dynamic_pred(P)) :- nonvar(P).
%%% valid_ann_term(static_consult(Files)) :- nonvar(Files).
%%% valid_ann_term(user_type(Type,Def)) :- nonvar(Type),nonvar(Def).
	










	





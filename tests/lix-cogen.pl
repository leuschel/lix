/*Generated by Lix*/
:- dynamic flag/2, memo_table/2.
:- use_module(library(lists)).
:- use_module(library(terms)).
:- op(1150, fx, type).
/*oldvalue__1(_7454,_7455)=oldvalue(_7454,_7455)*/
oldvalue__1(A, B) :-
        flag(gensym(A), B), !.
oldvalue__1(_, 0).
/*set_flag__1(_7939,_7918)=set_flag(gensym(_7939),_7918)*/
set_flag__1(A, B) :-
        retract(flag(gensym(A),_)), !,
        asserta(flag(gensym(A),B)).
set_flag__1(A, B) :-
        asserta(flag(gensym(A),B)).
/*gensym__1(_7191)=gensym(lix,_7191)*/
gensym__1(A) :-
        var(A),
        oldvalue__1(lix, B),
        C is B+1,
        set_flag__1(lix, C),
        name(C, D),
        name(A, [108,105,120,95,95|D]).
/*gensym__2(_8646)=gensym(memo,_8646)*/
gensym__2(A) :-
        var(A),
        oldvalue__1(memo, B),
        C is B+1,
        set_flag__1(memo, C),
        name(C, D),
        name(A, [109,101,109,111,95,95|D]).
/*unfold__4(_10059,_10061,_10036)=unfold(filter(_10059,_10061),_10036)*/
unfold__4(app(_,_,_), [dynamic,static,dynamic], true).
unfold__4(test(_), [dynamic], true).
unfold__4(l_eval(_,_,_), [static,(type list(struct(/,[static,dynamic]))),dynamic], true).
unfold__4(eval(_,_,_), [static,(type list(struct(/,[static,dynamic]))),dynamic], true).
unfold__4(rename(_,_,_), [dynamic,dynamic,dynamic], true).
unfold__4(function(_,_), [dynamic,dynamic], true).
unfold__4(store(_,_,_,_), [dynamic,static,static,dynamic], true).
unfold__4(lookup(_,_,_), [struct(static,[]),dynamic,dynamic], true).
unfold__4(fib(_,_), [dynamic,dynamic], true).
unfold__4(bench(_,_), [dynamic,dynamic], true).
unfold__4(bench2(_,_), [dynamic,dynamic], true).
unfold__4(eval_if(_,_,_,_,_), [static,static,static,(type list(struct(/,[static,dynamic]))),dynamic], true).
/*unfold__6(_11310,_11312,_11287)=unfold(typedef(_11310,_11312),_11287)*/
unfold__6(list(A), (struct([],[]);struct('.',[A,(type list(A))])), true).
/*gensym__3(_12336)=gensym(gen_filter,_12336)*/
gensym__3(A) :-
        var(A),
        oldvalue__1(gen_filter, B),
        C is B+1,
        set_flag__1(gen_filter, C),
        name(C, D),
        name(A, [103,101,110,95,102,105,108,116,101,114,95,95|D]).
/*flatten__1(_13236,_13237)=flatten(_13236,_13237)*/
flatten__1((A:-B), (A:-C)) :- !,
        flatten__1(B, C).
flatten__1((A,B), C) :- !,
        flatten__1(A, D),
        flatten__1(B, E),
        (   D=true ->
            C=E
        ;   E=true ->
            C=D
        ;   C=(D,E)
        ).
flatten__1((A;B), C) :- !,
        flatten__1(A, D),
        flatten__1(B, E),
        (   D=true ->
            C=E
        ;   E=true ->
            C=D
        ;   C=(D;E)
        ).
flatten__1((A->B;C), (D->E;F)) :- !,
        flatten__1(A, D),
        flatten__1(B, E),
        flatten__1(C, F).
flatten__1(A, A).
/*pretty_print_clauses__1(_12932)=pretty_print_clauses(_12932)*/
pretty_print_clauses__1([]).
pretty_print_clauses__1([A|B]) :-
        flatten__1(A, C),
        portray_clause(C),
        pretty_print_clauses__1(B).
/*memo__3(_11838,_11840,_11842,_11844,_11811)=memo(gen_filter(_11838,_11840,_11842,_11844),_11811)*/
memo__3(A, B, C, D, E) :-
        (   memo_table(gen_filter(A,B,C,D), E) ->
            true
        ;   gensym__3(F),
            G=..[F,H,I,J],
            assert(memo_table(gen_filter(A,H,I,J),G)),
            findall((G:-K), unfold__5(A,H,I,J,K), L),
            format('/*~k=~k*/~n', [G,gen_filter(A,H,I,J)]),
            pretty_print_clauses__1(L),
            memo_table(gen_filter(A,B,C,D), E)
        ).
/*unfold__5(_10985,_10987,_10989,_10991,_10958)=unfold(gen_filter(_10985,_10987,_10989,_10991),_10958)*/
unfold__5([], [], [], [], true).
unfold__5([(A;_)|B], C, D, E, F) :-
        unfold__5([A|B], C, D, E, F).
unfold__5([(_;A)|B], C, D, E, F) :-
        unfold__5([A|B], C, D, E, F).
unfold__5([static|A], [B|C], [B|D], E, F) :-
        unfold__5(A, C, D, E, F).
unfold__5([dynamic|A], [_|B], [C|D], [C|E], F) :-
        unfold__5(A, B, D, E, F).
unfold__5([nonvar|A], [B|C], [D|E], F, (B=..[G|H],length(H,I),length(J,I),D=..[G|J],K,append(J,L,F))) :-
        unfold__5(A, C, E, L, K).
unfold__5([(type A)|B], C, D, E, (F,G)) :-
        unfold__6(A, H, F),
        memo__3([H|B], C, D, E, G).
unfold__5([struct(A,B)|C], [D|E], [F|G], H, (D=..[A|I],J,F=..[A|K],L,append(M,N,H))) :-
        unfold__5(B, I, K, M, J),
        unfold__5(C, E, G, N, L).
/*gensym__4(_12363)=gensym(gensym,_12363)*/
gensym__4(A) :-
        var(A),
        oldvalue__1(gensym, B),
        C is B+1,
        set_flag__1(gensym, C),
        name(C, D),
        name(A, [103,101,110,115,121,109,95,95|D]).
/*gensym__5(_13808)=gensym(oldvalue,_13808)*/
gensym__5(A) :-
        var(A),
        oldvalue__1(oldvalue, B),
        C is B+1,
        set_flag__1(oldvalue, C),
        name(C, D),
        name(A, [111,108,100,118,97,108,117,101,95,95|D]).
/*unfold__8(_14390,_14392,_14367)=unfold(oldvalue(_14390,_14392),_14367)*/
unfold__8(A, B, (flag(gensym(A),B),!)).
unfold__8(_, 0, true).
/*memo__5(_13360,_13362,_13337)=memo(oldvalue(_13360,_13362),_13337)*/
memo__5(A, B, C) :-
        (   memo_table(oldvalue(A,B), C) ->
            true
        ;   gensym__5(D),
            E=..[D,F,G],
            assert(memo_table(oldvalue(F,G),E)),
            findall((E:-H), unfold__8(F,G,H), I),
            format('/*~k=~k*/~n', [E,oldvalue(F,G)]),
            pretty_print_clauses__1(I),
            memo_table(oldvalue(A,B), C)
        ).
/*gensym__6(_14513)=gensym(set_flag,_14513)*/
gensym__6(A) :-
        var(A),
        oldvalue__1(set_flag, B),
        C is B+1,
        set_flag__1(set_flag, C),
        name(C, D),
        name(A, [115,101,116,95,102,108,97,103,95,95|D]).
/*unfold__9(_15095,_15097,_15072)=unfold(set_flag(_15095,_15097),_15072)*/
unfold__9(A, B, (true,retract(flag(A,_)),!,asserta(flag(A,B)))) :-
        call(nonvar(A)).
unfold__9(A, B, (true,asserta(flag(A,B)))) :-
        call(nonvar(A)).
/*memo__6(_14002,_14004,_13979)=memo(set_flag(_14002,_14004),_13979)*/
memo__6(A, B, C) :-
        (   memo_table(set_flag(A,B), C) ->
            true
        ;   A=..[D|E],
            length(E, F),
            length(G, F),
            H=..[D|G],
            append(G, [I], J),
            gensym__6(K),
            L=..[K|J],
            assert(memo_table(set_flag(H,I),L)),
            findall((L:-M), unfold__9(H,I,M), N),
            format('/*~k=~k*/~n', [L,set_flag(H,I)]),
            pretty_print_clauses__1(N),
            memo_table(set_flag(A,B), C)
        ).
/*unfold__11(_15179,_15181,_15183,_15154)=unfold(append(_15179,_15181,_15183),_15154)*/
unfold__11([], A, A, true).
unfold__11([A|B], C, [A|D], E) :-
        unfold__11(B, C, D, E).
/*unfold__10(_14699,_14701,_14703,_14674)=unfold(string_concat(_14699,_14701,_14703),_14674)*/
unfold__10(A, B, C, (true,D,E,F)) :-
        call(name(A,G)),
        (   call(var(B)) ->
            true,
            name(B,H)=D
        ;   call(name(B,H)),
            true=D
        ),
        unfold__11(G, H, I, E),
        (   call(var(B)) ->
            true,
            name(C,I)=F
        ;   call(name(C,I)),
            true=F
        ).
/*unfold__7(_12941,_12943,_12918)=unfold(gensym(_12941,_12943),_12918)*/
unfold__7(A, B, (var(B),true,C,D is E+1,F,true,G,H)) :-
        call(atom(A)),
        memo__5(A, E, C),
        memo__6(gensym(A), D, F),
        call(name(I,[95,95])),
        unfold__10(A, I, J, G),
        unfold__10(J, D, B, H).
/*memo__4(_11917,_11919,_11894)=memo(gensym(_11917,_11919),_11894)*/
memo__4(A, B, C) :-
        (   memo_table(gensym(A,B), C) ->
            true
        ;   gensym__4(D),
            E=..[D,F],
            assert(memo_table(gensym(A,F),E)),
            findall((E:-G), unfold__7(A,F,G), H),
            format('/*~k=~k*/~n', [E,gensym(A,F)]),
            pretty_print_clauses__1(H),
            memo_table(gensym(A,B), C)
        ).
/*unfold__3(_9628,_9630,_9632,_9603)=unfold(generalise_and_filter(_9628,_9630,_9632),_9603)*/
unfold__3(A, B, C, (true,true,D,true,E,true,F,C=..[G|H])) :-
        call(functor(A,I,J)),
        call(functor(B,I,J)),
        unfold__4(A, K, D),
        call(A=..[L|M]),
        unfold__5(K, M, N, H, E),
        call(B=..[L|N]),
        memo__4(L, G, F).
/*gensym__7(_10816)=gensym(unfold,_10816)*/
gensym__7(A) :-
        var(A),
        oldvalue__1(unfold, B),
        C is B+1,
        set_flag__1(unfold, C),
        name(C, D),
        name(A, [117,110,102,111,108,100,95,95|D]).
/*unfold__13(_11704,_11706,_11708,_11679)=unfold(ann_clause(_11704,_11706,_11708),_11679)*/
unfold__13(1, app([],A,A), true, true).
unfold__13(2, app([A|B],C,[A|D]), logen(memo,app(B,C,D)), true).
unfold__13(5, test(A), hide_nf(logen(unfold,p(A))), true).
unfold__13(3, p(a), true, true).
unfold__13(4, p(b), true, true).
unfold__13(0, l_eval([],_,[]), true, true).
unfold__13(1, l_eval([A|B],C,[D|E]), (logen(unfold,eval(A,C,D)),logen(unfold,l_eval(B,C,E))), true).
unfold__13(2, eval(cst(A),_,constr(A,[])), true, true).
unfold__13(3, eval(constr(A,B),C,constr(A,D)), logen(unfold,l_eval(B,C,D)), true).
unfold__13(4, eval(var(A),B,C), logen(unfold,lookup(A,B,C)), true).
unfold__13(5, eval(plus(A,B),C,constr(D,[])), (logen(unfold,eval(A,C,constr(E,[]))),logen(unfold,eval(B,C,constr(F,[]))),logen(rescall,D is E+F)), true).
unfold__13(6, eval(minus(A,B),C,constr(D,[])), (logen(unfold,eval(A,C,constr(E,[]))),logen(unfold,eval(B,C,constr(F,[]))),logen(rescall,D is E-F)), true).
unfold__13(7, eval(times(A,B),C,constr(D,[])), (logen(unfold,eval(A,C,constr(E,[]))),logen(unfold,eval(B,C,constr(F,[]))),logen(rescall,D is E*F)), true).
unfold__13(8, eval(eq(A,B),C,constr(D,[])), (logen(unfold,eval(A,C,E)),logen(unfold,eval(B,C,F)),resif(logen(rescall,E=F),logen(rescall,D=true),logen(rescall,D=false))), true).
unfold__13(9, eval(let(A,B,C),D,E), (logen(unfold,eval(B,D,F)),logen(unfold,store(D,A,F,G)),logen(unfold,eval(C,G,E))), true).
unfold__13(10, eval(if(A,B,C),D,E), logen(unfold,eval_if(A,B,C,D,E)), true).
unfold__13(11, eval(if2(A,B,C),D,E), (logen(unfold,eval(A,D,F)),resif(logen(rescall,F=constr(true,[])),hide_nf(logen(unfold,eval(B,D,E))),hide_nf(logen(unfold,eval(C,D,E))))), true).
unfold__13(12, eval(lambda(A,B),_,lambda(A,B)), true, true).
unfold__13(13, eval(apply(A,B),C,D), (logen(unfold,eval(B,C,E)),logen(unfold,rename(E,C,lambda(F,G))),logen(unfold,eval(A,C,H)),logen(unfold,store(C,F,H,I)),logen(memo,eval(G,I,D))), true).
unfold__13(14, eval(fun(A),_,B), logen(unfold,function(A,B)), true).
unfold__13(15, eval(print(A),_,constr(true,[])), (logen(rescall,print(A)),logen(rescall,nl)), true).
unfold__13(16, eval_if(A,B,_,C,D), (logen(unfold,test(A,C)),logen(rescall,!),logen(unfold,eval(B,C,D))), true).
unfold__13(17, eval_if(_,_,A,B,C), logen(unfold,eval(A,B,C)), true).
unfold__13(18, test(eq(A,B),C), (logen(unfold,eval(A,C,D)),logen(unfold,eval(B,C,D))), true).
unfold__13(19, rename(A,_,B), logen(call,B=A), true).
unfold__13(20, function(fib,lambda(x,if(eq(var(x),cst(0)),cst(1),if(eq(var(x),cst(1)),cst(1),plus(apply(minus(var(x),cst(1)),fun(fib)),apply(minus(var(x),cst(2)),fun(fib))))))), true, true).
unfold__13(21, store([],A,B,[A/B]), true, true).
unfold__13(22, store([A/_|B],A,C,[A/C|B]), true, true).
unfold__13(23, store([A/B|C],D,E,[A/B|F]), (logen(call,D\==A),logen(unfold,store(C,D,E,F))), true).
unfold__13(24, lookup(A,[A/B|_],B), true, true).
unfold__13(25, lookup(A,[B/_|C],D), (logen(rescall,A\==B),logen(unfold,lookup(A,C,D))), true).
unfold__13(26, fib(A,B), (logen(unfold,store([],x,A,C)),logen(unfold,eval(apply(cst(A),fun(fib)),C,constr(B,_)))), true).
unfold__13(27, bench(A,B), (logen(rescall,A>B),logen(rescall,print('Done')),logen(rescall,nl)), true).
unfold__13(28, bench(A,B), (logen(rescall,A=<B),logen(unfold,fib(A,C)),logen(rescall,!),logen(rescall,print(fib(A))),logen(rescall,print(' == ')),logen(rescall,print(C)),logen(rescall,nl),logen(rescall,D is A+1),logen(memo,bench(D,B))), true).
/*gensym__8(_14712)=gensym(make_disj,_14712)*/
gensym__8(A) :-
        var(A),
        oldvalue__1(make_disj, B),
        C is B+1,
        set_flag__1(make_disj, C),
        name(C, D),
        name(A, [109,97,107,101,95,100,105,115,106,95,95|D]).
/*gensym__9(_16096)=gensym(simplify_eq,_16096)*/
gensym__9(A) :-
        var(A),
        oldvalue__1(simplify_eq, B),
        C is B+1,
        set_flag__1(simplify_eq, C),
        name(C, D),
        name(A, [115,105,109,112,108,105,102,121,95,101,113,95,95|D]).
/*gensym__10(_17643)=gensym(simplify_eqL,_17643)*/
gensym__10(A) :-
        var(A),
        oldvalue__1(simplify_eqL, B),
        C is B+1,
        set_flag__1(simplify_eqL, C),
        name(C, D),
        name(A, [115,105,109,112,108,105,102,121,95,101,113,76,95,95|D]).
/*unfold__17(_18237,_18239,_18241,_18212)=unfold(simplify_eqL(_18237,_18239,_18241),_18212)*/
unfold__17([A], [B], C, D) :-
        memo__9(A, B, C, D).
unfold__17([A|B], [C|D], (E,F), (G,H)) :-
        memo__9(A, C, E, G),
        memo__10(B, D, F, H).
/*memo__10(_17167,_17169,_17171,_17142)=memo(simplify_eqL(_17167,_17169,_17171),_17142)*/
memo__10(A, B, C, D) :-
        (   memo_table(simplify_eqL(A,B,C), D) ->
            true
        ;   gensym__10(E),
            F=..[E,G,H,I],
            assert(memo_table(simplify_eqL(G,H,I),F)),
            findall((F:-J), unfold__17(G,H,I,J), K),
            format('/*~k=~k*/~n', [F,simplify_eqL(G,H,I)]),
            pretty_print_clauses__1(K),
            memo_table(simplify_eqL(A,B,C), D)
        ).
/*unfold__16(_16686,_16688,_16690,_16661)=unfold(simplify_eq(_16686,_16688,_16690),_16661)*/
unfold__16(A, B, fail, A\=B).
unfold__16(A, B, true, A==B).
unfold__16(A, B, A=B, (var(A),!)).
unfold__16(A, B, A=B, (var(B),!)).
unfold__16(A, B, C, (nonvar(A),nonvar(B),functor(A,D,E),functor(B,D,E),A=..[D|F],B=..[D|G],H)) :-
        memo__10(F, G, C, H).
/*memo__9(_15622,_15624,_15626,_15597)=memo(simplify_eq(_15622,_15624,_15626),_15597)*/
memo__9(A, B, C, D) :-
        (   memo_table(simplify_eq(A,B,C), D) ->
            true
        ;   gensym__9(E),
            F=..[E,G,H,I],
            assert(memo_table(simplify_eq(G,H,I),F)),
            findall((F:-J), unfold__16(G,H,I,J), K),
            format('/*~k=~k*/~n', [F,simplify_eq(G,H,I)]),
            pretty_print_clauses__1(K),
            memo_table(simplify_eq(A,B,C), D)
        ).
/*unfold__15(_15298,_15300,_15302,_15273)=unfold(make_disj(_15298,_15300,_15302),_15273)*/
unfold__15([(A,B)], C, D, (E,D=(F,A))) :-
        memo__9(B, C, F, E).
unfold__15([(A,B)|C], D, (E;F), (G,H,E=(I,A))) :-
        memo__8(C, D, F, G),
        memo__9(B, D, I, H).
/*memo__8(_14238,_14240,_14242,_14213)=memo(make_disj(_14238,_14240,_14242),_14213)*/
memo__8(A, B, C, D) :-
        (   memo_table(make_disj(A,B,C), D) ->
            true
        ;   gensym__8(E),
            F=..[E,G,H,I],
            assert(memo_table(make_disj(G,H,I),F)),
            findall((F:-J), unfold__15(G,H,I,J), K),
            format('/*~k=~k*/~n', [F,make_disj(G,H,I)]),
            pretty_print_clauses__1(K),
            memo_table(make_disj(A,B,C), D)
        ).
/*gensym__11(_15301)=gensym(flatten,_15301)*/
gensym__11(A) :-
        var(A),
        oldvalue__1(flatten, B),
        C is B+1,
        set_flag__1(flatten, C),
        name(C, D),
        name(A, [102,108,97,116,116,101,110,95,95|D]).
/*unfold__18(_15883,_15885,_15860)=unfold(flatten(_15883,_15885),_15860)*/
unfold__18((A:-B), (A:-C), (!,D)) :-
        memo__11(B, C, D).
unfold__18((A,B), C, (!,D,E,(F=true->C=G;G=true->C=F;C=(F,G)))) :-
        memo__11(A, F, D),
        memo__11(B, G, E).
unfold__18((A;B), C, (!,D,E,(F=true->C=G;G=true->C=F;C=(F;G)))) :-
        memo__11(A, F, D),
        memo__11(B, G, E).
unfold__18((A->B;C), (D->E;F), (!,G,H,I)) :-
        memo__11(A, D, G),
        memo__11(B, E, H),
        memo__11(C, F, I).
unfold__18(A, A, true).
/*memo__11(_14851,_14853,_14828)=memo(flatten(_14851,_14853),_14828)*/
memo__11(A, B, C) :-
        (   memo_table(flatten(A,B), C) ->
            true
        ;   gensym__11(D),
            E=..[D,F,G],
            assert(memo_table(flatten(F,G),E)),
            findall((E:-H), unfold__18(F,G,H), I),
            format('/*~k=~k*/~n', [E,flatten(F,G)]),
            pretty_print_clauses__1(I),
            memo_table(flatten(A,B), C)
        ).
/*unfold__14(_13759,_13761,_13736)=unfold(body(_13759,_13761),_13736)*/
unfold__14(true, true, true).
unfold__14((A,B), (C,D), (E,F)) :-
        unfold__14(A, C, E),
        unfold__14(B, D, F).
unfold__14(logen(call,A), true, call(A)).
unfold__14(logen(rescall,A), A, true).
unfold__14(logen(memo,A), B, C) :-
        memo__2(A, B, C).
unfold__14(logen(unfold,A), B, C) :-
        memo__7(A, B, C).
unfold__14(resif(A,B,C), (D->E;F), (G,H,I)) :-
        unfold__14(A, D, G),
        unfold__14(B, E, H),
        unfold__14(C, F, I).
unfold__14(if(A,B,C), D, (E->F,G=D;H,I=D)) :-
        unfold__14(A, _, E),
        unfold__14(B, G, F),
        unfold__14(C, I, H).
unfold__14(resfindall(A,B,C), findall(A,D,C), E) :-
        unfold__14(B, D, E).
unfold__14(hide_nf(A), B, (term_variables(A,C),findall((D,C),E,F),(F=[]->B=fail;G,H))) :-
        unfold__14(A, D, E),
        memo__8(F, C, I, G),
        memo__11(I, B, H).
/*unfold__12(_11394,_11396,_11371)=unfold(unfold(_11394,_11396),_11371)*/
unfold__12(A, B, (C,D)) :-
        unfold__13(_, A, E, C),
        unfold__14(E, B, D).
/*memo__7(_10305,_10307,_10282)=memo(unfold(_10305,_10307),_10282)*/
memo__7(A, B, C) :-
        (   memo_table(unfold(A,B), C) ->
            true
        ;   A=..[D|E],
            length(E, F),
            length(G, F),
            H=..[D|G],
            append(G, [I], J),
            gensym__7(K),
            L=..[K|J],
            assert(memo_table(unfold(H,I),L)),
            findall((L:-M), unfold__12(H,I,M), N),
            format('/*~k=~k*/~n', [L,unfold(H,I)]),
            pretty_print_clauses__1(N),
            memo_table(unfold(A,B), C)
        ).
/*gensym__12(_11417)=gensym(pretty_print_clauses,_11417)*/
gensym__12(A) :-
        var(A),
        oldvalue__1(pretty_print_clauses, B),
        C is B+1,
        set_flag__1(pretty_print_clauses, C),
        name(C, D),
        name(A, [112,114,101,116,116,121,95,112,114,105,110,116,95,99,108,97,117,115,101,115,95,95|D]).
/*unfold__19(_12023,_12002)=unfold(pretty_print_clauses(_12023),_12002)*/
unfold__19([], true).
unfold__19([A|B], (C,portray_clause(D),E)) :-
        memo__11(A, D, C),
        memo__12(B, E).
/*memo__12(_10993,_10972)=memo(pretty_print_clauses(_10993),_10972)*/
memo__12(A, B) :-
        (   memo_table(pretty_print_clauses(A), B) ->
            true
        ;   gensym__12(C),
            D=..[C,E],
            assert(memo_table(pretty_print_clauses(E),D)),
            findall((D:-F), unfold__19(E,F), G),
            format('/*~k=~k*/~n', [D,pretty_print_clauses(E)]),
            pretty_print_clauses__1(G),
            memo_table(pretty_print_clauses(A), B)
        ).
/*unfold__2(_9220,_9222,_9197)=unfold(memo(_9220,_9222),_9197)*/
unfold__2(A, B, (memo_table(A,B)->true;C,assert(memo_table(D,E)),findall((E:-F),G,H),format('/*~k=~k*/~n',[E,D]),I,memo_table(A,B))) :-
        unfold__3(A, D, E, C),
        memo__7(D, F, G),
        memo__12(H, I).
/*memo__2(_8135,_8137,_8112)=memo(memo(_8135,_8137),_8112)*/
memo__2(A, B, C) :-
        (   memo_table(memo(A,B), C) ->
            true
        ;   A=..[D|E],
            length(E, F),
            length(G, F),
            H=..[D|G],
            append(G, [I], J),
            gensym__2(K),
            L=..[K|J],
            assert(memo_table(memo(H,I),L)),
            findall((L:-M), unfold__2(H,I,M), N),
            format('/*~k=~k*/~n', [L,memo(H,I)]),
            pretty_print_clauses__1(N),
            memo_table(memo(A,B), C)
        ).
/*unfold__1(_7716,_7718,_7693)=unfold(lix(_7716,_7718),_7693)*/
unfold__1(A, B, (retractall(memo_table(_,_)),print('/*Generated by Lix*/\n'),print(':- dynamic flag/2, memo_table/2.\n'),print(':- use_module(library(lists)).\n'),print(':- use_module(library(terms)).\n'),print(':- op(1150, fx, type).\n'),C)) :-
        memo__2(A, B, C).
/*memo__1(_6724,_6726,_6701)=memo(lix(_6724,_6726),_6701)*/
memo__1(A, B, C) :-
        (   memo_table(lix(A,B), C) ->
            true
        ;   A=..[D|E],
            length(E, F),
            length(G, F),
            H=..[D|G],
            append(G, [I], J),
            gensym__1(K),
            L=..[K|J],
            assert(memo_table(lix(H,I),L)),
            findall((L:-M), unfold__1(H,I,M), N),
            format('/*~k=~k*/~n', [L,lix(H,I)]),
            pretty_print_clauses__1(N),
            memo_table(lix(A,B), C)
        ).
/*lix__1(_6476,_6478,_6453)=lix(lix(_6476,_6478),_6453)*/
lix__1(A, B, C) :-
        retractall(memo_table(_,_)),
        print('/*Generated by Lix*/\n'),
        print(':- dynamic flag/2, memo_table/2.\n'),
        print(':- use_module(library(lists)).\n'),
        print(':- use_module(library(terms)).\n'),
        print(':- op(1150, fx, type).\n'),
        memo__1(A, B, C).
cogen(A, B, C) :-
        lix__1(A, B, C).

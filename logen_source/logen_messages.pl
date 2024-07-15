
:- module('logen_messages', [
	    print_error/1, print_message/1, print_short_msg/1,
	    print_error_line_number/1, print_error_line_number/0,
	    print_debug_message/1]).

:- if(current_prolog_flag(dialect, ciao)).
:- use_package(.(sicsignore)).

ciao((:- set_prolog_flag(multi_arity_warnings, off))).
:- endif.

print_error_line_number(Error) :-
    print_error(Error),print_error_line_number.
print_error_line_number :-
    current_stream(_FN,_mode,Stream),
    line_count(Stream,N),
	current_output(X),
	set_output(user_error),
	write('! ###  Error occured at line number: '), write(N), nl,
	set_output(X). 
print_error(Error) :-
	current_output(X),
	set_output(user_error),
	write('! ### '),write(Error), nl,
	set_output(X).
print_message(Msg) :-
	current_output(X),
	set_output(user),
	print_message(informational,Msg),
	set_output(X).

ciao(print_message(informational,Msg)) :- print(Msg),nl.

print_short_msg(Msg) :-
	current_output(X),
	set_output(user),
	write(Msg),
	set_output(X).
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:


%:- use_module('logen_preferences.pl').
get_preference(_,_) :- fail.

print_debug_message(Msg) :-
  ((get_preference(gx_debug_mode,C), C>0)
     -> print_message(Msg) ; true).
:- use_module(library(interpolate)).

% Unlike quasiquotation, the compiler can't detect singletons inside plain
% strings.  So we have to either disable singleton detection, mark each
% variable as a singleton or use the variable twice.
:- style_check(-singleton).

:- use_module(library(tap)).

'typical usage (double quotes)' :-
    Name = `Michael`,
    "Hello, $Name" == "Hello, Michael".

'leading variable (backticks)' :-
    Position = first,
    `$Position thing to do` == `first thing to do`.

'trailing variable (single quotes)' :-
    End = "end",
    'at the $End' == 'at the end'.

'looks like a variable, but it is not' :-
    Message = 'This is a $Trick',
    atom(Message).

'money amounts are not interpolations' :-
    Message = "That will be $3.27, please",
    string(Message).

'interpolating numbers' :-
    Answer = 42,
    "... and everything: $Answer" == "... and everything: 42".

'interpolating compound terms' :-
    X = foo(bar),
    "X is $X" == "X is foo(bar)".

% this test seems silly, but some people really use lists of
% atoms to represent a string.  we should honor that lifestyle choice.
'interpolating characters' :-
    Chars = [h,e,l,l,o],
    `$Chars to you` == `hello to you`.

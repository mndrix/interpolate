:- module(interpolate, []).
:- use_module(library(dcg/basics), [prolog_var_name//1, string//1]).

%%	Template(+Vars:pairs, -Formats:list, -Args:list)//
%
%   Parse a list of codes as if it were an interpolated string. Formats
%   is a list of atoms that can be joined together to
%   create the first argument of format/2. Args are values for the
%   second.
template(Vars, [Static,'~p'|Formats], [Arg|Args]) -->
    string(Codes),
    variable(VarName),
    { memberchk(VarName=Arg, Vars) },
    { atom_codes(Static, Codes) },
    template(Vars, Formats, Args).
template(_, [Format], []) -->
    string(Codes),
    { atom_codes(Format, Codes) }.


%%	variable(-VarName:atom)//
%
%	Parse a $-prefixed variable name.  Like `$Message`.
%   For now this is a thin wrapper around prolog_var_name//1 but may
%   eventuall grow to support other kinds of variables so I want the
%   details abstracted away.
variable(VarName) -->
    "$",
    prolog_var_name(VarName).

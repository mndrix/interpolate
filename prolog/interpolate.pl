:- module(interpolate, []).
:- use_module(library(dcg/basics), [string//1]).

%%	Template(+Vars:pairs, -Atoms:list)//
%
%	Parse a list of codes as if it were an interpolated string.
template(Vars, [Atom, Var|Rest]) -->
    string(Codes),
    variable(VarName),
    { memberchk(VarName=Var, Vars) },
    { atom_codes(Atom, Codes) },
    template(Vars, Rest).
template(_, [Atom]) -->
    string(Codes),
    { atom_codes(Atom, Codes) }.


%%	variable(-VarName:atom)//
%
%	Parse a $-prefixed variable name.  Like `$Message`.
%   For now this is a thin wrapper around prolog_var_name//1 but may
%   eventuall grow to support other kinds of variables so I want the
%   details abstracted away.
variable(VarName) -->
    "$",
    prolog_var_name(VarName).

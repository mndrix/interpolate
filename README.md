# Synopsis

    :- use_module(library(interpolate)).
    main :-
        Name = "Johannson",
        Time = evening,
        writeln('Hello Mr. $Name. Isn''t it a fine $Time?').

# Description

[Many
languages](http://rosettacode.org/wiki/String_interpolation_%28included%29)
allow one to construct strings using a built-in, miniature template
language in which the values of variables are inserted into an
otherwise static template.  It's often called string
interpolation.

This module provides similar functionality for Prolog.  It uses the
same syntax as Unix shell, Perl, PHP, Tcl, etc.  Namely, a local
variable name prefixed with `$`.  Interpolation is supported in all of
the following string types:

    * single quoted strings (atoms)
    * double quoted strings (depends on `double_quotes` flag)
    * backquoted strings (string)

Internally the string is constructed with `format/2`.  If a variable's
value is a list of codes or a list of chars (single letter atoms), we
use the ~s format sequence; otherwise, ~p.  This allows compound
terms to define their own string representation via the `portray/1`
hook.

If a string refers to a variable that doesn't exist, that portion of
the string is left alone.  This behavior may change.  In the future it
may generate a warning.

# Singleton Warnings

Because interpolation hides variables inside of strings, the compiler
can't detect that they're being used.  This often results in unwanted
singleton warnings.  You have some choices in how to resolve the
problem:

  * prefix the affected variable with an underscore
  * use [library(func)](http://www.swi-prolog.org/pack/list?p=func) string interpolation
  * `:- style_check(-singleton).`

Once I add quasiquotation support to this module, that will be
another option.

# Changes in this Version

    * First public release

# Installation

Using SWI-Prolog 7.1.1 or later:

    ?- pack_install(interpolate).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/interpolate

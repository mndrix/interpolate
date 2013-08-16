:- use_module(library(interpolate)).

:- use_module(library(tap)).

once(
    phrase(
        interpolate:template(
            ['Ho'=Ho],
            ['','~p',' ','~p',' ','~p',''],
            [Ho, Ho, Ho]
        ),
        "$Ho $Ho $Ho"
    )
).

:- dynamic(variable/2).

start :-
    write('Simple Calculator in Prolog'), nl,
    write('Type "quit." to exit. Do not forget to add . in the end of the line.'), nl,
    calc_loop,
    true.

calc_loop :-
    write('Calc> '),
    read(Input),
    ( Input == end_of_file
      -> true
      ; Input == quit
        -> write('Exiting calculator.'), nl, true
        ; process(Input),
          calc_loop
    ).

process(Input) :-
    ( Input = (Var = Expr)
      -> eval(Expr, Value),
         retractall(variable(Var, _)),  % Remove previous assignment if any
         assert(variable(Var, Value)),
         format('Assigned ~w = ~w~n', [Var, Value])
      ;  eval(Input, Value),
         format('Result: ~w~n', [Value])
    ).

eval(Number, Number) :-
    number(Number).

eval(Var, Value) :-
    atom(Var),
    variable(Var, Value),
    !.
eval(Var, _) :-
    atom(Var),
    \+ variable(Var, _),
    format('Error: variable ~w not defined.~n', [Var]),
    fail.

eval(Expr, Value) :-
    Expr =.. [Op, Left, Right],
    member(Op, ['+', '-', '*', '/']),
    eval(Left, LVal),
    eval(Right, RVal),
    compute(Op, LVal, RVal, Value).

compute('+', L, R, Value) :- Value is L + R.
compute('-', L, R, Value) :- Value is L - R.
compute('*', L, R, Value) :- Value is L * R.
compute('/', L, R, Value) :-
    ( R =:= 0
      -> ( write('Error: Division by zero.'), nl, fail )
      ;  Value is L / R
    ).

:- initialization(start).

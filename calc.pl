:- dynamic(variable/2).

% Entry point
start :-
    write('Simple Calculator in Prolog'), nl,
    write('Type "quit." to exit. Do not forget to add . in the end of the line.'), nl,
    calc_loop,
    true.

% Interactive input loop with end-of-file check
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

% Process the user input:
% If the input is an assignment (e.g., X = 3+2),
% evaluate the expression, store the variable, and print the result.
% Otherwise, evaluate it as an expression.
process(Input) :-
    ( Input = (Var = Expr)
      -> eval(Expr, Value),
         retractall(variable(Var, _)),  % Remove previous assignment if any
         assert(variable(Var, Value)),
         format('Assigned ~w = ~w~n', [Var, Value])
      ;  eval(Input, Value),
         format('Result: ~w~n', [Value])
    ).

% Evaluate arithmetic expressions

% A number evaluates to itself.
eval(Number, Number) :-
    number(Number).

% If the expression is a variable (atom), look it up in our dynamic storage.
eval(Var, Value) :-
    atom(Var),
    variable(Var, Value),
    !.
% If a variable is referenced but not assigned, signal an error.
eval(Var, _) :-
    atom(Var),
    \+ variable(Var, _),
    format('Error: variable ~w not defined.~n', [Var]),
    fail.

% Evaluate a binary arithmetic operation.
eval(Expr, Value) :-
    Expr =.. [Op, Left, Right],
    member(Op, ['+', '-', '*', '/']),
    eval(Left, LVal),
    eval(Right, RVal),
    compute(Op, LVal, RVal, Value).

% Compute the result of a binary operation.
compute('+', L, R, Value) :- Value is L + R.
compute('-', L, R, Value) :- Value is L - R.
compute('*', L, R, Value) :- Value is L * R.
compute('/', L, R, Value) :-
    ( R =:= 0
      -> ( write('Error: Division by zero.'), nl, fail )
      ;  Value is L / R
    ).

% Automatically start the calculator when the file is loaded.
:- initialization(start).

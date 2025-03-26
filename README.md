# BIL395-HW2

This github repository includes 2024-2025 Spring BIL395-Programming Language course's second homework. In this homework, I implemented simple calculator that includes variables, assigments, simple arithmetics and simple error handling with five different language. In repository, you can find calculator.pl, calc.pl (since Prolog and Perl have same extension I named Prolog one as calc), calculator.rs, calculator.scm, calculator.adb and test_cases.txt for checking my calculators's accuracy. Below you can find implementation decisions, simple code structure explanation, running and output of the programs for each language. All calculators takes inlines expressions, do not care about whitespaces and can result and operate floating point numbers.

# Rust

This Rust code implements a simple REPL (Read–Evaluate–Print Loop). In `main`, the program reads user input line by line and directs each line to `evaluate_line`, which checks if it is an assignment (contains =) or an arithmetic expression. Assignments stored in a HashMap data structure that tracks variables. Expressions are processed by `evaluate_expression`, which breaks them down into tokens with `tokenize`, converts these tokens into Reverse Polish Notation (RPN) `to_rpn`, and finally evaluates the RPN with `eval_rpn`. Each function focuses on a specific task: parsing input into tokens, handling operator precedence, and managing a stack-based RPN evaluation. Code also does the simple error handling includes Invalid Expression, Division By Zero, Not Enough Space In Stack, Unknown Variable, Mismatch Paranthesis.

I compiled and executed with online editor. You can access it via https://www.tutorialspoint.com/compile_rust_online.php.

Output of code:

<img width="199" alt="image" src="https://github.com/user-attachments/assets/f47ec8fb-da73-4688-bf52-687005967271" />

# ADA

This ADA implements a simple REPL (Read–Evaluate–Print Loop) calculator. The main loop repeatedly reads user input and uses `Get_Token` to break the input into tokens. If the first token is an identifier followed by =, the program interprets the input as an assignment, storing the evaluated expression's result in a fixed-size array using `Set_Variable`. Otherwise, the code treats the input as an expression and uses a "recursive-descent parser" (`Parse_Expression`, `Parse_Term`, and `Parse_Factor`) to compute the result. Variables are resolved by `Get_Variable`, which checks if a name is defined, while division by zero or missing parentheses trigger error messages. Code also does the simple error handling includes Invalid Expression, Division By Zero, Not Enough Space In Stack, Unknown Variable, Mismatch Paranthesis.

I compiled and executed with online editor. You can access it via https://www.tutorialspoint.com/compile_ada_online.php.

Output of code:

<img width="232" alt="image" src="https://github.com/user-attachments/assets/9cd858b0-c953-4f60-91e5-699794965efe" />

# Perl

This Perl code implements a simple REPL (Read–Evaluate–Print Loop) calculator. In the main loop, the program prompts the user for input, checks if it is an assignment (e.g., x = 3 + 4), and if so, stores the evaluated result in a global hash table. Otherwise, the line is processed as an expression by a "recursive-descent parser" made up of `parse_expression`, `parse_term`, and `parse_factor`. Parentheses and operator precedence are respected through carefully splitting the input text with `split_by_operators`. Error handling is achieved by wrapping the expression-processing calls in an `eval { ... }` block and capturing exceptions via $@; each exception string is tagged by `make_error` to indicate errors. These tags are extracted in `parse_error` to display. These simple error handling includes Invalid Expression, Division By Zero, Not Enough Space In Stack, Unknown Variable, Mismatch Paranthesis.

I compiled and executed with online editor. You can access it via https://www.tutorialspoint.com/execute_perl_online.php.

Output of code:

<img width="374" alt="image" src="https://github.com/user-attachments/assets/286cdc1e-83f3-4ce3-844f-fe499c91e5ea" />

# Scheme

This Scheme code implements a straightforward REPL (Read–Evaluate–Print Loop) calculator. In the calculator function, the user is prompted for input, which is then split into tokens by `tokenize`. These tokens are processed by `parse`. The code stores variables in a list, with lookup retrieving existing values and store adding new ones or updating existing entries. The `evaluate` function then interprets the expression tree, performing basic arithmetic or defining variables if an assignment operator (=) is encountered. While it correctly handles symbol lookups and simple arithmetic (+, –, *, /), it does not implement operator precedence or parenthesized expressions. Any badly formed input or undefined variable access is caught by a `with-exception-handler` block, generating an error message without halting the program. These simple error handling includes Invalid Expression, Division By Zero, Unknown Variable.

For compiling I tried various compilers that includes racket, mit-scheme, online compilers however, they do not work. Lastly, I was able to compile and took output in online compiler jdoodle. You can access it via https://www.jdoodle.com/execute-scheme-online.

Output of code:

<img width="253" alt="image" src="https://github.com/user-attachments/assets/8bfb5216-6d3f-4bc3-8157-5b51379637f6" />

# Prolog

This Prolog code implements a simple interactive calculator in Prolog. The main entry point is `start/0`, which prints an introduction and then calls `calc_loop/0`. Within the loop, the program reads user input, checks for a quit or end-of-file condition, and otherwise delegates to `process/1` to either handle an assignment or evaluate an expression. Variable assignments are stored dynamically with `variable/2`, allowing reassignment. The core expression evaluation is implemented by `eval/2`, which handles numbers directly, resolves variable references and reports an error if a variable is undefined, and recursively processes binary operations by breaking them down (Expr =.. [Op, Left, Right]) and delegating to `compute/4`. Division by zero triggers an error message and a fail to prevent proceeding with an invalid result. This code detects many other errors however, only gives error message to Division By Zero, Variable Not Defined.

Unfortunately online editors so not support Prolog. Thus I installed swi-prolog to run the code. Below is a steps to run the code. 

Open terminal and cd to the directory where prolog file is.
swipl
[calc].

Output of code:

<img width="492" alt="image" src="https://github.com/user-attachments/assets/cd5b850d-deaf-4ca0-96bb-a1fa3bef3bbb" />

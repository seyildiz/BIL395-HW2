use std::collections::HashMap;
use std::io::{self, Write};

fn main() {
    let mut variables = HashMap::<String, f64>::new();
    println!("Simple Calculator in Rust. Type an expression");
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            println!("Error reading input.");
            continue;
        }

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        // Exit condition
        if input.eq_ignore_ascii_case("exit") || input.eq_ignore_ascii_case("q") {
            break;
        }

        // Try evaluating
        match evaluate_line(input, &mut variables) {
            Ok(value) => println!("{}", value),
            Err(e) => println!("Error: {}", e),
        }
    }
}

/// Main dispatcher: figure out if it's an assignment or just an expression
fn evaluate_line(line: &str, variables: &mut HashMap<String, f64>) -> Result<f64, String> {
    // If there's an '=' in the line, treat as an assignment
    if let Some(eq_index) = line.find('=') {
        // Split into "var = expr"
        let var_name = line[..eq_index].trim();   // e.g. "y"
        let expr_part = line[eq_index + 1..].trim(); // e.g. "6 / 5"

        // Validate the variable name
        if var_name.is_empty() {
            return Err("No variable name before '='".to_string());
        }

        // Evaluate the expression on the right side
        let value = evaluate_expression(expr_part, variables)?;
        // Store it
        variables.insert(var_name.to_string(), value);
        Ok(value)
    } else {
        // Otherwise, just evaluate the line as an expression
        evaluate_expression(line, variables)
    }
}

/// Evaluate a single expression (like "5 * 3 - 4 + 2") and return the numeric result.
fn evaluate_expression(expr: &str, variables: &mut HashMap<String, f64>) -> Result<f64, String> {
    // 1. Tokenize the expression
    let tokens = tokenize(expr)?;
    if tokens.is_empty() {
        return Err("Empty expression".to_string());
    }

    // 2. Convert tokens -> RPN using shunting yard
    let rpn = to_rpn(&tokens, variables)?;

    // 3. Evaluate the RPN
    eval_rpn(&rpn, variables)
}

/// Tokenize the string into a Vec of tokens.
/// e.g. "3.14 + y * ( 1 - 2 )" -> ["3.14", "+", "y", "*", "(", "1", "-", "2", ")"].
fn tokenize(line: &str) -> Result<Vec<String>, String> {
    let mut tokens = Vec::new();
    let mut number_buf = String::new();

    let mut chars = line.chars().peekable();
    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() {
            chars.next();
        } else if ch.is_ascii_digit() || ch == '.' {
            // Build up a number (which may have multiple digits, decimal point, etc.)
            number_buf.clear();
            while let Some(&ch2) = chars.peek() {
                if ch2.is_ascii_digit() || ch2 == '.' {
                    number_buf.push(ch2);
                    chars.next();
                } else {
                    break;
                }
            }
            tokens.push(number_buf.clone());
        } else if ch.is_ascii_alphabetic() {
            // Build up an identifier (variable name)
            number_buf.clear();
            while let Some(&ch2) = chars.peek() {
                if ch2.is_ascii_alphanumeric() || ch2 == '_' {
                    number_buf.push(ch2);
                    chars.next();
                } else {
                    break;
                }
            }
            tokens.push(number_buf.clone());
        } else {
            // Assume it's an operator or paren
            tokens.push(ch.to_string());
            chars.next();
        }
    }

    Ok(tokens)
}

/// Convert list of tokens to Reverse Polish notation using the Shunting Yard algorithm.
fn to_rpn(tokens: &[String], variables: &HashMap<String, f64>) -> Result<Vec<String>, String> {
    // We only handle +, -, *, /, parentheses, and variables/numbers
    // Operator precedence map
    let precedence = |op: &str| match op {
        "+" | "-" => 1,
        "*" | "/" => 2,
        _ => 0,
    };

    let mut output_queue: Vec<String> = Vec::new();
    let mut operator_stack: Vec<String> = Vec::new();

    for token in tokens {
        if let Ok(_num) = token.parse::<f64>() {
            // It's a number
            output_queue.push(token.clone());
        } else if variables.contains_key(token) {
            // It's a known variable name
            output_queue.push(token.clone());
        } else if token == "(" {
            operator_stack.push(token.clone());
        } else if token == ")" {
            // Pop from operator stack to output until "("
            while let Some(top) = operator_stack.pop() {
                if top == "(" {
                    break;
                }
                output_queue.push(top);
            }
        } else if ["+", "-", "*", "/"].contains(&token.as_str()) {
            // Operator
            let current_precedence = precedence(token);
            // Pop higher-precedence ops from the stack
            while let Some(top) = operator_stack.last() {
                if ["+", "-", "*", "/"].contains(&top.as_str())
                    && precedence(top) >= current_precedence
                {
                    output_queue.push(operator_stack.pop().unwrap());
                } else {
                    break;
                }
            }
            operator_stack.push(token.clone());
        } else {
            output_queue.push(token.clone());
        }
    }

    // Pop remaining operators
    while let Some(op) = operator_stack.pop() {
        if op == "(" || op == ")" {
            return Err("Mismatched parentheses".to_string());
        }
        output_queue.push(op);
    }

    Ok(output_queue)
}

/// Evaluate an RPN expression and return the result.
/// This is a standard stack-based RPN evaluator.
fn eval_rpn(rpn: &[String], variables: &HashMap<String, f64>) -> Result<f64, String> {
    let mut stack = Vec::new();

    for token in rpn {
        if let Ok(num) = token.parse::<f64>() {
            // It's a literal number
            stack.push(num);
        } else if ["+", "-", "*", "/"].contains(&token.as_str()) {
            // Pop the top two values
            if stack.len() < 2 {
                return Err("Not enough values on stack for operator".to_string());
            }
            let b = stack.pop().unwrap();
            let a = stack.pop().unwrap();

            let result = match token.as_str() {
                "+" => a + b,
                "-" => a - b,
                "*" => a * b,
                "/" => {
                    if b == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    a / b
                }
                _ => unreachable!(),
            };
            stack.push(result);
        } else {
            // It's presumably a variable name
            if let Some(&val) = variables.get(token) {
                stack.push(val);
            } else {
                return Err(format!("Unknown variable: {}", token));
            }
        }
    }

    // Final result should be the only thing left on the stack
    if stack.len() == 1 {
        Ok(stack[0])
    } else {
        Err("Invalid RPN expression (stack has extra values)".to_string())
    }
}
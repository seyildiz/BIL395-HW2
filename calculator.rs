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

        match evaluate_line(input, &mut variables) {
            Ok(value) => println!("{}", value),
            Err(e) => println!("Error: {}", e),
        }
    }
}

fn evaluate_line(line: &str, variables: &mut HashMap<String, f64>) -> Result<f64, String> {
    if let Some(eq_index) = line.find('=') {
        let var_name = line[..eq_index].trim();
        let expr_part = line[eq_index + 1..].trim();

        if var_name.is_empty() {
            return Err("No variable name before '='".to_string());
        }

        let value = evaluate_expression(expr_part, variables)?;
        variables.insert(var_name.to_string(), value);
        Ok(value)
    } else {
        evaluate_expression(line, variables)
    }
}

fn evaluate_expression(expr: &str, variables: &mut HashMap<String, f64>) -> Result<f64, String> {
    let tokens = tokenize(expr)?;
    if tokens.is_empty() {
        return Err("Empty expression".to_string());
    }

    let rpn = to_rpn(&tokens, variables)?;

    eval_rpn(&rpn, variables)
}


fn tokenize(line: &str) -> Result<Vec<String>, String> {
    let mut tokens = Vec::new();
    let mut number_buf = String::new();

    let mut chars = line.chars().peekable();
    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() {
            chars.next();
        } else if ch.is_ascii_digit() || ch == '.' {
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
            tokens.push(ch.to_string());
            chars.next();
        }
    }

    Ok(tokens)
}

fn to_rpn(tokens: &[String], variables: &HashMap<String, f64>) -> Result<Vec<String>, String> {
    let precedence = |op: &str| match op {
        "+" | "-" => 1,
        "*" | "/" => 2,
        _ => 0,
    };

    let mut output_queue: Vec<String> = Vec::new();
    let mut operator_stack: Vec<String> = Vec::new();

    for token in tokens {
        if let Ok(_num) = token.parse::<f64>() {
            output_queue.push(token.clone());
        } else if variables.contains_key(token) {
            output_queue.push(token.clone());
        } else if token == "(" {
            operator_stack.push(token.clone());
        } else if token == ")" {
            while let Some(top) = operator_stack.pop() {
                if top == "(" {
                    break;
                }
                output_queue.push(top);
            }
        } else if ["+", "-", "*", "/"].contains(&token.as_str()) {
            let current_precedence = precedence(token);
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

    while let Some(op) = operator_stack.pop() {
        if op == "(" || op == ")" {
            return Err("Mismatched parentheses".to_string());
        }
        output_queue.push(op);
    }

    Ok(output_queue)
}

fn eval_rpn(rpn: &[String], variables: &HashMap<String, f64>) -> Result<f64, String> {
    let mut stack = Vec::new();

    for token in rpn {
        if let Ok(num) = token.parse::<f64>() {
            stack.push(num);
        } else if ["+", "-", "*", "/"].contains(&token.as_str()) {
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
            if let Some(&val) = variables.get(token) {
                stack.push(val);
            } else {
                return Err(format!("Unknown variable: {}", token));
            }
        }
    }

    if stack.len() == 1 {
        Ok(stack[0])
    } else {
        Err("Invalid RPN expression (stack has extra values)".to_string())
    }
}

use strict;
use warnings;

# Simple calculator with variables and basic arithmetic

# Store variables
my %variables;

# Error types
use constant {
    PARSE_ERROR => 'parse',
    UNKNOWN_VAR => 'unknown_var',
    DIV_BY_ZERO => 'div_by_zero',
};

# Main loop
print "Simple Calculator in Perl\n";
print "Type 'exit' or 'q' to exit or type an expression\n";

while (1) {
    print "> ";
    my $input = <STDIN>;
    chomp $input;
    
    last if $input =~ /^(exit|q)$/i;
    
    if ($input =~ /^\s*$/) {
        next;  # Skip empty lines
    }
    
    eval {
        my $result = process_line($input);
        if (defined $result) {
            printf("= %s\n", $result);
        }
    };
    if ($@) {
        my ($type, $msg) = parse_error($@);
        print "Error: $msg\n";
    }
}

print "Goodbye!\n";

# Process a line of input
sub process_line {
    my $line = shift;
    
    # Check for assignment
    if ($line =~ /^\s*([a-zA-Z][a-zA-Z0-9_]*)\s*=\s*(.+)$/) {
        my $var_name = $1;
        my $expr = $2;
        
        my $value = evaluate_expression($expr);
        $variables{$var_name} = $value;
        return $value;
    }
    
    # Otherwise evaluate expression
    return evaluate_expression($line);
}

# Show all variables
sub show_variables {
    print "Variables:\n";
    foreach my $name (sort keys %variables) {
        printf("%s = %s\n", $name, $variables{$name});
    }
}

# Evaluate an expression
sub evaluate_expression {
    my $expr = shift;
    
    $expr =~ s/^\s+|\s+$//g;  # trim whitespace
    
    # Empty expression
    if ($expr eq '') {
        die make_error(PARSE_ERROR, "Empty expression");
    }
    
    return parse_expression($expr);
}

# Recursive descent parser functions

sub parse_expression {
    my $expr = shift;
    
    # Split by + and - operators (outside of parentheses)
    my @parts = split_by_operators($expr, qr/[+\-]/);
    
    # If there's only one part, it's a term
    if (@parts == 1) {
        return parse_term($parts[0]->{text});
    }
    
    # Process terms with operators
    my $result = parse_term($parts[0]->{text});
    
    for (my $i = 1; $i < @parts; $i++) {
        my $term = parse_term($parts[$i]->{text});
        my $op = $parts[$i]->{op};
        
        if ($op eq '+') {
            $result += $term;
        } elsif ($op eq '-') {
            $result -= $term;
        }
    }
    
    return $result;
}

sub parse_term {
    my $term = shift;
    
    $term =~ s/^\s+|\s+$//g;  # trim whitespace
    
    # Split by * and / operators (outside of parentheses)
    my @parts = split_by_operators($term, qr/[*\/]/);
    
    # If there's only one part, it's a factor
    if (@parts == 1) {
        return parse_factor($parts[0]->{text});
    }
    
    # Process factors with operators
    my $result = parse_factor($parts[0]->{text});
    
    for (my $i = 1; $i < @parts; $i++) {
        my $factor = parse_factor($parts[$i]->{text});
        my $op = $parts[$i]->{op};
        
        if ($op eq '*') {
            $result *= $factor;
        } elsif ($op eq '/') {
            if ($factor == 0) {
                die make_error(DIV_BY_ZERO, "Division by zero");
            }
            $result /= $factor;
        }
    }
    
    return $result;
}

sub parse_factor {
    my $factor = shift;
    
    $factor =~ s/^\s+|\s+$//g;  # trim whitespace
    
    # Handle parenthesized expressions
    if ($factor =~ /^\((.+)\)$/) {
        return parse_expression($1);
    }
    
    # Handle numbers
    if ($factor =~ /^-?\d+(\.\d+)?$/) {
        return $factor;
    }
    
    # Handle variables
    if ($factor =~ /^([a-zA-Z][a-zA-Z0-9_]*)$/) {
        my $var_name = $1;
        if (exists $variables{$var_name}) {
            return $variables{$var_name};
        } else {
            die make_error(UNKNOWN_VAR, "Unknown variable: $var_name");
        }
    }
    
    die make_error(PARSE_ERROR, "Invalid expression: $factor");
}

# Helper function to split by operators while respecting parentheses
sub split_by_operators {
    my ($text, $op_pattern) = @_;
    
    my @parts;
    my $current = '';
    my $paren_count = 0;
    my $prev_op = '';
    
    # Process the first character separately to handle unary operators
    if ($text =~ /^\s*([+\-])(.*)/s) {
        my $op = $1;
        $text = $2;
        if ($op eq '-') {
            $current = '-';
        }
    }
    
    for (my $i = 0; $i < length($text); $i++) {
        my $char = substr($text, $i, 1);
        
        if ($char eq '(') {
            $paren_count++;
            $current .= $char;
        } 
        elsif ($char eq ')') {
            $paren_count--;
            $current .= $char;
        } 
        elsif ($char =~ $op_pattern && $paren_count == 0) {
            # Found an operator outside of parentheses
            push @parts, { text => $current, op => $prev_op };
            $current = '';
            $prev_op = $char;
        } 
        else {
            $current .= $char;
        }
    }
    
    # Add the last part
    push @parts, { text => $current, op => $prev_op };
    
    # Remove the first empty part if there is no leading operator
    if (@parts > 0 && $parts[0]->{text} eq '' && $parts[0]->{op} eq '') {
        shift @parts;
    }
    
    return @parts;
}

# Create error messages
sub make_error {
    my ($type, $msg) = @_;
    return "$type:$msg";
}

# Parse error messages
sub parse_error {
    my $error_msg = shift;
    
    if ($error_msg =~ /^([^:]+):(.*)$/) {
        return ($1, $2);
    }
    
    return ('unknown', $error_msg);
}
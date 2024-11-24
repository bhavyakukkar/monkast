syntax statement  -> 1 = expr1 ";" expr2;
syntax statement  -> 1 = expr1 ";";

syntax paren <- 70 = "(" expr ")";

# arithmetic operators
syntax add <- 41 = lhs "+" rhs;
syntax sub <- 41 = lhs "-" rhs;
syntax mul <- 42 = lhs "*" rhs;
syntax div <- 42 = lhs "/" rhs;
syntax mod <- 42 = lhs "%" rhs;
syntax pow <- 43 = lhs "^" rhs;

syntax eq  -> 51 = lhs "==" rhs;
syntax neq -> 51 = lhs "!=" rhs;
syntax gt  -> 51 = lhs ">" rhs;
syntax gte -> 51 = lhs ">=" rhs;
syntax lt  -> 51 = lhs "<" rhs;
syntax lte -> 51 = lhs "<=" rhs;

syntax fn         -> 3 = "fn" "(" args ")" "{" body "}";
syntax fn         -> 3 = "fn" "(" args ")" "{" "}";
syntax fn         -> 3 = "fn" "(" ")" "{" body "}";
syntax fn         -> 3 = "fn" "(" ")" "{" "}";
syntax bind_value <- 2 = "let" binding "=" value;
syntax reassign   <- 2 = binding "=" value;

# tuples
syntax tuple <- 11 = expr1 "," expr2;

# maps
syntax map       <- 21 = "{" fields "}";
syntax map_field <- 22 = key ":" value;

# array
syntax arr       <- 21 = "[" items "]";
syntax arr_index <- 31 = arr "[" index "]";

# functions
syntax fn_call <- 61 = fn_bind "(" args ")";
syntax fn_call <- 61 = fn_bind "(" ")";

# conditionals
syntax if <- 31 = "if" "(" condition ")" "{" if_expr "}" "else" "{" else_expr "}";
syntax if <- 31 = "if" "(" condition ")" "{" if_expr "}";

syntax return <- 32 = "return" expr;
syntax return <- 32 = "return";

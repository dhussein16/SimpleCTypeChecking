// Analyzer for SimpleC programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid SimpleC program.
//
// Modified by:
//   Dua'a Hussein 655469322
//   the analyze is supposed to check for the type checking to make sure that the 
//   type of the variable or literal matches that what was being expected. like when
//   you try to compare an int to a string returns an error  

// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024


// a question to whoever grades this, isn't it kind of redundant? I
// feel like there are better ways for this than altering the same code over and over???

namespace compiler

module checker =
    // similar to analyzer and parser, we need these for 
    // the code's type checking
    // Function for matching tokens
    let private matchToken expected_token (tokens: string list) =
        List.tail tokens

    // check if an element exists in the symbol table
    // it will return false if it does not exist
    let rec contains (x, y) L =
        match L with
        | [] -> false
        | (name, the_type)::tail when name = x -> true
        | _::tail -> contains (x, y) tail

    // get the type of an element from the symbol table
    // it returns "issue" if something is wrong
    let rec contains_type (x, y) L =
        match L with
        | [] -> "issue"
        | (name, the_type)::tail when name = x -> the_type
        | _::tail -> contains_type (x, y) tail

    // parse the expression values, use the matchtoken function to make sure everything matches
    // extracts and analyzes expression values from the tokens
    // Returns A tuple containing the remaining tokens after parsing and the type of the expression, the leftover tokens
    // and the exprpession type 
    let rec private expr_value tokens symboltable =
        let next = List.head tokens
        // the following pattern matching is to make sure that the next token that is being
        // evaluated is matched to its correct expression, which in order is bool, identifier, 
        // and the literals
        match next with
        | "false" -> (matchToken "false" tokens, "bool")
        | "true" -> (matchToken "true" tokens, "bool")
        | _ when next.StartsWith("identifier") ->
            let var_name = next.Substring(11)
            let T2 = matchToken "identifier" tokens
            if contains (var_name, "type") symboltable = false then
                failwith("variable '" + var_name + "' undefined")
            else
                // Get the type of the variable from the symbol table
                let the_type = contains_type (var_name, "type") symboltable
                (T2, the_type)
        | _ when next.StartsWith("int_literal") -> (matchToken "int_literal" tokens, "int")
        | _ when next.StartsWith("str_literal") -> (matchToken "str_literal" tokens, "str")
        | _ when next.StartsWith("real_literal") -> (matchToken "real_literal" tokens, "real")
        | _ -> (tokens, "not")

    // parse expression operators with matchToken
    let rec private expr_op tokens =
        let next = List.head tokens
        match next with
        | "+" | "-" | "*" | "/" | "^" | "<" | "<=" | ">" | ">=" | "==" | "!=" -> matchToken next tokens
        | _ -> tokens

    // parse expressions with matchToken 
    let rec private expr tokens symboltable =
        // Parse the left-hand side of the expression
        let (T2, the_type_left) = expr_value tokens symboltable
        let next = List.head T2
        // Check if the next token is an operator
        if next = "+" || next = "-" || next = "*" || next = "/" || next = "^" ||
           next = "<" || next = "<=" || next = ">" || next = ">=" || next = "==" ||
           next = "!=" then
            // Parse the operator and right-hand side of the expression
            let T3 = expr_op T2
            let (T4, the_type_right) = expr_value T3 symboltable
            // Parse the operator and right-hand side of the expression
            if next = "+" || next = "-" || next = "*" || next = "/" || next = "^" then
                if (the_type_left = "real" || the_type_left = "int") && (the_type_left = the_type_right) then
                    (T4, the_type_left)
                else
                    failwith("operator " + next + " must involve 'int' or 'real'")
            else
                // check with the compariosn operators
                if next = "<" || next = "<=" || next = ">" || next = ">=" || next = "==" || next = "!=" then
                    if (next = "==") && (the_type_left = "real") && (the_type_right = "real") then
                        printfn "warning: comparing real numbers with == may never be true"
                    if (the_type_left = the_type_right) then
                        (T4, "bool")
                    else
                        failwith("type mismatch '" + the_type_left + "' " + next + " '" + the_type_right + "'")
                else
                    (T4, "Issue, wrong type")
        else
            (T2, the_type_left) // Return the original tokens and the type if no operator is found

    // parse the empty statements
    let rec private empty tokens =
        let T2 = matchToken ";" tokens
        T2

    // parse the variable declarations with matchToken
    // This function analyzes variable declarations by extracting type information and variable names.
    let rec private vardecl tokens symboltable =
        // Extract the type of the variable declaration
        let the_type:string = List.head tokens
        // Handle integer variable declaration with the keyword "int"
        if the_type = "int" then
            let T2 = matchToken "int" tokens
            let var_name = List.head T2
            let new_var_name = var_name.Substring(11)
            let added_var = (new_var_name, "int")

            let T3 = matchToken "identifier" T2
            let T4 = matchToken ";" T3
            // Return the remaining tokens and the updated symbol table with the new variable
            (T4, added_var::symboltable)
        else
            // hadndling the variable declarations with the keyword "real"
            let T2 = matchToken "real" tokens
            let var_name = List.head T2
            let new_var_name = var_name.Substring(11)
            let added_var = (new_var_name, "real")
            let T3 = matchToken "identifier" T2
            let T4 = matchToken ";" T3
            (T4, added_var::symboltable) // Return the remaining tokens and the updated symbol table with the new variable

    // parse input statements with MatchToken to validate if the cin is in the correct structure
    // and returns the remaining tokens after parsing 
    let rec private input tokens symboltable =
        let T2 = matchToken "cin" tokens
        let T3 = matchToken ">>" T2
        let checking = List.head T3
        if contains (checking.Substring(11), "type") symboltable = false then
            failwith("variable '" + checking.Substring(11) + "' undefined")
        let T4 = matchToken "identifier" T3
        let T5 = matchToken ";" T4
        T5

    // parse output value statements with matchToken
    let rec private output_value tokens symboltable =
        let next = List.head tokens
        if next = "endl" then
            let T2 = matchToken "endl" tokens
            T2
        else
            let (T2, the_type) = expr_value tokens symboltable
            T2

    // parse output statements with matchToken and returns the remaining tokens
    let rec private output tokens symboltable =
        let T2 = matchToken "cout" tokens
        let T3 = matchToken "<<" T2
        let T4 = output_value T3 symboltable
        let T5 = matchToken ";" T4
        T5

    // Parse assignment statements using MatchToken.
    // This function validates and parses assignment statements, making sure there correct syntax and type compatibility.
    let rec private assignment tokens symboltable =
        let hd:string = List.head tokens
        let var_name = hd.Substring(11)
        let the_type_x = contains_type (var_name, "type") symboltable

        // Raise an error if the variable is undefined
        if the_type_x = "issue" then
            failwith("variable '" + var_name + "' undefined")

        let T2 = matchToken "identifier" tokens
        let T3 = matchToken "=" T2
        let (T4, the_type_y) = expr T3 symboltable
        let T5 = matchToken ";" T4

        // Check type compatibility between the variable and expression
        if (the_type_x = the_type_y) || (the_type_x = "real" && the_type_y = "int") then
            let result = T5
            result |> ignore
        else
            // Raise an error for type mismatch
            failwith("cannot assign '" + the_type_y + "' to variable of type '" + the_type_x + "'")
        // Raise an error for type mismatch
        if contains (var_name, "type") symboltable = false then
            failwith("variable '" + var_name + "' undefined")
        else
            T5 // return the leftover tokens

    // parse statements of the SimpleC
    // This function parses different types of statements based on the first token in the token list.
    let rec private stmt tokens symboltable =
        // Get the first token in the list
        let next = List.head tokens
        
        // using the functions declared above, determine what type the token is and
        // use the appropriate function to parse and check if it is correct type
        // each one of these functions are declared, defined, and commented above
        if next = ";" then
            let T2 = empty tokens
            (T2, symboltable)
        elif next = "int" || next = "real" then
            let (T2, table) = vardecl tokens symboltable
            (T2, table)
        elif next = "cin" then
            let T2 = input tokens symboltable
            (T2, symboltable)
        elif next = "cout" then
            let T2 = output tokens symboltable
            (T2, symboltable)
        elif next.StartsWith("identifier") then
            let T2 = assignment tokens symboltable
            (T2, symboltable)
        elif next = "if" then
            let (T2, table) = ifstmt tokens symboltable
            (T2, table)
        else
            (tokens, symboltable)

    // parse if statements with MatchToken, parsing the if statement
    and private ifstmt tokens symboltable =
        let T2 = matchToken "if" tokens
        let T3 = matchToken "(" T2
        let T4 = condition T3 symboltable
        let T5 = matchToken ")" T4
        let (T6, table) = then_part T5 symboltable
        let (T7, table2) = else_part T6 table
        (T7, table2)

    // parse conditions with MatchToken. checks if the conditional is valid
    and private condition tokens symboltable =
        let (T1, the_type) = expr tokens symboltable
        if the_type <> "bool" then
            failwith("if condition must be 'bool', but found '" + the_type + "'")
        else
            T1

    // parse then parts of if statements
    and private then_part tokens symboltable =
        let (T1, table) = stmt tokens symboltable
        (T1, table)

    // parse else parts of if statements
    and private else_part tokens symboltable =
        let next = List.head tokens
        if next = "else" then
            let T2 = matchToken "else" tokens
            let (T3, table) = stmt T2 symboltable
            (T3, table)
        else
            (tokens, symboltable)

    // parse additional statements that will appear such as the ;, int, real, cin, and cout 
    // and use the stmt and morestmts functions to determine if the types for each
    // token are correct
    let rec private morestmts tokens symboltable =
        let next = List.head tokens
        if next = ";" || next = "int" || next = "real" || next = "cin" || next = "cout" ||
           next.StartsWith("identifier") || next = "if" then
            let (T2, table) = stmt tokens symboltable
            let (T3, table2) = morestmts T2 table
            (T3, table2)
        else
            (tokens, symboltable)

    // parse multiple statements by calling the stmt and morstmts functions
    // to create the tuples of tokens and strings of the SimpleC program and
    // return the tuple containing the remaining tokens after parsing the statements and the updated symbol table.
    let rec private stmts tokens symboltable =
        let (T2, table) = stmt tokens symboltable
        let (T3, table_2) = morestmts T2 table
        (T3, table_2)

    // parse the main program, this is code taken from the Prgram 4 write up with minor updates.
    let private simpleC tokens symboltable =
        let T2 = matchToken "void" tokens
        let T3 = matchToken "main" T2
        let T4 = matchToken "(" T3
        let T5 = matchToken ")" T4
        let T6 = matchToken "{" T5
        let (T7, table) = stmts T6 symboltable
        let T8 = matchToken "}" T7
        let T9 = matchToken "$" T8
        (T9, table)


    // typecheck tokens symboltable
    // Given a list of tokens and a symbol table, type-checks 
    // the program to ensure program's variables and expressions
    // are type-compatible. If the program is valid, returns 
    // the string "success". If the program contains a semantic
    // error or warning, returns a string of the form
    // "type_error: ...".
    // THIS FUNCTION IS NOT TO BE CHANGED ACCORDING TO THE DOCUMENT
    let typecheck tokens symboltable = 
        try
            let T2 = simpleC tokens symboltable
            "success"
        with 
            | ex -> "type_error: " + ex.Message


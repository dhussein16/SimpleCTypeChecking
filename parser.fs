// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Dua'a Hussein dhusse4 6554a69322
// This is meant to act as the parser for SimpleC programs
// that helps to denote if a program was in the correct format 
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022

namespace compiler

module parser =
    // matchToken
    let private matchToken expected_token tokens =
        // if the next token matches the expected token,
        // keep parsing by returning the rest of the tokens.
        // Otherwise throw an exception because there's a
        // syntax error, effectively stopping compilation
        // at the first error.
        let next_token = List.head tokens

        if expected_token = next_token then
            List.tail tokens
        else
            failwith ("Expecting " + expected_token + ", but found " + next_token)

    // Recursive function that matches and looks for comparison of 
    // values like true, false, ints values, etc.
    let rec private expr_value tokens = 
        match List.head tokens with
        | "true" -> matchToken "true" tokens
        | "false" -> matchToken "false" tokens
        | t when t.StartsWith("str_literal:") -> matchToken t tokens
        | t when t.StartsWith("int_literal:") -> matchToken t tokens
        | t when t.StartsWith("real_literal:") -> matchToken t tokens // Adjusted to handle real literals
        | t when t.StartsWith("identifier:") -> matchToken t tokens
        | t -> failwith ("Expecting identifier or literal, but found " + t)

    // Recursive function for matching current token with operators like + - etc
    let private expr_op tokens = 
        match List.head tokens with
        | "+" | "-" | "*" | "/" | "^" | ">" | "<" | ">=" | "<=" | "==" | "!=" ->
            matchToken (List.head tokens) tokens
        | _ ->
            failwith ("Expecting expression operator, but found " + List.head tokens)

    // Recursive function for expressions, similar to above in functionality
    let private expr tokens =   
        let T1 = expr_value tokens
        let T2:string = List.head T1
        // checking for the different type of operators
        if T2 = "+" || T2 = "-" || T2 = "*" || T2 = "/" || T2 = "^" || T2 = "<" || T2 =">" 
                    || T2 = "<=" || T2 = ">=" || T2 = "==" || T2 = "!=" then
            let T3 = expr_op T1
            let T4 = expr_value T3
            T4
        else
            T1

    // Recursive function  to identidfy empty statements or complete statements,
    // usually to look for the semi colon that indicates the end
    let rec private empty tokens = 
        // Check if the token is ";", yes ==> return the rest of the tokens
        match List.head tokens with
        | ";" -> List.tail tokens
        | _ -> failwith ("Expecting ';', but found " + List.head tokens)
    
    // Recursive function to identify variable declarations in a program 
    let  private vardecl tokens =
        let T1 = List.head tokens 
        let T2 = matchToken T1 tokens // Check for "real" keyword
        let T3:string = List.head T2 // :string ==> explicitly say that this is meant to be a string
        if T3.StartsWith("identifier:") then // was told that "StartsWith" is useful for this
            let T4 = matchToken T3 T2
            let T5 = matchToken ";" T4
            T5
            else 
            failwith ("Expecting identifier, but found " + T3)
            
    // identify input statements, lookling for "cin" and " >>" etc
    let rec private input tokens = 
        let T1 = matchToken "cin" tokens // get the cin
        let T2 = matchToken ">>"  T1 // get the >> identifier
        let T3 = List.head T2 // the actual statement
        
        // if it starts with the identifier keyword, use matchToken to compare the expected with actual
        if T3.StartsWith("identifier:") then 
            let T4 = matchToken T3 T2
            let T5 = matchToken ";" T4
            T5
        else 
            failwith ("Expecting identifier, but found " + T3)

    // identify any output values that may occur
    let private output_value tokens =
        let T1:string = List.head tokens 
        if T1.StartsWith("endl") then 
            let T2 = matchToken "endl" tokens 
            T2
        else 
            let T4 = expr_value tokens 
            T4
    
    // identify output statements like cout 
    let rec private output tokens = 
        // Match the token with "cout << <output-value> ;"
        let T2 = matchToken "cout" tokens
        let T3 = matchToken "<<" T2
        let T4 = output_value T3
        let T5 = matchToken ";" T4
        T5

    // identify assignment statements like = for assignments
    let private assignment tokens = 
            let T1:string = List.head tokens 
            if T1.StartsWith("identifier:") then 
                let T2 = matchToken T1 tokens
                let T3 = matchToken "=" T2 
                let T4 = expr T3
                let T5 = matchToken ";" T4
                T5
            else 
                failwith ("Expecting identifier or literal, but found " + T1)
    
    // Get the actual condition part of the if statement 
    let private condition tokens = 
        let T1 = expr tokens 
        T1

    // Recursive function to identify statements
    let rec private stmt tokens = 
        // look for matches between identifiers for statements
        match List.head tokens with
        | ";" -> empty tokens
        | "real" -> vardecl tokens // Adjusted to handle "real" keyword
        | "int" -> vardecl tokens // Also handle "int" keyword
        | "cout" -> output tokens
        | "cin" -> input tokens
        | "if" -> if_stmt tokens
        | t when t.StartsWith("identifier:") -> assignment tokens
        | t -> failwith ("Expecting statement, but found " + t)

    // Function for if statements to check its structure of if ( condition ) and potential else statements
    // its essentially checking for the proper structure of an if statement
    and private if_stmt tokens = 
        let T1 = matchToken "if" tokens 
        let T2 = matchToken "("  T1
        let T3 = condition T2
        let T4 = matchToken ")" T3
        let T5 = then_part T4
        let T6 = List.head T5
        if T6 = "else" then 
            let T7 = else_part T5 
            T7
            else
            T5
    
    // Function for checking the then-part of an if statement
    and private then_part tokens = 
        stmt tokens 

    // Function for checking the else-part of an if statement
    and private else_part tokens = 
        match List.head tokens with
        | "else" -> matchToken "else" tokens |> stmt
        | _ -> tokens

    // Recursive function to identify any additional statements
    // in a manner to parse them
    let rec private morestmts tokens = 
        let T1:string = List.head tokens 
        if T1 = "}" then 
            tokens
        else 
            let T2 = stmt tokens 
            let T3 = morestmts T2
            T3 

    // Recursively parses multiple statements that can occur and calls the
    // stmts and morestmts functions 
    let rec private stmts tokens=
        let T2 = stmt tokens
        let T3 = morestmts T2
        T3
    
    // looking for tokens
    let private simpleC tokens =
        tokens|> matchToken "void"|> matchToken "main"|> matchToken "("|> matchToken ")"|> matchToken "{"|> stmts|> matchToken "}"|> matchToken "$"
    
    // either prints sucess or errors
    let parse tokens = 
        try 
            let _ = simpleC tokens
            "Success!"
        with 
            | ex ->"syntax_error: " + ex.Message

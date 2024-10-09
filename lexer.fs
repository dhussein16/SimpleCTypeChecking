// Lexical analyzer for SimpleC programs.  This component 
// translates the input into a list of lexical units or
// "tokens". Each token is a string, so the end result
// is a list of strings.
//
// Returns: a list of strings, for example:
//   ["void";"main";"(";")";"{";"int";"identifier:x",...]
//
// Modified by:
//   Dua'a Hussein 655469322  dhusse4
//   this is the lexer that is supposed to be able to translate the code into
//   components called tokens so that it can be processed as a list of strings

// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024

namespace compiler

module lexer =
    // the keywords that the compiler should be able to recognize
    let private keywords =
        [ "cin"
          "cout"
          "else"
          "endl"
          "false"
          "if"
          "int"
          "real" // modified to add the real keyword
          "main"
          "true"
          "void" ]

    // any identifiers that the compiler should be able to recognize
    let private identifier_start = ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ])
    let private identifier_chars = ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ])

    // this funciton is used to look up if an id is a keyword or 
    // an identifier for the compiler to recognize
    let rec private lookupKeyword id =
        if (List.contains id keywords) then
            id
        else // not found, so it's an identifier:
            "identifier:" + id


    // a function to read in chars
    let private nextChar (input: System.IO.StreamReader) =
        if input.EndOfStream then '$' else (char (input.Read()))

    // this is a funciton that should behave to skip characters until the end
    // of the line is reached
    let rec private skipRestOfLine input =
        match (nextChar input) with
        | '$' -> ()
        | '\n' -> ()
        | '\r' -> ()
        | _ -> skipRestOfLine input

    // this function just really collects the ID of elements in a line
    let rec private collectID nextc input id =
        if (List.contains nextc identifier_chars) then
            collectID (nextChar input) input (id + (string nextc))
        else
            (id, nextc)

    // this function is to collect the string literals
    let rec private collectStrLiteral nextc input literal =
        match nextc with
        | '"' -> literal // end of string "
        | '\n' -> literal // end of line:
        | '\r' -> literal
        | '$' -> literal // end of file:
        | _ -> collectStrLiteral (nextChar input) input (literal + (string nextc))

    // this function is to collect the integer literal
    let rec private collectIntLiteral nextc input literal =
        if (List.contains nextc [ '0' .. '9' ]) then
            collectIntLiteral (nextChar input) input (literal + (string nextc))
        else
            (literal, nextc)

    // this function is to preoess the next so that the characters, symbols, and
    // other characters that can be valid and appear in the code are processed properly
    // within the lexer. separated by EOF, whitespace, and other elements 
    // such as the identifiers 
    let rec private lexer nextc input tokens =
        match nextc with
        | '$' -> List.rev ("$" :: tokens) // EOF:

        // white space characters being recognized and parsed
        | ' ' -> lexer (nextChar input) input tokens 
        | '\t' -> lexer (nextChar input) input tokens
        | '\n' -> lexer (nextChar input) input tokens
        | '\r' -> lexer (nextChar input) input tokens

        // other elements like paratheses
        | ';' -> lexer (nextChar input) input (";" :: tokens)
        | '(' -> lexer (nextChar input) input ("(" :: tokens)
        | ')' -> lexer (nextChar input) input (")" :: tokens)
        | '{' -> lexer (nextChar input) input ("{" :: tokens)
        | '}' -> lexer (nextChar input) input ("}" :: tokens)
        | '^' -> lexer (nextChar input) input ("^" :: tokens)
        | '+' -> lexer (nextChar input) input ("+" :: tokens)
        | '-' -> lexer (nextChar input) input ("-" :: tokens)
        | '*' -> lexer (nextChar input) input ("*" :: tokens)
        | '/' -> // could be division, or a comment:
            let lookahead = (nextChar input)

            if lookahead = '/' then
                skipRestOfLine input
                lexer (nextChar input) input tokens
            else
                lexer lookahead input ("/" :: tokens)
        | '=' -> // could be =, or ==
            let lookahead = (nextChar input)

            if lookahead = '=' then
                lexer (nextChar input) input ("==" :: tokens)
            else
                lexer lookahead input ("=" :: tokens)
        | '<' -> // could be <, <=, or <<
            let lookahead = (nextChar input)

            if lookahead = '=' then
                lexer (nextChar input) input ("<=" :: tokens)
            else if lookahead = '<' then
                lexer (nextChar input) input ("<<" :: tokens)
            else
                lexer lookahead input ("<" :: tokens)
        | '>' -> // could be >, >=, or >>
            let lookahead = (nextChar input)

            if lookahead = '=' then
                lexer (nextChar input) input (">=" :: tokens)
            else if lookahead = '>' then
                lexer (nextChar input) input (">>" :: tokens)
            else
                lexer lookahead input (">" :: tokens)
        | '!' -> // could be !, or !=
            let lookahead = (nextChar input)

            if lookahead = '=' then
                lexer (nextChar input) input ("!=" :: tokens)
            else
                lexer lookahead input ("!" :: tokens)
        | '&' -> // expecting &&
            let lookahead = (nextChar input)

            if lookahead = '&' then
                lexer (nextChar input) input ("&&" :: tokens)
            else
                lexer lookahead input ("unknown:&" :: tokens)
        | '|' -> // expecting || as the full character
            let lookahead = (nextChar input)

            if lookahead = '|' then
                lexer (nextChar input) input ("||" :: tokens)
            else
                lexer lookahead input ("unknown:|" :: tokens)

        | '"' ->
            // handling string literals
            let literal = collectStrLiteral (nextChar input) input "" // "
            lexer (nextChar input) input (("str_literal:" + literal) :: tokens)

        | _ when List.contains nextc identifier_start ->
            // collect letters and numbers into identifier, either keyword or identifier:
            let (id, lookahead) = collectID (nextChar input) input (string nextc)
            let token = lookupKeyword id // could be keyword or identifier
            lexer lookahead input (token :: tokens)

        | _ when List.contains nextc [ '0' .. '9' ] ->
            // collect digits into an integer literal:
            let (literal, lookahead) = collectIntLiteral (nextChar input) input (string nextc)
            //lookup if it is a real number or integer

            if lookahead = '.' then
                let (literal, lookahead) = collectIntLiteral (nextChar input) input (literal + ".")
                lexer lookahead input (("real_literal:" + literal) :: tokens)
            else
                lexer lookahead input (("int_literal:" + literal) :: tokens)

        | _ -> lexer (nextChar input) input (("unknown:" + (string nextc)) :: tokens)

    // analyzer filename
    // Given a filename representing a SimpleC program, returns
    // a list of tokens, where each token is a sub-list.
    let analyze (filename: string) =
        use input = new System.IO.StreamReader(filename)
        lexer (nextChar input) input []

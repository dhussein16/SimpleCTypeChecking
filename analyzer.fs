// Analyzer for SimpleC programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//   [("x", "int"); ("y", "int"); ("z", "real")]

// Modified by:
//   Dua'a Hussein 655469322
//   The analyzer checks for the semantics of the code, to make sure that it makes sense 
//   and follows the rule of SimpleC. Kind of like Grammarly checking grammar, this is checking
//   the grammar of the code to make sure it makes sense

// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024
 
// you'll need to do the same checks that is done in the parser
// its going to be tedious but it should act in a similar manner but
// analyze it for semantics, not syntax

namespace compiler

module analyzer =
  // We need the matchToken in order to match the tokens from what we get to what is expected
  let private matchToken expected (tokens:string list) = 
    List.tail tokens

  // check to see if the elements exist in the list already
  // if an element already exists within the list, return false. Otherwise, return true.
  let rec private contains (x,y) lists = 
    match lists with
    | [] -> false
    | (name, type_check)::tail when name = x -> true
    | head::tail -> contains(x,y) tail

  // check to see if the element has a duplicate (should be an error if there is)
  // if the element is a duplicate, return false. Otherwise true 
  let rec private contains_dupes lists = 
    match lists with
    | [] -> false
    | head::tail when contains head tail = true -> true
    | head::tail -> contains_dupes tail

  // from the symbol table, you only need to see if the variable name exists in there. 
  // if it exists, do nothing. well this returns an empty list but it's not used in context
  // to the rest of the code.
  // if it doesn't exist, throw the failwith "undefined". 
  let rec private check_undefined tokens symboltable = 
    let current = (List.head tokens:string).Substring(11)
    let variable_names = List.map (fun (x,y) -> x) symboltable

    if(List.contains current variable_names) then []
    else failwith("variable '" + current + "' undefined")

  // check to see the expression value of the tokens
  // get the token at the head of the list and determine the
  // expression's value against it's expected value by 
  // calling matchToken on it 
  let rec private expr_value tokens symboltable = 
    let next = List.head tokens
    // this is checking the different types, like false and literals. This is going to call
    // match token to make sure the types match and return T2. otherwise it just returns tokens.
    if next = "false" then 
      let T2 = matchToken "false" tokens
      T2
    elif next = "true" then
      let T2 = matchToken "true" tokens 
      T2
    elif next.StartsWith("identifier") then
      let check = check_undefined tokens symboltable
      let T2 = matchToken "identifier" tokens
      T2
    elif next.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      T2
    elif next.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      T2
    elif next.StartsWith("real_literal") then
      let T2 = matchToken "real_literal" tokens
      T2
    else 
      tokens

  // similar to above, but checking to see the expression operators for the 
  // tokens and match them using MatchToken. Otherwise, it should just return the tokens
  let rec private expr_op tokens = 
    let next = List.head tokens
    if next = "+" || next = "-" || next = "*" || next = "/" || next = "^" || next = "<" || next = "<=" || next = ">" || next = ">=" || next = "==" || next = "!=" then 
      let T2 = matchToken next tokens
      T2
    else
      // just return the tokens otherwise.
      tokens

  // for calling the other expr_XXX functions, they work relatively the same way
  // with the expression operations. It performs the checks from expr_op and value to make sure that they're valid semantics
  let rec private expr tokens symboltable = 
    let T2 = expr_value tokens symboltable
    let next = List.head T2
    if next = "+" || next = "-" || next = "*" || next = "/" || next = "^" || next = "<" || next = "<=" || next = ">" || next = ">=" || next = "==" || next = "!=" then 
      let T3 = expr_op T2
      let T4 = expr_value T3 symboltable
      T4
    else 
      T2

  // This should check to see the ; to notify the end of a line
  // or  the check for the empty statement
  let rec private empty token = 
    let T2 = matchToken ";" token
    T2

  // okay now this is where the variable declarations will be held
  let rec private vardecl tokens symboltable = 
    let type_check = List.head tokens
    
    // if the varaible is an int, check to see if it is a redefinition
    // if it is, there should be an error. otherwise add it to the symbol table
    if type_check = "int" then
      let T2 = matchToken "int" tokens
      let old_name = List.head T2 
      let new_name = old_name.Substring(11)
      let var_add = (new_name, "int")
      if contains var_add symboltable = true then
        failwith("redefinition of variable '" + new_name + "'")
      else 
        let T3 = matchToken "identifier" T2
        let T4 = matchToken ";" T3
        (T4, var_add::symboltable)

    // its the same code but for the "real" keyword check. same code as above.
    else
      let T2 = matchToken "real" tokens
      let old_name = List.head T2 
      let new_name = old_name.Substring(11)
      let var_add = (new_name, "real")
      if contains var_add symboltable = true then
        failwith("redefinition of variable '" + new_name + "'")
      else 
        let T3 = matchToken "identifier" T2
        let T4 = matchToken ";" T3
        (T4, var_add::symboltable)

  // checking the cin inputs for the analyzer to recognize
  // its pretty similar to how it is in the parser and it returns T5
  let rec private input tokens symboltable = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let check = check_undefined T3 symboltable
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5

  // the same type of idea from above but for cout.
  // two functions to help determine its proper structure depending if it has endl or not.
  let rec private output_values tokens symboltable = 
    let next_token = List.head tokens
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let T2 = expr_value tokens symboltable
      T2

  let rec private output tokens symboltable = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_values T3 symboltable
    let T5 = matchToken ";" T4
    T5

  // this one is for matching for assignments with the single =
  let rec private assignment tokens symboltable = 
    // these will check if the variable is defined. if it isn't defined, the
    // failwith inside of the function will terminate the program. otherwise this is just passed
    // and not used beyond that.
    let undefined = check_undefined tokens symboltable
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3 symboltable
    let T5 = matchToken ";" T4 
    T5
  
  // now the most important function to help analyze.
  // this is mostly the same as the one in the parser but
  // with the symboltable instead
  // the next few functions are just copied from my parser and altered
  // to include the symbol table stuff
  // this calls each of the respective functions above to make sure that
  // every declaration, input, output, and identfier are handled properly.
  let rec private stmt tokens symboltable = 
    let next_token = List.head tokens
    // calling the empty to check if the semnatics for empty is correct
    if next_token = ";" then
      let T2 = empty tokens
      (T2, symboltable)
    elif next_token = "int" then  // changes here
      let (T2,table) = vardecl tokens symboltable
      (T2, table)
    elif next_token = "real" then  // changes here
      let (T2,table) = vardecl tokens symboltable
      (T2,table)
    elif next_token = "cin" then
      let T2 = input tokens symboltable
      (T2,symboltable)
    elif next_token = "cout" then
      let T2 = output tokens symboltable
      (T2,symboltable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens symboltable
      (T2,symboltable)
    elif next_token = "if" then
      let (T2,table) = ifstmt tokens symboltable
      (T2,table)
    else
      (tokens,symboltable) 

  // copied from my parser code, a lot of this is just copy and paste
  // this is to match the structure of an if statement to what is had, 
  // and is made to handle whether that if statement has an else attached.
  and private ifstmt tokens symboltable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 symboltable
    let T5 = matchToken ")" T4
    let (T6,table) = then_part T5 symboltable
    let (T7,table2) = else_part T6 table
    (T7,table2)

  // this is to handle the conditional of a statement by calling the expr function
  and private condition tokens symboltable = 
    let T1 = expr tokens symboltable
    T1

  // this is to handle the then part of an if statement by calling the stmt function
  and private then_part tokens symboltable = 
    let (T1,table) = stmt tokens symboltable
    (T1,table)

  // this is to handle the else part of an if statement by calling stmt and match 
  and private else_part tokens symboltable =  
    let next_token = List.head tokens
    // match the tokens using matchToken and stmt
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3,table) = stmt T2 symboltable
      (T3,table)
    else
      // return tokens back otherwise
      (tokens,symboltable)
  
  
  // this function is going to check to see for more statements 
  // and check for the keywords that the program should recognize
  let rec private morestmts tokens symboltable = 
    let next = List.head tokens
    if next = ";" || next = "int" || next = "real" || next = "cin" || next = "cout" || next.StartsWith("identifier") || next = "if" then
        // construct a tuple and call the stmt tokens to help
        let(T2, table) = stmt tokens symboltable
        let (T3, table2) = morestmts T2 table
        (T3, table2)
    else
      // just return the tuple of tokens and symbol table
      (tokens, symboltable)
    
  // start constructing tuples by calling the stmts functions to fill the symbol table
  let rec private stmts tokens symboltable = 
    let (T2, table) = stmt tokens symboltable
    let (T3, table2) = morestmts T2 table
    (T3, table2)

  // this was part of the start code? will still need it somewhere
  // Okay was told that this code from Project 4 is useful, just add the 
  // [] for T6 and change T9 to be a tuple with the symbol table
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7,symboltable) = stmts T6 []
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF
    (T9,symboltable)

  // build_symboltable tokens
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  // DO NOT CHANGE THIS FUNCTION. DO NOT CHANGE THIS FUNCTION
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])

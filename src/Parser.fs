module Parser

open System

type ParserLabel = string

type ParserError = string

type ParserPosition =
    { currentLine : string
      line : int
      column : int }

type Position =
    { line : int
      column : int }

type InputState =
    { lines : string []
      position : Position }

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Parser<'a> =
    { parseFn : InputState -> Result<'a * InputState>
      label : ParserLabel }

let initialPos =
    { line = 0
      column = 0 }

let incrCol pos =
    { line = pos.line
      column = pos.column + 1 }

let incrLine pos =
    { line = pos.line + 1
      column = 0 }

let printResult result =
    match result with
    | Success(value, input) -> printfn "%A" value
    | Failure(label, error, parserPos) -> 
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^ %s" colPos "" error
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then inputState.lines.[linePos]
    else "\0"

let parserPositionFromInputState (inputState : InputState) =
    { currentLine = currentLine inputState
      line = inputState.position.line
      column = inputState.position.column }

let fromStr str =
    if String.IsNullOrEmpty(str) then 
        { lines = [||]
          position = initialPos }
    else 
        let separators = [| "\r\n"; "\n" |]
        let lines = str.Split(separators, StringSplitOptions.None)
        { lines = lines
          position = initialPos }

/// Get the next character from the input, if any
/// else return None. Also return the updated InputState
/// Signature: InputState -> InputState * char option 
let nextChar input =
    let linePos = input.position.line
    let colPos = input.position.column
    // three cases
    // 1) if line >= maxLine -> 
    // return EOF
    // 2) if col less than line length -> 
    // return char at colPos, increment colPos
    // 3) if col at line length -> 
    // return NewLine, increment linePos
    if linePos >= input.lines.Length then input, Some(Convert.ToChar(0))
    
    else 
        let currentLine = currentLine input
        if colPos < currentLine.Length then 
            let char = currentLine.[colPos]
            let newPos = incrCol input.position
            let newState = { input with position = newPos }
            newState, Some char
        else 
            // end of line, so return LF and move to next line
            let char = '\n'
            let newPos = incrLine input.position
            let newState = { input with position = newPos }
            newState, Some char

/// Run the parser on a InputState
let runOnInput parser input =
    // call inner function with input
    parser.parseFn input

/// Run the parser on a string
let run parser inputStr =
    // call inner function with input
    runOnInput parser (fromStr inputStr)

/// Match an input token if the predicate is satisfied
let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = nextChar input
        match charOpt with
        | None -> 
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure(label, err, pos)
        | Some first -> 
            if predicate first then Success(first, remainingInput)
            else 
                let err = sprintf "Unexpected '%c'" first
                let pos = parserPositionFromInputState input
                Failure(label, err, pos)
    { // return the parser
      parseFn = innerFn
      label = label }

let pchar charToMatch =
    let predicate ch = (ch = charToMatch)
    let label = sprintf "%c" charToMatch
    satisfy predicate label

let pwhitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

let returnP value =
    { parseFn = (fun input -> Success(value, input))
      label = "unknown" }

let failP l err =
    let innerFn input =
      let pos = parserPositionFromInputState input
      Failure(l, err, pos)
    { parseFn = innerFn
      label = l }

let bindP f p =
    let label = "unknown"
    
    let innerFn input =
        let result1 = runOnInput p input
        match result1 with
        | Failure(l, err, pos) -> Failure(l, err, pos)
        | Success(value1, remainingInput) -> 
            let p2 = f value1
            runOnInput p2 remainingInput
    { parseFn = innerFn
      label = label }

let (>>=) p f = bindP f p
let andThen p1 p2 = 
      p1 >>= (fun p1Result -> 
      p2 >>= (fun p2Result -> 
      returnP (p1Result, p2Result)))
let (.>>.) = andThen

let orElse parser1 parser2 =
    let p1Label = parser1.label
    let p2Label = parser2.label
    let label = sprintf "%s or %s" p1Label p2Label
    
    let innerFn input =
        // run parser1 with the input
        let result1 = runOnInput parser1 input
        // test the result for Failure/Success
        match result1 with
        | Success _ -> result1
        | Failure(l, err, pos) -> (runOnInput parser2 input)
    { parseFn = innerFn
      label = label }

let (<|>) = orElse
let choice listOfParsers = List.reduce (<|>) listOfParsers
let mapP f = bindP (f >> returnP)
let (<!>) = mapP
let (|>>) x f = mapP f x

/// apply
let applyP fP xP = fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

let (<*>) = applyP
let lift2 f xP yP = (returnP f) <*> xP <*> yP

let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head :: tail
    // lift it to Parser World
    let consP = lift2 cons
    // process the list of parsers recursively
    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

/// Helper to create a string from a list of chars
let charListToStr charList = String(List.toArray charList)

let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = runOnInput parser input
    // test the result for Failure/Success
    match firstResult with
    | Failure(label, err, pos) -> ([], input)
    | Success(firstValue, inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)

/// match zero or more occurences of the specified parser
let many parser =
    let rec innerFn input = Success(parseZeroOrMore parser input)
    { parseFn = innerFn
      label = "l" }

/// match one or more occurences of the specified parser
let many1 p =
    p      >>= (fun head ->
    many p >>= (fun tail ->
        returnP (head::tail) ))
let opt p =
    let some = Some <!> p
    let none = returnP None
    some <|> none

/// Keep only the result of the left side parser
let (.>>) p1 p2 = (p1 .>>. p2) |> mapP (fun (a, b) -> a)

/// Keep only the result of the right side parser
let (>>.) p1 p2 = (p1 .>>. p2) |> mapP (fun (a, b) -> b)

let between p1 p2 p3 = p1 >>. p2 .>> p3

/// Update the label in the parser
let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s -> Success s
        | Failure(_, err, pos) -> Failure(newLabel, err, pos)
    { // return the Parser
      parseFn = newInnerFn
      label = newLabel }

let (<?>) = setLabel

/// parse a specific string
let pstring str =
    // label is just the string
    let label = str
    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr
    <?> label

/// Choose any of a list of characters
let anyOf listOfChars =
    let label = sprintf "one of %A" listOfChars
    listOfChars
    |> List.map pchar // convert into parsers
    |> choice
    <?> label

let pdigit = satisfy (fun ch -> Char.IsDigit(ch)) "digit"

/// parse a whitespace char
let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

/// parse zero or more whitespace char
let spaces = many whitespaceChar

/// parse one or more whitespace char
let spaces1 = many1 whitespaceChar
/// Keep only the result of the middle parser
/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []

let EOF = (spaces .>>. (Convert.ToChar(0) |> pchar)) <?> "EOF"
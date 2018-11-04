module ParserInput

open System
open System.IO

type ParserPosition =
    {currentLine: string
     line: int
     column: int}

type Position =
    {line: int
     column: int}

type InputState =
    {lines: string []
     position: Position}

let initialPos =
    {line = 0
     column = 0}

let incrCol pos =
    {line = pos.line
     column = pos.column + 1}

let incrLine pos =
    {line = pos.line + 1
     column = 0}

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then inputState.lines.[linePos]
    else "\0"

let parserPositionFromInputState(inputState: InputState) =
    {currentLine = currentLine inputState
     line = inputState.position.line
     column = inputState.position.column}

let inputStateFromStr str =
    if String.IsNullOrEmpty(str) then
        {lines = [||]
         position = initialPos}
    else
        let separators = [|"\r\n";"\n"|]
        let lines = str.Split(separators,StringSplitOptions.None)
        {lines = lines
         position = initialPos}

let inputStateFromFile file =
    if File.Exists(file) then
        let lines = File.ReadAllLines(file)
        {lines = lines
         position = initialPos}
    else
        {lines = [||]
         position = initialPos}

let private nullChar = char 0

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
    if linePos >= input.lines.Length then input,Convert.ToChar(0)
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrCol input.position
            let newState = {input with position = newPos}
            newState,char
        else
            // end of line, so return LF and move to next line
            let char = '\n'
            let newPos = incrLine input.position
            let newState = {input with position = newPos}
            newState,char

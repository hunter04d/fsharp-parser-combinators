module EmailParser

open Parser
open System

let private atParser = pchar '@'
let private validLocalChars = "!#$%&'*+-/=?^_`{|}~"
let private localSymbolParser = satisfy (fun ch -> validLocalChars.IndexOf(ch) <> -1) "symbol"
let private dotParser = pchar '.'
let private quoteParser = pchar '"'
let private isLatinLetter ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
let private latinLetterParser = satisfy isLatinLetter "Letter"

let localPartParser : Parser<String> =
    
    let validForAll = latinLetterParser <|> localSymbolParser <|> pdigit
    let unquotedLocal = (sepBy1IncludeSep validForAll (validForAll <|> dotParser)) <|> many1 validForAll
    let quotedLocal = 
        quoteParser 
        .>>. many1 (validForAll <|> dotParser) 
        .>>. quoteParser 
        |>> fun ((q1, m), q2) -> (q1 :: m) @ [q2]
    let localPartParser = unquotedLocal <|> quotedLocal
    localPartParser |>> charListToStr


let domainPartParser =
    let label = "domain part" 
    let binder (str:String) =     
        if str.StartsWith('-') && str.EndsWith('-') then
            failP label "cant start and end with -"
        elif str.StartsWith('-') then
            failP label "can't start with -"
        elif str.EndsWith('-') then
            failP label "can't end with -"
        else 
            returnP str
    let charParser = latinLetterParser <|> pchar '-' <|> pdigit
    sepBy1 ((many1 charParser) |>> charListToStr >>= binder) dotParser

let email =
    localPartParser .>> atParser .>>. domainPartParser <?> "email"


let emailList = (sepBy email spaces) <?> "email list"
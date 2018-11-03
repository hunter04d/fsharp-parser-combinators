module EmailParser

open Parser
open System

let atParser = pchar '@'
let private validLocalChars = "!#$%&'*+-/=?^_`{|}~"
let localSymbolParser = satisfy (fun ch -> validLocalChars.IndexOf(ch) <> -1) "symbol"
let dotParser = pchar '.'
let quoteParser = pchar '"'
let isLatinLetter ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
let latinLetterParser = satisfy isLatinLetter "Letter"

let localPartParser : Parser<String> =
    let binder v =
        match v with
        | ((Some q1, str), Some q2) -> 
            let res = (q1 :: str) @ [ q2 ]
            returnP (charListToStr res)
        | ((None, str), None) -> returnP (charListToStr str)
        | _ -> failP "matched quotes" "one quote missing"
    
    let validForAll = latinLetterParser <|> localSymbolParser <|> pdigit
    opt quoteParser .>>. ((many validForAll)) .>>. opt quoteParser >>= binder


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


let emailList = (sepBy email whitespaceChar) .>> EOF
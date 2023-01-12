let () = print_endline "Hello, World!"

type 'a result = Ok of 'a | Err of string

type 'a parser = Parser of (string -> ('a * string) result)

exception Failure of string

let pchar charToMatch =
  let innerFn str =
    match str with
    | "" -> Err "No more input"
    | _ ->
        let first = str.[0] in
        if first = charToMatch then
          Ok (charToMatch, String.sub str 1 (String.length str - 1))
        else
          Err (Printf.sprintf "Expecting '%c'. Got '%c'" charToMatch first)
  in
  Parser innerFn

let run parser input =
  let (Parser innerFn) = parser in
  innerFn input

let andThen parser1 parser2 =
  let innerFn input =
    let result1 = run parser1 input in
    match result1 with
    | Err err -> Err err
    | Ok (value1, remaining1) -> (
        let result2 = run parser2 remaining1 in
        match result2 with
        | Err err -> Err err
        | Ok (value2, remaining2) ->
            let newValue = (value1, value2) in
            Ok (newValue, remaining2) )
  in
  Parser innerFn

let ( >> ) = andThen

let orElse parser1 parser2 =
  let innerFn input =
    let result1 = run parser1 input in
    match result1 with Ok _ -> result1 | Err _ -> run parser2 input
  in
  Parser innerFn

let ( <|> ) = orElse

let rec choice listOfParsers : 'a parser =
  match listOfParsers with
  | [] -> Parser (fun _ -> Err "empty")
  | x :: xs -> x <|> choice xs

let anyOf listOfChars = listOfChars |> List.map pchar |> choice

type 'a result = Ok of 'a | Err of string

type 'a parser = Parser of (string -> ('a * string) result)

let composition f g x = g (f x)

let ( >>| ) = composition

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

let bindP f p =
  let innerFn input =
    let result1 = run p input in
    match result1 with
    | Err _ as e -> e
    | Ok (value1, remainingInput) -> run (f value1) remainingInput
  in
  Parser innerFn

let ( >>= ) p f = bindP f p

let returnP x =
  let innerFn input = Ok (x, input) in
  Parser innerFn

let mapP f = bindP (f >>| returnP)

let ( <!> ) = mapP

let ( |>> ) x f = mapP f x

let applyP fP xP = fP >>= fun f -> xP >>= fun x -> returnP (f x)

let ( <*> ) = applyP

let lift2 f xP yP = returnP f <*> xP <*> yP

let andThen p1 p2 =
  p1 >>= fun p1Result -> p2 >>= fun p2Result -> returnP (p1Result, p2Result)

let ( >> ) = andThen

let orElse p1 p2 =
  let innerFn input =
    let result1 = run p1 input in
    match result1 with Ok _ -> result1 | Err _ -> run p2 input
  in
  Parser innerFn

let ( <|> ) = orElse

let rec choice listOfParsers =
  match listOfParsers with
  | [] -> Parser (fun _ -> Err "empty")
  | x :: xs -> x <|> choice xs

let anyOf listOfChars = listOfChars |> List.map pchar |> choice

let rec sequence parserList =
  let cons head tail = head :: tail in
  let consP = lift2 cons in
  match parserList with
  | [] -> returnP []
  | head :: tail -> consP head (sequence tail)

let rec parseZeroOrMore parser input =
  let firstResult = run parser input in
  match firstResult with
  | Err _ -> ([], input)
  | Ok (firstValue, inputAfterFirstParse) ->
      let subsequentValues, remainingInput =
        parseZeroOrMore parser inputAfterFirstParse
      in
      let values = firstValue :: subsequentValues in
      (values, remainingInput)

let many parser =
  let innerFn input = Ok (parseZeroOrMore parser input) in
  Parser innerFn

let many1 p = p >>= fun head -> many p >>= fun tail -> returnP (head :: tail)

let opt p =
  let some = p |>> Option.some in
  let none = returnP None in
  some <|> none

let ( <$ ) p1 p2 = p1 >> p2 |> mapP (fun (a, _) -> a)

let ( $> ) p1 p2 = p1 >> p2 |> mapP (fun (_, b) -> b)

let between p1 p2 p3 = p1 $> p2 <$ p3

let sepBy1 p sep =
  let sepThenP = sep $> p in
  p >> many sepThenP |>> fun (p, pList) -> p :: pList

let sepBy p sep = sepBy1 p sep <|> returnP []

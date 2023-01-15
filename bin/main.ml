module Input = struct
  type position = {line: int; column: int}

  let init_pos = {line= 0; column= 0}

  let incr_col pos = {pos with column= pos.column + 1}

  let incr_line pos = {line= pos.line + 1; column= 0}

  type state = {lines: string list; position: position}

  let cur_line state =
    let line_pos = state.position.line in
    if line_pos < List.length state.lines then List.nth state.lines line_pos
    else "end of file"

  let from_string str =
    if str = "" then {lines= []; position= init_pos}
    else
      let lines = String.split_on_char '\n' str in
      {lines; position= init_pos}

  let next_char input =
    let line_pos = input.position.line in
    let col_pos = input.position.column in
    if line_pos >= List.length input.lines then (input, None)
    else
      let current_line = cur_line input in
      if col_pos < String.length current_line then
        let c = String.get current_line col_pos in
        let new_pos = incr_col input.position in
        let new_state = {input with position= new_pos} in
        (new_state, Some c)
      else
        let c = '\n' in
        let new_pos = incr_line input.position in
        let new_state = {input with position= new_pos} in
        (new_state, Some c)
end

let compose f g x = g (f x)

type state = Input.state

type parser_position = {current_line: string; line: int; column: int}

type 'a result = Ok of 'a | Err of string * string * parser_position

type 'a parser = {fn: state -> ('a * state) result; label: string}

let run_on_input parser input = parser.fn input

let run parser str = run_on_input parser (Input.from_string str)

let parser_pos_from_state state =
  { current_line= Input.cur_line state
  ; line= state.position.line
  ; column= state.position.column }

let print_result = function
  | Ok (value, _) -> value
  | Err (label, error, parser_pos) ->
      let error_line = parser_pos.current_line in
      let col_pos = parser_pos.column in
      let line_pos = parser_pos.line in
      let failure_caret = Printf.sprintf "%*s^%s" col_pos "" error in
      failwith
      @@ Printf.sprintf "Line:%i Col:%i Error parsing %s\n%s\n%s" line_pos
           col_pos label error_line failure_caret

let get_label parser = parser.label

let set_label parser new_label =
  let inner_fn input =
    let res = parser.fn input in
    match res with
    | Ok (_, _) as o -> o
    | Err (_, error, pos) -> Err (new_label, error, pos)
  in
  {fn= inner_fn; label= new_label}

let ( <?> ) = set_label

let satisfy pred label =
  let inner_fn input =
    let remaining, c = Input.next_char input in
    match c with
    | None ->
        let error = "No more input" in
        let pos = parser_pos_from_state input in
        Err (label, error, pos)
    | Some first ->
        if pred first then Ok (first, remaining)
        else
          let error = Printf.sprintf "Unexpected '%c'" first in
          let pos = parser_pos_from_state input in
          Err (label, error, pos)
  in
  {fn= inner_fn; label}

let bind f p =
  let label = "unknown" in
  let innerFn input =
    match run_on_input p input with
    | Err (_, _, _) as e -> e
    | Ok (v, remaining) ->
        let p2 = f v in
        run_on_input p2 remaining
  in
  {fn= innerFn; label}

let ( >>= ) p f = bind f p

let return x =
  let inner_fn input = Ok (x, input) in
  {fn= inner_fn; label= "hwo to do this?"}

let map f = bind (compose f return)

let ( <!> ) = map

let ( |>> ) x f = map f x

let apply f x = f >>= fun f -> x >>= fun x -> return (f x)

let ( <*> ) = apply

let lift2 f x y = return f <*> x <*> y

let and_then p1 p2 =
  let label = Printf.sprintf "%s andThen %s" (get_label p1) (get_label p2) in
  p1 >>= (fun r1 -> p2 >>= fun r2 -> return (r1, r2)) <?> label

let ( >> ) = and_then

let or_else p1 p2 =
  let label = Printf.sprintf "%s orElse %s" (get_label p1) (get_label p2) in
  let inner_fn input =
    let r1 = run_on_input p1 input in
    match r1 with
    | Ok _ -> r1
    | Err _ ->
        let r2 = run_on_input p2 input in
        r2
  in
  {fn= inner_fn; label}

let ( <|> ) = or_else

let rec choice parsers =
  match parsers with
  | [] ->
      { fn=
          (fun _ ->
            Err ("empty", "empty", {current_line= ""; line= 0; column= 0}) )
      ; label= "no more input" }
  | x :: xs -> x <|> choice xs

let rec sequence parsers =
  let cons head tail = head :: tail in
  let cons_parser = lift2 cons in
  match parsers with
  | [] -> return []
  | head :: tail -> cons_parser head (sequence tail)

let rec parse_zero_or_more parser input =
  let first_result = run_on_input parser input in
  match first_result with
  | Err (_, _, _) -> ([], input)
  | Ok (first_value, input_after_first_parse) ->
      let sub_value, remaining =
        parse_zero_or_more parser input_after_first_parse
      in
      let values = first_value :: sub_value in
      (values, remaining)

let many parser =
  let label = Printf.sprintf "many %s" (get_label parser) in
  let inner_fn input = Ok (parse_zero_or_more parser input) in
  {fn= inner_fn; label}

let many1 p =
  let label = Printf.sprintf "many1 %s" (get_label p) in
  p >>= (fun head -> many p >>= fun tail -> return (head :: tail)) <?> label

let opt p =
  let label = Printf.sprintf "opt %s" (get_label p) in
  let some = p |>> Option.some in
  let none = return None in
  some <|> none <?> label

let ( <$ ) p1 p2 = p1 >> p2 |> map (fun (a, _) -> a)

let ( $> ) p1 p2 = p1 >> p2 |> map (fun (_, b) -> b)

let between p1 p2 p3 = p1 $> p2 <$ p3

let sep_by1 p sep =
  let sep_then = sep $> p in
  p >> many sep_then |>> fun (p, rest) -> p :: rest

let sep_by p sep = sep_by1 p sep <|> return []

let pchar c =
  let label = Printf.sprintf "%c" c in
  let predicate ch = ch = c in
  satisfy predicate label

let any_of chars =
  (* %A *)
  let label = "any_of" in
  chars |> List.map pchar |> choice <?> label

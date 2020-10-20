{
open Result
open Astring
open Migrate_ast

type token = [ `Block of Block.t | `Section of int * string | `Text of string ]

let line_ref = ref 1

let newline lexbuf =
  Lexing.new_line lexbuf;
  incr line_ref

let labels l =
  match Label.of_string l with
  | Ok labels -> labels
  | Error msgs ->
    let msgs = List.map (fun (`Msg (x : string)) -> x) msgs in
    let msg = String.concat ~sep:" " msgs in
    failwith msg
}

let eol = '\n' | '\r' '\n' | eof
let ws = [' ' '\t']

let until_eol = [^'\n' '\r']
let until_ws = [^' ' '\t']
let until_ws_or_eol = [^' ' '\t' '\n' '\r']

rule text section = parse
  | eof { [] }
  | ("#"+ as n) " " (until_eol* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: text (Some section) lexbuf }
  | ( "<!--" ws* "$MDX" ws* (until_ws* as label_cmt) ws* "-->" ws* eol? )?
      "```" (until_ws_or_eol* as h) ws* (until_eol* as legacy_labels) eol
      { let header = Block.Header.of_string h in
        let contents = block lexbuf in
        let labels, legacy_labels =
          match (label_cmt, legacy_labels) with
          | Some label_cmt, "" -> labels label_cmt, false
          | Some _, _ -> failwith "cannot mix both block labels syntax"
          | None, l -> labels l, true
        in
        let errors =
          match error_block lexbuf with
          | exception _ -> []
          | e ->
            List.map (fun x ->
                match String.trim x with
                | "..." -> `Ellipsis
                | _ -> `Output x) e
        in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        let column = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
        newline lexbuf;
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        let block =
          match
            Block.mk ~file ~line ~column ~section ~header ~contents ~labels
              ~legacy_labels ~errors
          with
          | Ok block -> block
          | Error (`Msg msg) -> failwith msg
        in
        (match errors with
         | [] -> ()
         | _ ->
           newline lexbuf;
           List.iter (fun _ -> newline lexbuf) errors;
           newline lexbuf);
        `Block block :: text section lexbuf }
  | (until_eol* as str) eol
      { newline lexbuf;
        `Text str :: text section lexbuf }

and block = parse
  | eof | "```" ws* eol    { [] }
  | (until_eol* as str) eol { str :: block lexbuf }

and error_block = parse
  | "```mdx-error" ws* eol { block lexbuf }

and cram_text section = parse
  | eof { [] }
  | ("#"+ as n) " " (until_eol* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: cram_text (Some section) lexbuf }
  | "  " (until_eol* as first_line) eol
      { let header = Some (Block.Header.Shell `Sh) in
        let requires_empty_line, contents = cram_block lexbuf in
        let contents = first_line :: contents in
        let labels = [] in
        let legacy_labels = false in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        let column = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        let block =
          match
            Block.mk ~file ~line ~column ~section ~header ~contents ~labels
              ~legacy_labels ~errors:[]
          with
          | Ok block -> block
          | Error (`Msg msg) -> failwith msg
        in
        `Block block
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | "<-- non-deterministic" ws* (until_eol* as choice) eol
      { let header = Some (Block.Header.Shell `Sh) in
        let requires_empty_line, contents = cram_block lexbuf in
        let labels =
          match Label.interpret "non-deterministic" (Some (Eq, choice)) with
          | Ok label -> [label]
          | Error (`Msg msg) -> failwith msg
        in
        let legacy_labels = false in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        let column = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
        newline lexbuf;
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        let block =
          match
            Block.mk ~file ~line ~column ~section ~header ~contents ~labels
              ~legacy_labels ~errors:[]
          with
          | Ok block -> block
          | Error (`Msg msg) -> failwith msg
        in
        `Block block
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | (until_eol* as str) eol
      { newline lexbuf;
        `Text str :: cram_text section lexbuf }

and cram_block = parse
  | eof { false, [] }
  | eol { newline lexbuf; true, [] }
  | "  " (until_eol* as str) eol
      { let requires_empty_line, lst = cram_block lexbuf in
        requires_empty_line, str :: lst }

{
  let markdown_token lexbuf =
    try Ok (text None lexbuf)
    with
    | Failure e ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: invalid code block: %s" Location.print_loc loc e
      in
      Util.Result.errorf "%s" msg
    | exn ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: %s" Location.print_loc loc (Printexc.to_string exn)
      in
      Util.Result.errorf "%s" msg


let cram_token lexbuf =
    try Ok (cram_text None lexbuf)
    with
    | Failure e ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: invalid code block: %s" Location.print_loc loc e
      in
      Util.Result.errorf "%s" msg
    | exn ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: %s" Location.print_loc loc (Printexc.to_string exn)
      in
      Util.Result.errorf "%s" msg
}

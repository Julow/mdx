open! Compat
module Loc = Odoc_parser.Loc

module Code_block = struct
  type t = { location : Loc.span; metadata : string option; contents : string }
end

let drop_last lst =
  match List.rev lst with
  | [] -> None
  | last :: rev_tl -> Some (List.rev rev_tl, last)

(* drop_first_and_last [1; 2; 3; 4] = Some (1, Some ([2; 3], 4)). *)
let drop_first_and_last = function
  | [] -> None
  | first :: tl -> Some (first, drop_last tl)

let slice lines ~(start : Loc.point) ~(end_ : Loc.point) =
  let lines_to_include =
    Util.Array.slice lines ~from:(start.line - 1) ~to_:(end_.line - 1)
    |> Array.to_list
  in
  match drop_first_and_last lines_to_include with
  | None -> ""
  | Some (line, None) ->
      String.sub line start.column (end_.column - start.column)
  (* Imagine we were slicing the file from (Line 2, Column 3) to (Line 6, Column 7):

       0123456789
       1 ----------
       2 ---[---
       3 ---------
       4 --
       5 ----------
       6 -------]--
       7 ----------
       8 ----------

       The case below handles this multiline case, concatenating the included substrings
       from lines 2-6 ([lines_to_include]). *)
  | Some (first_line, Some (stripped, last_line)) ->
      let first_line =
        String.sub first_line start.column
          (String.length first_line - start.column)
      in
      let last_line = String.sub last_line 0 end_.column in
      String.concat "\n" ([ first_line ] @ stripped @ [ last_line ])

(* Imagine a docstring that is within a file with four characters # of indentation. (I'll
   use square brackets rather than parens to avoid escaping):

   ####[** foo
   ####
   ####bar
   ####
   ####baz *]
   ####val x : int
   ####val y : int

   According to odoc, the "b" in "bar" is at column 0 inside the docstring and at column 4
   within the broader file. That is correct. But it says the "f" in "foo" is at column 1
   inside the docstring and column 5 within the file. This isn't right.

   The problem is that it starts counting the inside-the-docstring column number from the
   end of "[**", but doesn't add those three characters to the within-the-file column
   number. Here, we make the adjustment.
*)
let account_for_docstring_open_token (location : Odoc_parser.Loc.span) =
  let start_shift = 3 in
  let end_shift = if location.start.line = location.end_.line then 3 else 0 in
  {
    location with
    start = { location.start with column = location.start.column + start_shift };
    end_ = { location.end_ with column = location.end_.column + end_shift };
  }

let extract_code_blocks ~(location : Lexing.position) ~docstring =
  let rec acc blocks =
    List.map
      (fun block ->
        match Loc.value block with
        | `Code_block (metadata, { Odoc_parser.Loc.value = contents; _ }) ->
            let location =
              if location.pos_lnum = block.location.start.line then
                account_for_docstring_open_token block.location
              else block.location
            in
            let metadata =
              match metadata with
              | None -> None
              | Some { Odoc_parser.Loc.value; _ } -> Some value
            in
            [ { Code_block.location; metadata; contents } ]
        | `List (_, _, lists) -> List.map acc lists |> List.concat
        | _ -> [])
      blocks
    |> List.concat
  in
  let parsed = Odoc_parser.parse_comment ~location ~text:docstring in
  List.iter
    (fun error -> failwith (Odoc_parser.Warning.to_string error))
    (Odoc_parser.warnings parsed);
  List.map
    (fun element ->
      match element with
      | { Loc.value = #Odoc_parser.Ast.nestable_block_element; _ } as e ->
          acc [ e ]
      | { value = `Tag tag; _ } -> (
          match tag with
          | `Deprecated blocks -> acc blocks
          | `Param (_, blocks) -> acc blocks
          | `Raise (_, blocks) -> acc blocks
          | `Return blocks -> acc blocks
          | `See (_, _, blocks) -> acc blocks
          | `Before (_, blocks) -> acc blocks
          | _ -> [])
      | { value = `Heading _; _ } -> [])
    (Odoc_parser.ast parsed)
  |> List.concat

let docstrings lexbuf =
  let rec loop list =
    match Lexer.token_with_comments lexbuf with
    | Parser.EOF -> list
    | Parser.DOCSTRING docstring ->
        let docstring =
          ( Docstrings.docstring_body docstring,
            Docstrings.docstring_loc docstring )
        in
        loop (docstring :: list)
    | _ -> loop list
  in
  loop [] |> List.rev

let convert_pos (p : Lexing.position) (pt : Loc.point) =
  { p with pos_lnum = pt.line; pos_cnum = pt.column }

let convert_loc (loc : Location.t) (sp : Loc.span) =
  let loc_start = convert_pos loc.loc_start sp.start in
  let loc_end = convert_pos loc.loc_end sp.end_ in
  { loc with loc_start; loc_end }

let docstring_code_blocks str =
  Lexer.handle_docstrings := true;
  Lexer.init ();
  List.map
    (fun (docstring, (location : Location.t)) ->
      let blocks =
        extract_code_blocks ~location:location.loc_start ~docstring
      in
      List.map
        (fun (b : Code_block.t) -> (b, convert_loc location b.location))
        blocks)
    (docstrings (Lexing.from_string str))
  |> List.concat

let parse_metadata s =
  match String.index_opt s ' ' with
  | None -> Ok (Block.Header.of_string s, [])
  | Some i -> (
      let header = String.sub s 0 i
      and labels = String.sub s (i + 1) (String.length s - i - 1) in
      let header = Block.Header.of_string header in
      match Label.of_string labels with
      | Ok labels -> Ok (header, labels)
      | Error _ as e -> e)

let parse_mli file_contents =
  (* Find the locations of the code blocks within [file_contents], then slice it up into
     [Text] and [Block] parts by using the starts and ends of those blocks as
     boundaries. *)
  let code_blocks = docstring_code_blocks file_contents in
  let cursor = ref { Loc.line = 1; column = 0 } in
  let lines = String.split_on_char '\n' file_contents |> Array.of_list in
  let tokens =
    List.map
      (fun ((code_block : Code_block.t), loc) ->
        let pre_text =
          Document.Text
            (slice lines ~start:!cursor ~end_:code_block.location.start)
        in
        let column = code_block.location.start.column in
        let contents = Compat.String.split_on_char '\n' code_block.contents in
        let header, labels, opening =
          match code_block.metadata with
          | Some s -> (
              match parse_metadata s with
              | Ok (hd, lbl) -> (hd, lbl, "{@" ^ s ^ "[")
              | Error msgs ->
                  let (`Msg msg) = List.hd msgs in
                  failwith msg)
          | None ->
              (* Blocks with no header are ocaml by default. *)
              (Some OCaml, [], "{[")
        in
        let block =
          match
            Block.mk ~loc ~section:None ~labels ~header ~contents
              ~legacy_labels:false ~errors:[]
          with
          | Ok block -> Document.Block block
          | Error _ -> failwith "Error creating block"
        in
        let hpad =
          if List.length contents = 1 then ""
          else Astring.String.v ~len:column (fun _ -> ' ')
        in
        cursor := code_block.location.end_;
        [ pre_text; Text opening; block; Text (hpad ^ "]}") ])
      code_blocks
    |> List.concat
  in
  let eof =
    {
      Loc.line = Array.length lines;
      column = String.length lines.(Array.length lines - 1);
    }
  in
  let eof_is_beyond_location (loc : Loc.point) =
    eof.line > loc.line || (eof.line = loc.line && eof.column > loc.column)
  in
  if eof_is_beyond_location !cursor then
    let remainder = slice lines ~start:!cursor ~end_:eof in
    if not (Compat.String.equal remainder "") then tokens @ [ Text remainder ]
    else tokens
  else tokens

let parse_mli file_contents =
  try Result.Ok (parse_mli file_contents)
  with exn -> Util.Result.errorf "%s" (Printexc.to_string exn)

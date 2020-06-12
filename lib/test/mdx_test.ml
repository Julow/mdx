open Mdx
open Compat
open Result
open Astring

let src = Logs.Src.create "cram.test"

module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Filename.concat

(* From jbuilder's stdlib *)
let ansi_color_strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  and skip i =
    if i = len then Buffer.contents buf
    else match str.[i] with 'm' -> loop (i + 1) | _ -> skip (i + 1)
  in
  loop 0

let output_from_line s =
  `Output (String.drop ~rev:true ~sat:Char.Ascii.is_blank s)

let with_dir root f =
  match root with
  | None -> f ()
  | Some d -> (
      let old_d = Sys.getcwd () in
      try
        Sys.chdir d;
        let r = f () in
        Sys.chdir old_d;
        r
      with e ->
        Sys.chdir old_d;
        raise e )

let get_env blacklist =
  let env = Array.to_list (Unix.environment ()) in
  let env = List.map (String.cuts ~sep:"=") env in
  let f env var =
    let g l = String.compare (List.nth l 0) var <> 0 in
    List.filter g env
  in
  let env = List.fold_left f env blacklist in
  Array.of_list (List.map (String.concat ~sep:"=") env)

let run_test ?root blacklist temp_file t =
  let cmd = Cram.command_line t in
  let env = get_env blacklist in
  Log.info (fun l -> l "exec: %S" cmd);
  let fd = Unix.openfile temp_file [ O_WRONLY; O_TRUNC ] 0 in
  let pid =
    with_dir root (fun () ->
        Unix.create_process_env "sh" [| "sh"; "-c"; cmd |] env Unix.stdin fd fd)
  in
  Unix.close fd;
  match snd (Unix.waitpid [] pid) with WEXITED n -> n | _ -> 255

let root_dir ?root ?block () =
  match block with
  | Some t -> (
      match Mdx.Block.directory t with
      | Some d -> (
          match root with
          | Some r -> Some (r / d)
          | None -> Some (Filename.dirname t.file / d) )
      | None -> root )
  | None -> root

let resolve_root file dir root =
  match root with None -> dir / file | Some r -> r / dir / file

let run_cram_tests ?syntax t ?root ppf temp_file pad tests =
  Block.pp_header ?syntax ppf t;
  let pad = match syntax with Some Cram -> pad + 2 | _ -> pad in
  List.iter
    (fun test ->
      let root = root_dir ?root ~block:t () in
      let blacklist = Block.unset_variables t in
      let n = run_test ?root blacklist temp_file test in
      let lines = Mdx.Util.File.read_lines temp_file in
      let output =
        let output = List.map output_from_line lines in
        if Output.equal output test.output then test.output
        else Output.merge output test.output
      in
      Cram.pp_command ~pad ppf test;
      List.iter
        (function
          | `Ellipsis -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
              let line = ansi_color_strip line in
              Output.pp ~pad ppf (`Output line))
        output;
      Cram.pp_exit_code ~pad ppf n)
    tests;
  Block.pp_footer ?syntax ppf t

let eval_test ?block ?root c test =
  Log.debug (fun l ->
      l "eval_test %a" Fmt.(Dump.list (Fmt.fmt "%S")) (Toplevel.command test));
  let root = root_dir ?root ?block () in
  with_dir root (fun () -> Mdx_top.eval c (Toplevel.command test))

let err_eval ~cmd lines =
  Fmt.epr "Got an error while evaluating:\n---\n%a\n---\n%a\n%!"
    Fmt.(list ~sep:(unit "\n") string)
    cmd
    Fmt.(list ~sep:(unit "\n") string)
    lines;
  exit 1

let eval_raw c ?block ?root ~line lines =
  let test =
    Toplevel.{ vpad = 0; hpad = 0; line; command = lines; output = [] }
  in
  match eval_test ?block ?root c test with
  | Ok _ -> ()
  | Error e -> err_eval ~cmd:lines e

let split_lines lines =
  let aux acc s =
    (* XXX(samoht) support windowns *)
    let lines = String.cuts ~sep:"\n" s in
    List.append lines acc
  in
  List.fold_left aux [] (List.rev lines)

let eval_ocaml ~block ?syntax ?root c ppf ~line lines errors =
  let test =
    Toplevel.{ vpad = 0; hpad = 0; line; command = lines; output = [] }
  in
  let update ~errors = function
    | { Block.value = OCaml v; _ } as b ->
        { b with value = OCaml { v with errors } }
    (* [eval_ocaml] only called on OCaml blocks *)
    | _ -> assert false
  in
  match eval_test ?root ~block c test with
  | Ok _ -> Block.pp ?syntax ppf (update ~errors:[] block)
  | Error lines ->
      let errors =
        let lines = split_lines lines in
        let output = List.map output_from_line lines in
        if Output.equal output errors then errors
        else
          List.map
            (function
              | `Ellipsis -> `Ellipsis
              | `Output x -> `Output (ansi_color_strip x))
            (Output.merge output errors)
      in
      Block.pp ?syntax ppf (update ~errors block)

let lines = function Ok x | Error x -> x

let run_toplevel_tests ?syntax ?root c ppf tests t =
  Block.pp_header ?syntax ppf t;
  List.iter
    (fun test ->
      let lines = lines (eval_test ?root ~block:t c test) in
      let lines = split_lines lines in
      let output =
        let output = List.map output_from_line lines in
        if Output.equal output test.output then test.output else output
      in
      let pad = test.hpad in
      Toplevel.pp_command ppf test;
      List.iter
        (function
          | `Ellipsis -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
              let line = ansi_color_strip line in
              Output.pp ~pad ppf (`Output line))
        output)
    tests;
  match syntax with Some Syntax.Mli -> () | _ -> Block.pp_footer ?syntax ppf t

type file = { first : Mdx.Part.file; current : Mdx.Part.file }

let files : (string, file) Hashtbl.t = Hashtbl.create 8

let has_changed ~force_output { first; current } =
  let contents = Mdx.Part.contents current in
  if contents = Mdx.Part.contents first && force_output = false then None
  else Some contents

let read_parts file =
  try Hashtbl.find files file
  with Not_found -> (
    match Mdx.Part.read file with
    | exception Sys_error msg -> failwith msg
    | parts ->
        let f = { first = parts; current = parts } in
        Hashtbl.add files file f;
        f )

let read_part file part =
  let parts = read_parts file in
  match Mdx.Part.find parts.current ~part with
  | None ->
      Fmt.failwith "Cannot find part %S in %s"
        (match part with None -> "" | Some p -> p)
        file
  | Some lines ->
      let contents = String.concat ~sep:"\n" lines in
      String.drop contents ~rev:true ~sat:Char.Ascii.is_white
      |> String.drop ~sat:(function '\n' -> true | _ -> false)

let write_parts ~force_output file parts =
  let output_file = file ^ ".corrected" in
  match has_changed ~force_output parts with
  | None -> if Sys.file_exists output_file then Sys.remove output_file
  | Some c ->
      let oc = open_out output_file in
      output_string oc c;
      flush oc;
      close_out oc

let update_block_content ?syntax ppf t content =
  Block.pp_header ?syntax ppf t;
  Output.pp ppf (`Output content);
  Block.pp_footer ?syntax ppf t

let update_file_or_block ?syntax ?root ppf md_file ml_file block part =
  let root = root_dir ?root ~block () in
  let dir = Filename.dirname md_file in
  let ml_file = resolve_root ml_file dir root in
  update_block_content ?syntax ppf block (read_part ml_file part)

type output = [ `File of string | `Stdout ]

exception Test_block_failure of Block.t * string

let with_non_det ~command ~output ~det non_deterministic = function
  (* the command is non-deterministic so skip everything *)
  | Some Label.Nd_command when not non_deterministic -> command ()
  (* its output is non-deterministic; run it but keep the old output. *)
  | Some Label.Nd_output when not non_deterministic -> output ()
  | _ -> det ()

let run c ?(non_deterministic = false) ?syntax ?section ?root
    ?(force_output = false) ?output file =
  let syntax =
    match syntax with Some syntax -> Some syntax | None -> Syntax.infer ~file
  in
  let test_block ~ppf ~temp_file t =
    let print_block () = Block.pp ?syntax ppf t in
    if Block.is_active ?section t then
      match Block.value t with
      | Raw _ -> print_block ()
      | Include { file_included; file_kind = Fk_ocaml { part_included } } ->
          assert (syntax <> Some Cram);
          update_file_or_block ?syntax ?root ppf file file_included t
            part_included
      | Include { file_included; file_kind = Fk_other _ } ->
          let new_content = read_part file_included None in
          update_block_content ?syntax ppf t new_content
      | OCaml { non_det; env; errors } ->
          let det () =
            assert (syntax <> Some Cram);
            Mdx_top.in_env env (fun () ->
                eval_ocaml ~block:t ?syntax ?root c ppf ~line:t.line t.contents
                  errors)
          in
          with_non_det non_deterministic non_det ~command:print_block
            ~output:det ~det
      | Cram { language = _; non_det } ->
          let pad, tests = Cram.of_lines t.contents in
          with_non_det non_deterministic non_det ~command:print_block
            ~output:(fun () ->
              print_block ();
              let blacklist = Block.unset_variables t in
              List.iter
                (fun t -> ignore (run_test ?root blacklist temp_file t))
                tests)
            ~det:(fun () ->
              run_cram_tests ?syntax t ?root ppf temp_file pad tests)
      | Toplevel { non_det; env } ->
          let tests =
            let syntax = Util.Option.value syntax ~default:Normal in
            Toplevel.of_lines ~syntax ~file:t.file ~line:t.line ~column:t.column
              t.contents
          in
          with_non_det non_deterministic non_det ~command:print_block
            ~output:(fun () ->
              assert (syntax <> Some Cram);
              print_block ();
              List.iter
                (fun test ->
                  match
                    Mdx_top.in_env env (fun () ->
                        eval_test ~block:t ?root c test)
                  with
                  | Ok _ -> ()
                  | Error e ->
                      let output = List.map (fun l -> `Output l) e in
                      if Output.equal test.output output then ()
                      else err_eval ~cmd:test.command e)
                tests)
            ~det:(fun () ->
              assert (syntax <> Some Cram);
              Mdx_top.in_env env (fun () ->
                  run_toplevel_tests ?syntax ?root c ppf tests t))
    else print_block ()
  in
  let gen_corrected file_contents items =
    let temp_file = Filename.temp_file "ocaml-mdx" ".output" in
    at_exit (fun () -> Sys.remove temp_file);
    let buf = Buffer.create (String.length file_contents + 1024) in
    let ppf = Format.formatter_of_buffer buf in
    List.iter
      (function
        | (Mdx.Document.Section _ | Text _) as t -> Mdx.pp_line ?syntax ppf t
        | Block t -> (
            List.iter (fun (k, v) -> Unix.putenv k v) (Block.set_variables t);
            try test_block ~ppf ~temp_file t
            with Failure msg -> raise (Test_block_failure (t, msg)) ))
      items;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  ( match output with
  | Some `Stdout -> Mdx.run_to_stdout ?syntax ~f:gen_corrected file
  | Some (`File outfile) ->
      Mdx.run_to_file ?syntax ~outfile ~f:gen_corrected file
  | None -> Mdx.run ?syntax ~force_output ~f:gen_corrected file );
  Hashtbl.iter (write_parts ~force_output) files

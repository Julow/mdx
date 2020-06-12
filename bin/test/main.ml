(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Mdx
open Compat

let eval_prelude ?root top prelude prelude_str =
  let aux to_lines p =
    let env, f = Mdx.Prelude.env_and_file p in
    let eval () = Mdx_test.eval_raw ?root top ~line:0 (to_lines f) in
    Mdx_top.in_env env eval
  in
  match (prelude, prelude_str) with
  | [], [] -> ()
  | [], fs -> List.iter (aux (fun x -> [ x ])) fs
  | fs, [] -> List.iter (aux Mdx.Util.File.read_lines) fs
  | _ -> Fmt.failwith "only one of --prelude or --prelude-str shoud be used"

let run_exn (`Setup ()) (`Non_deterministic non_deterministic)
    (`Silent_eval silent_eval) (`Record_backtrace record_backtrace)
    (`Syntax syntax) (`Silent silent) (`Verbose_findlib verbose_findlib)
    (`Prelude prelude) (`Prelude_str prelude_str) (`File file)
    (`Section section) (`Root root) (`Force_output force_output)
    (`Output output) =
  Printexc.record_backtrace record_backtrace;
  let c = Mdx_top.init ~verbose:(not silent_eval) ~silent ~verbose_findlib () in
  eval_prelude ?root c prelude prelude_str;
  Mdx_test.run c ~non_deterministic ?syntax ?section ?root ~force_output ?output file;
  0

let report_error_in_block block msg =
  let kind =
    match block.Block.value with
    | Raw _ -> ""
    | Include { file_kind = Fk_ocaml _; _ } -> "OCaml file include "
    | Include { file_kind = Fk_other _; _ } -> "file include "
    | OCaml _ -> "OCaml "
    | Cram _ -> "cram "
    | Toplevel _ -> "toplevel "
  in
  Fmt.epr "Error in the %scode block in %s at line %d:@]\n%s\n" kind block.file
    block.line msg

let run setup non_deterministic silent_eval record_backtrace syntax silent
    verbose_findlib prelude prelude_str file section root force_output output :
    int =
  try
    run_exn setup non_deterministic silent_eval record_backtrace syntax silent
      verbose_findlib prelude prelude_str file section root force_output output
  with
  | Failure f ->
      prerr_endline f;
      1
  | Mdx_test.Test_block_failure (block, msg) ->
      report_error_in_block block msg;
      1

(**** Cmdliner ****)

open Cmdliner

let cmd =
  let exits = Term.default_exits in
  let man = [] in
  let doc = "Test markdown files." in
  ( Term.(
      pure run $ Cli.setup $ Cli.non_deterministic $ Cli.silent_eval
      $ Cli.record_backtrace $ Cli.syntax $ Cli.silent $ Cli.verbose_findlib
      $ Cli.prelude $ Cli.prelude_str $ Cli.file $ Cli.section $ Cli.root
      $ Cli.force_output $ Cli.output),
    Term.info "ocaml-mdx-test" ~version:"%%VERSION%%" ~doc ~exits ~man )

let main () = Term.(exit_status @@ eval cmd)

let () = main ()

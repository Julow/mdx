open Mdx

type output = [ `File of string | `Stdout ]

exception Test_block_failure of Block.t * string

val eval_raw : Mdx_top.t -> ?block:Block.t -> ?root:string -> line:int -> string list -> unit

val run :
  Mdx_top.t ->
  ?non_deterministic:bool ->
  ?syntax:Mdx.syntax ->
  ?section:string ->
  ?root:string ->
  ?force_output:bool ->
  ?output:output ->
  string ->
  unit
(** Example usage:
    {[
      let mdx = Mdx_top.init () in
      Mdx_test.run mdx ~output:`Stdout "test.md";
    ]} *)

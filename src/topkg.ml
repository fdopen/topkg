(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

(* Results *)

include Topkg_result

(* Strings *)

let strf = Topkg_string.strf
module String = Topkg_string

(* Log *)

module Log = Topkg_log

(* OS interaction *)

type fpath = string

module Fpath = Topkg_fpath
module Cmd = Topkg_cmd
module OS = Topkg_os
module Vcs = Topkg_vcs

(* Package description *)

module Topkg = struct

  (* Parses the command line. The actual cmd execution occurs in the call
     to Pkg.describe. *)

  let err ?(stop = true) fmt =
    let k _ = if stop then exit 1 else () in
    Format.kfprintf k Format.err_formatter ("%s: " ^^ fmt ^^ "@.") Sys.argv.(0)

  let err_parse a = err "value of `%s' is not 'true' or 'false'" a
  let err_mdef a = err "bool `%s' is defined more than once" a
  let err_file f e = err "%s: %s" f e
  let err_miss a = err ~stop:false "boolean key '%s' is missing" a
  let warn_unused k = err ~stop:false "warning: environment key `%s` unused" k
  let err_cmd cmd = err "unknown command '%s'" cmd

  let cmd, env =
    let rec parse_env acc = function
    | key :: def :: defs ->
        begin try ignore (List.assoc key acc); err_mdef key; [] with
        | Not_found -> parse_env ((key, def) :: acc) defs
        end
    | [] -> (List.rev acc)
    | key :: [] -> err "key '%s' has no definition" key; exit 1
    in
    match List.tl (Array.to_list Sys.argv) with
    | "explain" :: args -> `Explain, parse_env [] args
    | ("help" | "-h" | "--help" | "-help") :: args -> `Help, parse_env [] args
    | "build" :: args -> `Build, parse_env [] args
    | "ipc" :: "lint" :: args -> `Lint (Cmd.of_list args), []
    | args -> `Unknown args, []
end

module Env = struct
  let error = ref false
  let env = ref []
  let get () = !env
  let add_bool key b = env := (key, (`Bool b)) :: !env
  let add_string key s = env := (key, (`String s)) :: !env
  let bool ?(quiet = false) ?(absent = fun () -> Ok true) key =
    let b = try bool_of_string (List.assoc key Topkg.env) with
    | Not_found ->
        if Topkg.cmd = `Build && not quiet
        then (error := true; Topkg.err_miss key; true) else
        begin match absent () with
        | Ok v -> v
        | Error (`Msg e) ->
            Log.err (fun m ->
                m "error while determining value for key '%s':\n%s" key e);
            true
        end
    | Invalid_argument _ ->
        Topkg.err_parse key; true
    in
    add_bool key b; b

  let string ?(quiet = false) ?(absent = fun () -> Ok "undefined") key =
    let s = try List.assoc key Topkg.env with
    | Not_found ->
       if Topkg.cmd = `Build && not quiet
       then (error := true; Topkg.err_miss key; "undefined") else
       begin match absent () with
       | Ok v -> v
       | Error (`Msg e) ->
           Log.err (fun m ->
               m "error while determining value for key '%s':\n%s" key e);
           "undefined"
       end
    in
    add_string key s; s

  module OCaml = Topkg_conf_ocaml

  (* Build context *)

  let human = bool "human"
  let vcs =
    let absent () = Vcs.find () >>= function vcs -> Ok (vcs <> None) in
    bool ~quiet:true ~absent "vcs"

  let build =
    if not vcs then `Release else
    if not human then `Pin else
    `Dev

  let error () = !error
end

module Exts = Topkg_fexts

module Private = struct
  module Codec = Topkg_codec
  module Lint = Topkg_lint
  module Opam = Topkg_opam
end

module Pkg = struct

  let nop = fun () -> Ok ()

  (* Distrib *)

  type watermark = Topkg_distrib.watermark
  let watermarks = Topkg_distrib.default_watermarks
  let files_to_watermark = Topkg_distrib.default_files_to_watermark

  type distrib = Topkg_distrib.t
  let commitish = Topkg_distrib.default_commitish
  let version = Topkg_distrib.default_version
  let paths_to_remove = Topkg_distrib.default_paths_to_remove
  let distrib = Topkg_distrib.v

  (* Linting *)

  type lint = Topkg_lint.t
  let lint = Topkg_lint.v

  (* Builder *)

  type build_cmd =
  [ `OCamlbuild of string list
  | `OCamlbuild_no_ocamlfind of string list
  | `Other of Cmd.t * string ]

  type builder =
  { watermark_on_pin : bool;
    pre : unit -> unit result;
    build_cmd : build_cmd;
    post : unit -> unit result; }

  let builder ?(watermark_on_pin = true) ?(pre = nop) ?(post = nop) build_cmd =
    { watermark_on_pin; pre; build_cmd; post }

  (* Install *)

  type install = Topkg_install.t
  type field = Topkg_install.field

  let lib = Topkg_install.lib
  let libexec = Topkg_install.libexec
  let bin = Topkg_install.bin
  let sbin = Topkg_install.sbin
  let toplevel = Topkg_install.toplevel
  let share = Topkg_install.share
  let share_root = Topkg_install.share_root
  let etc = Topkg_install.etc
  let doc = Topkg_install.doc
  let stublibs = Topkg_install.stublibs
  let misc = Topkg_install.misc
  let man = Topkg_install.man

  (* Package *)

  type t =
    { name : string;
      lint : lint;
      distrib : distrib;
      builder : builder;
      install : install; }

  let warn_unused () =
    let keys = List.map fst Topkg.env in
    let keys_used = List.map fst (Env.get ()) in
    let unused = List.find_all (fun k -> not (List.mem k keys_used)) keys in
    List.iter Topkg.warn_unused unused

  let pr = Format.printf
  let pr_explanation btool bdir pkg mvs  =
(*
    let env = Env.get () in
    let ext_to_string = Exts.ext_to_string (Env.OCaml.v `Host_os) in
    let exec_sep = " \\\n  " in
    let install, exec = build_strings ~exec_sep btool bdir ext_to_string mvs in
    pr "@[<v>";
    pr "Package name: %s@," pkg;
    pr "Build tool: %s@," btool;
    pr "Build directory: %s@," bdir;
    pr "Environment:@, ";
    List.iter (fun (k,v) ->
        match v with
        | `Bool b -> pr "%s = %b@, " k b
        | `String s -> pr "%s = %S@," k s) (List.sort compare env);
    pr "@,Build invocation:@,";
    pr " %s@,@," exec;
    pr "Install file:@,";
    pr "%s@," install;
    pr "@]";
*)
    pr "TODO";
    ()

  let pr_help () =
    pr "Usage example:@\n %s" Sys.argv.(0);
    List.iter (fun (k, v) -> match v with
    | `Bool b -> pr " %s %b" k b
    | `String s -> pr " %s %S" k s) (List.sort compare (Env.get ()));
    pr "@."

  let build pkg btool bdir = (* TODO cleanup and move to topkg_build *)
    if pkg.builder.watermark_on_pin && Env.build = `Pin then begin
      match Topkg_distrib.watermark_pin ~name:pkg.name pkg.distrib with
      | Error (`Msg e) ->
         Log.err (fun m -> m "Distribution watermaking failed: %s" e); exit 1
      | Ok () -> ()
    end;
    (pkg.builder.pre ())
    |> R.reword_error_msg ~replace:true
      (fun e -> R.msgf "Pre-build hook failed: %s" e)
    >>= fun () ->
    let header = pkg.name in
    let conf = Topkg_conf_ocaml.v `Host_os in
    let targets, install =
      Topkg_install.to_instructions ~header ~bdir conf pkg.install
    in
    let build_cmd = Cmd.(btool %% of_list targets) in
    Topkg_os.Cmd.run build_cmd
    >>= fun () ->
    let install_file = pkg.name ^ ".install" in
    let install = Topkg_opam.Install.to_string install in
    OS.File.write install_file install
    >>= fun () ->
    (pkg.builder.post ())
    |> R.reword_error_msg ~replace:true
      (fun e -> R.msgf "Post-build hook failed: %s" e)

  let do_standalone_main = ref true
  let prevent_standalone_main () = do_standalone_main := false

  let standalone_main pkg =
    Log.info (fun m -> m "topkg %%VERSION%% standalone main running");
    prevent_standalone_main ();
    let btool, bdir = match pkg.builder.build_cmd with
    | `OCamlbuild args ->
        Cmd.(v "ocamlbuild" % "-use-ocamlfind" % "-classic-display" %% args),
        "_build"
    | `OCamlbuild_no_ocamlfind args -> Cmd.(v "ocamlbuild" %% args), "_build"
    | `Other (btool, bdir) -> btool, bdir
    in
    match Topkg.cmd with
    | `Explain ->
        pr_explanation btool bdir pkg.name pkg.install
    | `Help -> pr_help ()
    | `Build ->
        warn_unused ();
        (build pkg btool bdir
         |> Log.on_error_msg ~use:(fun () -> ()))
    | `Lint args ->
        Topkg_lint.run_ipc pkg.lint args
        |> Log.on_error_msg ~use:(fun () -> ())
    | `Unknown args ->
        match args with
        | cmd :: _ -> Topkg.err_cmd cmd
        | [] -> Topkg.err "missing command"

  let pkg = ref None
  let describe
      ?(lint = lint ()) ?(distrib = distrib ()) name ~builder installs
    =
    match !pkg with
    | Some _ -> invalid_arg "Topkg.Pkg.describe already called"
    | None ->
        let install = List.sort compare (List.flatten installs) in
        let _pkg = { name; lint; distrib; builder; install } in
        pkg := Some _pkg;
        if !do_standalone_main then standalone_main _pkg else ()

  let err_no_main () =
    if !do_standalone_main && !pkg = None then begin
      Log.err (fun m -> m "No@ package@ description@ found.@ Did@ you@ \
                           forget@ to call Topkg.Pkg.describe ?")
    end

  let () = at_exit err_no_main
end

(** Default configuration. *)
module Config_default : sig
  val vars : (string * string) list
  (** [vars] is the list of variables to substitute, empty. *)

  val git_hook : string option
  (** [git_start_hook] is an ocaml script to invoke before a git package
      build, after variable substitution occured. *)

  val distrib_remove : string list
  (** [distrib_remove] is a list of files to remove before making
      the distributino tarball. *)

  val distrib_hook : string option
  (** [distrib_hook] is an ocaml script to invoke before trying
      to build the distribution. *)

  val www_demos : string list
  (** [www_demos] is a list of build targets that represent single page
      js_of_ocaml demo. *)
end = struct
  let vars = []
  let git_hook = None
  let distrib_remove = [".git"; ".gitignore"; "build"]
  let distrib_hook = None
  let www_demos = []
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

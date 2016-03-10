(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos
open Cmdliner

(* Manual *)

let common_opts = "COMMON OPTIONS"

let common_opts_man =
  [ `S common_opts; `P "These options are common to all commands." ]

let see_also ~cmds =
  let cmds = (String.concat ~sep:"(1), " ("topkg" :: cmds)) ^ "(1)" in
  [ `S "SEE ALSO";
    `P cmds ]

(* Converters and arguments *)

let path_arg =
  let parse s = match Fpath.of_string s with
  | None -> `Error (strf "%a: not a path" String.dump s)
  | Some s -> `Ok s
  in
  parse, Fpath.pp

let ignore_pkg =
  let doc = "Ignore package description file." in
  Arg.(value & flag & info ["i"; "ignore-pkg" ] ~doc)

(* Basic setup for every command *)

let setup style_renderer log_level cwd pkg_file =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ());
  Logs.info (fun m -> m "topkg-care %%VERSION%% running");
  match cwd with
  | None -> `Ok pkg_file
  | Some dir ->
      match OS.Dir.set_current dir with
      | Ok () -> `Ok pkg_file
      | Error (`Msg m) -> `Error (false, m) (* use cmdliner evaluation error *)

let setup =
  let docs = common_opts in
  let style_renderer =
    let env = Arg.env_var "TOPKG_COLOR" in
    Fmt_cli.style_renderer ~docs:common_opts ~env ()
  in
  let log_level =
    let env = Arg.env_var "TOPKG_VERBOSITY" in
    Logs_cli.level ~docs:common_opts ~env ()
  in
  let cwd =
    let doc = "Change to directory $(docv) before doing anything." in
    let docv = "DIR" in
    Arg.(value & opt (some path_arg) None & info ["C"; "pkg-dir"]
           ~docs ~doc ~docv)
  in
  let pkg_file =
    let doc = "Use $(docv) as the package description file" in
    let docv = "FILE" in
    Arg.(value & opt path_arg (Fpath.v "pkg/pkg.ml") & info ["pkg-file"]
           ~docs ~doc ~docv)
  in
  Term.(ret (const setup $ style_renderer $ log_level $ cwd $ pkg_file))

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

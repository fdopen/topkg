(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg

let () = Pkg.prevent_standalone_main ()

open Cmdliner

let cmds = [ Lint.cmd; Ipc.cmd ]
let main _ = `Help (`Pager, None)

(* Command line interface *)

let doc = "topkg package care"
let man =
  [ `S "DESCRIPTION";
    `P "$(b,$(mname)) lints and helps you with various aspects
        of the life cycle of topkg packages.";
    `P "Use '$(b,$(mname)) help $(i,COMMAND)' for information about
        $(i,COMMAND).";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
    `S "BUGS";
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information.";
    `S "AUTHOR";
    `P "Daniel C. Buenzli, $(i,http://erratique.ch)"; ]

let main =
  let version = "%%VERSION%%" in
  let info = Term.info "topkg" ~version ~doc ~sdocs:Cli.common_opts ~man in
  let t = Term.(ret (const main $ Cli.setup)) in
  (t, info)

let main () = match Term.eval_choice main cmds with
| `Error _ -> exit 1
| `Ok ret when ret <> 0 -> exit ret
| _ -> if Logs.err_count () > 0 then exit 1 else exit 0

let () = main ()

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

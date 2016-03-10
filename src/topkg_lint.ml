(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type t =
  { deps_excluding : string list option;
    files : string list option;
    custom : (unit -> R.msg result list);
    custom_result : R.msg result list; (* result of [custom] in Topkg_care *) }

let custom_default () = []
let v
    ?(deps_excluding = Some []) ?(files = Some []) ?(custom = custom_default) ()
  =
  { deps_excluding; files; custom; custom_result = [] }

let deps_excluding l = l.deps_excluding
let files l = l.files
let custom_result l = l.custom_result

(* Codec *)

let string_list_option = Topkg_codec.(option @@ list string)
let msg_result_list =
  let msg = (fun (`Msg m) -> m), (fun m -> `Msg m) in
  let msg = Topkg_codec.(view msg string) in
  Topkg_codec.(list @@ result ~ok:msg ~error:msg)

let codec =
  let deps = Topkg_codec.(with_kind "deps_excluding" @@ string_list_option) in
  let files = Topkg_codec.(with_kind "files" @@ string_list_option) in
  let res = Topkg_codec.(with_kind "custom_result" @@ msg_result_list) in
  let fields =
    (fun t -> t.deps_excluding, t.files, t.custom_result),
    (fun (deps_excluding, files, custom_result) ->
      { deps_excluding; files; custom = custom_default; custom_result })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"package lint" fields (t3 deps files res))

(* Run *)

let ipc_cmd ~custom =
  Topkg_cmd.(v "ipc" % "lint" %% on custom (v "run-custom"))

let lint lint ~custom =
  let custom_result = if custom then lint.custom () else [] in
  let lint = { lint with custom_result } in
  Topkg_codec.write Topkg_os.File.dash codec lint

let run_ipc l cmd = match Topkg_cmd.to_list cmd with
| "run-custom" :: [] -> lint l ~custom:true
| "ipc" :: [] -> lint l ~custom:false
| args -> R.error_msgf "ipc lint: unknown arguments %a" Topkg_cmd.dump cmd

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

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let dump_opam_file file =
  let err_file () = R.error_msgf "%S: can't parse file path" file in
  R.of_option ~none:err_file (Fpath.of_string file)
  >>= fun file -> Topkg_care.Opam.fields file
  >>= fun fs -> Ok (String.Map.bindings fs)
  >>= fun fs -> OS.File.(write dash (Topkg.Opam.Private.enc_fields fs))

let main () =
  let log fmt = Printf.eprintf (fmt ^^ "\n%!") in
  match Array.length Sys.argv with
  | n when n <> 2 -> log "usage: topkg-opam-fields FILE"; exit 1
  | _ ->
      match dump_opam_file Sys.argv.(1) with
      | Ok () -> exit 0
      | Error (`Msg m) -> log "topkg-opam-fields: %s" m; exit 1

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

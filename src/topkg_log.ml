(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type level = App | Error | Warning | Info | Debug

let _level =
  let default = Some Warning in
  let init =
    try match Sys.getenv "TOPKG_VERBOSITY" with
    | l when Topkg_string.is_prefix ~affix:"quiet" l -> None
    | l when Topkg_string.is_prefix ~affix:"error" l -> Some Error
    | l when Topkg_string.is_prefix ~affix:"warning" l -> Some Warning
    | l when Topkg_string.is_prefix ~affix:"info" l -> Some Info
    | l when Topkg_string.is_prefix ~affix:"debug" l -> Some Debug
    | l ->
        Format.eprintf "topkg: TOPKG_VERBOSITY env var unknown value: %S" l;
        default
    with Not_found | Sys_error _ -> default
  in
  ref init

let level () = !_level
let set_level l = _level := l

type 'a msgf =
  (('a, Format.formatter, unit) Pervasives.format -> 'a) -> unit

let _err_count = ref 0
let err_count () = !_err_count

let pp_level_header ppf l = Format.pp_print_string ppf begin match l with
| App -> ""
| Error -> "[ERROR] "
| Warning -> "[WARNING] "
| Info -> "[INFO] "
| Debug -> "[DEBUG] "
end

let msg level msgf = match !_level with
| None -> ()
| Some level' when level > level' -> if level = Error then incr _err_count
| Some _ ->
    if level = Error then incr _err_count;
    msgf @@
    (fun fmt ->
       Format.eprintf ("topkg: %a@[" ^^ fmt ^^ "@]@.") pp_level_header level)

let app msgf = msg App msgf
let err msgf = msg Error msgf
let warn msgf = msg Warning msgf
let info msgf = msg Info msgf
let debug msgf = msg Debug msgf

let on_error_msg ?(level = Error) ~use = function
| Ok v -> v
| Error (`Msg e) -> msg Error (fun m -> m "%s" e); use ()

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

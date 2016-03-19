(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf

include String

let head s = if s = "" then None else Some s.[0]

(* Predicates *)

let is_prefix ~affix s =
  let len_a = length affix in
  let len_s = length s in
  if len_a > len_s then false else
  let max_idx_a = len_a - 1 in
  let rec loop i =
    if i > max_idx_a then true else
    if unsafe_get affix i <> unsafe_get s i then false else loop (i + 1)
  in
  loop 0

let is_suffix ~affix s =
  let max_idx_a = length affix - 1 in
  let max_idx_s = length s - 1 in
  if max_idx_a > max_idx_s then false else
  let rec loop i =
    if i > max_idx_a then true else
    if unsafe_get affix (max_idx_a - i) <> unsafe_get s (max_idx_s - i)
    then false
    else loop (i + 1)
  in
  loop 0

let for_all sat s =
  let max_idx = length s - 1 in
  let rec loop i =
    if i > max_idx then true else
    if sat (unsafe_get s i) then loop (i + 1) else false
  in
  loop 0

let exists sat s =
  let max_idx = length s - 1 in
  let rec loop i =
    if i > max_idx then false else
    if sat (unsafe_get s i) then true else loop (i + 1)
  in
  loop 0

(* Traversing *)

let find_byte ?(start = 0) c s =
  let max = String.length s - 1 in
  if start > max then None else
  try Some (String.index_from s start c) with Not_found -> None

(* Extracting substrings *)

let with_index_range ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else
  String.sub s first (last - first + 1)

let cut ?(rev = false) ~sep s =
  let find_index = if rev then String.rindex else String.index in
  match try Some (find_index s sep) with Not_found -> None with
  | None -> None
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))

let cuts ~sep s =
  let rec loop acc s = match cut ~sep s with
  | Some (v, vs) -> loop (v :: acc) vs
  | None -> List.rev (s :: acc)
  in
  loop [] s



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

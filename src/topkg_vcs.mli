(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** VCS repositories.

    See {!Topkg.Vcs} for documentation. *)

open Topkg_result

type kind = [ `Git | `Hg ]
val pp_kind : Format.formatter -> kind -> unit

type t
val v : kind -> dir:Topkg_fpath.t -> t
val kind : t -> kind
val dir : t -> Topkg_fpath.t
val find : ?dir:Topkg_fpath.t -> unit -> t option result
val get : ?dir:Topkg_fpath.t -> unit -> t result
val pp : Format.formatter -> t -> unit

val is_dirty : t -> bool result
val not_dirty : t -> unit result
val head : ?dirty:bool -> t -> string result
val describe : ?dirty:bool -> ?commitish:string -> t -> string result
val tracked_files : ?treeish:string -> t -> Topkg_fpath.t list result

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

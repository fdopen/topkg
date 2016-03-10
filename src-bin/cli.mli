(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} and common definitions for commands. *)

open Cmdliner

(** {1 Manual sections and fragments} *)

val common_opts :string

val common_opts_man : Manpage.block list
(** [common_opts_man] is the manual section for common options. *)

val see_also : cmds:string list -> Manpage.block list
(** [see_also cmds] is a "see also" manpage fragment. *)

(** {1 Converters and arguments} *)

val path_arg : Fpath.t Arg.converter
(** [path_arg] is a path argument converter. *)

val ignore_pkg : bool Term.t
(** An [--ignore-pkg] option to ignore the package description file. *)

(** {1 Basic setup for every command} *)

val setup : Fpath.t Term.t
(** [setup env] defines a basic setup common to all commands and
    returns the package description file to read. The setup includes,
    by side effect, setting log verbosity for {!Logs} and ajusting
    colored output. *)

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

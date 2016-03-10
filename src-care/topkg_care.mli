(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Topkg package care.

    Tools to help the package developer in the life cycle of the
    package. Most of these tools can be invoked directly from the
    command line via the [topkg] binary installed by the [topkg-care]
    package.

    {b WARNING.} Do not use this API in your package description file, use
    only {!Topkg}. This API may change even between minor versions of
    [topkg].

    {e %%VERSION%% - {{:%%PKG_WWW%% }homepage}} *)

open Astring
open Rresult

(** {1 Package care} *)

(** [OPAM] helpers. *)
module Opam : sig

  module File : sig
    (** {1:file OPAM file} *)

    val field_names : String.set
    (** [field_names] is the maximal domain of the map returned by
        {!fields}, excluding extension fields (not yet supported by
        [opam-lib] 1.2.2). *)

    val fields : Fpath.t -> ((string list) String.map , R.msg) result
    (** [fields f] returns a simplified model of the fields of the OPAM
        file [f]. The domain of the result is included in
        {!field_names}. Note that the [depends:] and [depopts:] fields
        are returned without version contsraints. *)

    (** {1:deps Dependencies} *)

    val deps : ?opts:bool -> (string list) String.map -> String.set
    (** [deps ~opts fields] returns the packages mentioned in the [depends:]
        fields, if [opts] is [true] (default) those from [depopts:] are added
        aswell. *)
  end
end

(** [ocamlbuild] helpers. *)
module OCamlbuild : sig

  (** {1 Packages} *)

   val package_tags : ?roots:bool -> Fpath.t -> (String.set, R.msg) result
   (** [packages ~roots f] is the set of packages identifiers
       mentioned by the
       {{:https://github.com/gasche/manual-ocamlbuild/blob/master/manual.md#intro-ocamlfind}
       package tags} of the [_tags] file [f]. If [roots] is [true]
       (defaults to [false]) only root packages, i.e. the identifier
       before the first ['.'], are in the set.

       {b Warning.} This is a very dumb parsing that simply looks up
       for all ["package($ID)"] patterns in the [_tags] file. *)
end

(** [ocamlfind] helpers. *)
module OCamlfind : sig

   (** {1 Packages} *)

   val base_root_packages : String.set
   (** [base_root_packages] are the OCamlfind root packages that are
       distributed with OCaml. *)
end

(** Topkg package linter. *)
module Lint : sig

  (** {1:ldeps Dependencies}

      {b Note.} Dependency linting assumes that an OPAM package
      identified by [$ID] has an OCamlfind package whose root name is
      [$ID]. If you face exceptions you can add them to your package
      description see [Topkg]'s {{!Topkg.Pkg.pkglint}package linting
      description}. There are also a few predefined exclusions, see
      {!deps}. *)

  val deps : ?exclude:string list -> opam:Fpath.t -> tags:Fpath.t ->
    ((String.set * String.set) option, R.msg) result
  (** [deps ~exclude opam tags] is [Some (opam, tags)] with [opam] a
      set of packages dependencies (optional ones included) that are
      {{!Opam.deps}mentioned} only in the [opam] file and [tags] a set of
      root packages that are mentioned only in the [_tags] file. If both
      sets are empty then this is [None]. Names mentioned in [excluded]
      (defaults to [[]]) are excluded from the check. Besides the following
      names are automatically excluded:
      {ul
      {- [opam] package names that start with ["conf-"].}
      {- {!OCamlfind.base_root_packages}}} *)

  val pp_deps_mismatches : (String.set * String.set) Fmt.t
  (** [pp_deps_mismatches ppf miss] pretty prints missmatches returned
       by {!deps}. *)

  (** {1:lfile_exist File existence} *)

  type file = string * Fpath.set
  (** The type for file existence lints. A name for the file and the paths
      under which it can be found. *)

  val files : file list
  (** [files] is the list of standard files expected in a repository. *)

  val files_exist :
    ?err:(Fpath.t -> ('a, R.msg) result -> unit) ->
    dir:Fpath.t -> file list -> (file * bool) list
  (** [std_files ~dir fs] indicates for each [fs] whose name is not
      [excluded] whether it exists in [dir] or not. [err] is used to
      log file system errors, the default function does nothing. *)

  val pp_file_miss : (file * bool) Fmt.t
  (** [pp_file_miss ppf (f, exists)] is {!Fmt.nop} if [exist] is [true]
      or prints an error message otherwise. *)
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

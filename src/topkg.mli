(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** The trivial OCaml package builder.

    See the {{!manual}manual}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:res Results} *)

val ( >>= ) :
  ('a, 'b) Result.result -> ('a -> ('c, 'b) Result.result) ->
  ('c, 'b) Result.result
(** [r >>= f] is [f v] if [r = Ok v] and [r] if [r = Error _]. *)

(** This definition re-export [result]'s constructors so that an
    [open Topkg] will get them in scope. *)
type ('a, 'b) r = ('a, 'b) Result.result = Ok of 'a | Error of 'b

type 'a result = ('a, [ `Msg of string]) r
(** The type for topkg results. *)

(** Result value combinators. *)
module R : sig

  (** {1 Errors} *)

  val reword_error : ('b -> 'c) -> ('a, 'b) Result.result ->
    ('a, 'c) Result.result
  (** [reword_error reword r] is:
      {ul
      {- [r] if [r = Ok v]}
      {- [Error (reword e)] if [r = Error e]}} *)

  (** {1 Error messages} *)

  type msg = [ `Msg of string ]
  (** The type for (error) messages. *)

  val error_msg : string -> ('b, [> msg]) Result.result
  (** [error_msg s] is [Error (`Msg s)]. *)

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> msg]) Result.result) format4 -> 'a
  (** [error_msgf fmt ...] is an error formatted according to [fmt]. *)

  val reword_error_msg :
    ?replace:bool -> (string -> msg) -> ('a, msg) Result.result ->
    ('a, [> msg]) Result.result
  (** [reword_error_msg ~replace reword r] is like {!reword_error} except
      if [replace] is [false] (default), the result of [reword old_msg] is
      concatened, on a new line to the old message. *)
end

(** {1 Strings} *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf] is [Printf.sprintf]. *)

(** Strings. *)
module String : sig

  (** {1 String additions} *)

  include module type of String

  val head : string -> char option
  (** [head s] if [Some s.[0]] if [s <> ""] and [None] otherwise. *)

  (** {1:preds Predicates} *)

  val is_prefix : affix:string -> string -> bool
  (** [is_prefix ~affix s] is [true] iff [affix.[i] = s.[i]] for
      all indices [i] of [affix]. *)

  val is_suffix : affix:string -> string -> bool
  (** [is_suffix ~affix s] is true iff [affix.[n - i] = s.[m - i]] for all
      indices [i] of [affix] with [n = String.length affix - 1] and [m =
      String.length s - 1]. *)

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] is [true] iff for all indices [i] of [s], [p s.[i]
      = true]. *)

  val exists : (char -> bool) -> string -> bool
  (** [exists p s] is [true] iff there exists an index [i] of [s] with
      [p s.[i] = true]. *)

  (** {1:subs Extracting substrings} *)

  val with_index_range : ?first:int -> ?last:int -> string -> string
  (** [with_index_range ~first ~last s] are the consecutive bytes of [s]
      whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  val cut : ?rev:bool -> sep:char -> string -> (string * string) option
  (** [cut ~sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the
      first match of the separator character [sep] or [None] if
      [sep] can't be matched in [s]. Matching starts from the
      beginning of [s] ([rev] is [false], default) or the end ([rev]
      is [true]).

      The invariant [l ^ (String.make 1 sep) ^ r = s] holds. *)

  val cuts : sep:char -> string -> string list
  (** [cuts ~sep s] is the list of all substring of [s] that are delimited by
      matches of [sep]. The invariant
      [String.concat (String.make 1 sep) (split ~sep s) = s] holds. *)
end

(** {1 OS interaction} *)

type fpath = string
(** The type for file system paths. *)

(** Command lines.

    Both command lines and command line fragments using the same are
    represented with the same {{!t}type}.

    When a command line is {{!section:OS.Cmd.run}run}, the first
    element of the line defines the program name and each other
    element is an argument that will be passed {e as is} in the
    program's [argv] array: no shell interpretation or any form of
    argument quoting and/or concatenation occurs. *)
module Cmd : sig

  (** {1:lines Command line fragments} *)

  type t
  (** The type for command line fragments. *)

  val v : string -> t
  (** [v cmd] is a new command line (or command line fragment)
      whose first argument is [cmd]. *)

  val empty : t
  (** [empty] is an empty command line. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is empty. *)

  val ( % ) : t -> string -> t
    (** [l % arg] adds [arg] to the command line [l]. *)

  val ( %% ) : t -> t -> t
  (** [l %% frag] appends the line fragment [frag] to [l]. *)

  val add_arg : t -> string -> t
  (** [add_arg l arg] is [l % arg]. *)

  val add_args : t -> t -> t
  (** [add_args l frag] is [l %% frag]. *)

  val on : bool -> t -> t
  (** [on bool line] is [line] if [bool] is [true] and {!empty}
      otherwise. *)

  val p : fpath -> string
  (** [p] is [(fun f -> f)]. *)

  (** {1:predicates Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal l l'] is [true] iff [l] and [l'] are litterally equal. *)

  val compare : t -> t -> int
  (** [compare l l'] is a total order on command lines. *)

  (** {1:convert Conversions and pretty printing} *)

  val to_list : t -> string list
  (** [to_list l] is [l] as a list of strings. *)

  val of_list : ?slip:string -> string list -> t
  (** [of_list ?slip l] is a command line from the list of arguments
      [l].  If [slip] is specified it is added on the command line
      before each element of [l]. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf cmd] formats an unspecified representation of [cmd] on
      [ppf]. *)
end

(** Topkg log. *)
module Log : sig

  (** {1 Reporting levels} *)

  (** The type for reporting levels. *)
  type level = App | Error | Warning | Info | Debug

  val level : unit -> level option
  (** [level ()] is the current reporting level. *)

  val set_level : level option -> unit
  (** [set_level l] sets the current reporting level to [l]. *)

  (** {1 Log functions} *)

  type 'a msgf =
    (('a, Format.formatter, unit) Pervasives.format -> 'a) -> unit

  val msg : level -> 'a msgf -> unit
  (** [msg l (fun m -> m fmt ...)] logs with level [l] a message formatted
      with [fmt]. *)

  val app : 'a msgf -> unit
  (** [app] is msg [App]. *)

  val err : 'a msgf -> unit
  (** [err] is [msg Error]. *)

  val warn : 'a msgf -> unit
  (** [err] is [msg Warning]. *)

  val info : 'a msgf -> unit
  (** [err] is [msg Info]. *)

  val debug : 'a msgf -> unit
  (** [err] is [msg Debug]. *)

  val on_error_msg : ?level:level -> use:(unit -> 'a) -> 'a result -> 'a
  (** [on_error_msg ~level r] is:
      {ul
      {- [v] if [r = Ok v]}
      {- [use e] if [r = Error (`Msg e)]. As a side effect [e] is logged
         with level [level] (defaults to {!Error}).}} *)

  (** {1 Monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]
      across all sources. *)
end

(** OS interaction. *)
module OS : sig

  (** {1 OS} *)

  (** Environment variables *)
  module Env : sig

    (** {1 Variables} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name], if
        defined. *)

    val opt_var : string -> absent:string -> string
    (** [opt_var name ~absent] is the value of the optionally defined
        environment variable [name], if defined, and [absent] if undefined. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:paths Famous file paths} *)

    val null : fpath
    (** [null] represents a file on the OS that discards all writes
        and returns end of file on reads. *)

    val dash : fpath
    (** [dash] is ["-"]. This value is is used by {!read} and {!write}
        to respectively denote [stdin] and [stdout]. *)

    (** {1:exdel Existence and deletion} *)

    val exists : fpath -> bool result
    (** [exists file] is [true] if [file] is a regular in the file
        system and false otherwise. Symbolic links are followed. *)

    val must_exist : fpath -> fpath result
    (** [must_exist file] is [file] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    val delete : ?must_exist:bool -> fpath -> unit result
    (** [delete ~must_exist file] deletes file [file]. If [must_exist]
        is [true] (defaults to [false]) an error is returned if [file]
        doesn't exist. *)

    (** {1:rw Reading and writing} *)

    val read : fpath -> string result
    (** [read file] is [file]'s contents. If [file] is {!dash} reads
        from {!Pervasives.stdin} and the channel is not closed when
        the function returns. *)

    val write : fpath -> string -> unit result
    (** [write file content] writes [content] to [file]. If [file] is {!dash}
        writes to {!Pervasives.stdout} and flushes but doesn't close the channel
        when the function returns. *)

    (** {1:tmpfiles Temporary files} *)

    val tmp : unit -> fpath result
    (** [tmp ()] creates a temporary file and returns its name. The file
        is destroyed at the end of program execution. *)
  end

  (** Directory operations. *)
  module Dir : sig

    (** {1:exists Existence} *)

    val exists : fpath -> bool result
    (** [exists dir] is [true] if directory [dir] exists in the file
        system. Symbolic links are followed. *)

    val must_exist : fpath -> fpath result
    (** [must_exist dir] is [dir] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    (** {1:current Current working directory} *)

    val current : unit -> fpath result
    (** [current ()] is the current working directory. *)

    val set_current : fpath -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)

    (** {1:fold Folding over files} *)

    val fold_files :
      ?skip:(fpath -> bool) -> (fpath -> 'a -> 'a result) -> 'a ->
      fpath list -> 'a result
    (** [fold_files skip f acc paths] folds [f] over the {e files}
        found in the file hierarchies starting at [paths].  Files
        and directories [p] for which [skip p] is [true] are skipped.
        [skip] defaults to [(fun _ -> false)]. *)
  end

  (** Running commands.

      {b Warning.} All the following function raise [Invalid_argument]
      on {!Cmd.empty} commands. *)
  module Cmd : sig

    (** {1:exists Command existence} *)

    val exists : Cmd.t -> bool result
    (** [exists cmd] is [true] if the executable of [cmd] can be found
        in the path and [false] otherwise. *)

    val must_exist : Cmd.t -> Cmd.t result
    (** [must_exist cmd] is [cmd] if the executable of [cmd] can be found
        in the path and an error otherwise. *)

    (** {1:run Running commands} *)

    val run : ?err:fpath -> Cmd.t -> unit result
    (** [run cmd] runs the command [cmd]. [std{i,o,err}] are connected
        to the invoking process' standard channels. If [err] is specified
        [stderr] is redirected to the given file (e.g. {!File.null}). *)

    val run_status : ?err:fpath -> Cmd.t -> [`Exited of int] result
    (** [run_status cmd] is like {!run}, but doesn't error on non-zero
        exit status. *)

    (** {1 Capturing standard output} *)

    type run_status = Cmd.t * [`Exited of int ]
    (** The type for run statuses, the command that was run and the run
        status. *)

    val success : ('a * run_status) result -> 'a result
    (** [success r] is:
        {ul
        {- [Ok v] if [r = Ok (v, (_, `Exited 0))]}
        {- [Error _] otherwise. Non [`Exited 0] statuses are turned into
           an error message.}} *)

    type run_out
    (** The type for representing the standard output of a command run. *)

    val out_string : ?trim:bool -> run_out -> (string * run_status) result
    (** [out_string ~trim o] captures the standard output [o] as a [string].
        If [trim] is [true] (default) the result is passed through
        {!String.trim}. *)

    val out_lines : ?trim:bool -> run_out -> (string list * run_status) result
    (** [out_lines ~trim] is like {!out_string} but the result is
        {{!String.cuts}cut} on newlines (['\n']). *)

    val out_file : fpath -> run_out -> (unit * run_status) result
    (** [out_file f o] writes the standard output [o] to file [f]. *)

    val to_string : ?trim:bool -> run_out -> string result
    (** [to_string] is [(out_string ?trim o |> success)]. *)

    val to_lines : ?trim:bool -> run_out -> string list result
    (** [to_lines ?trim o] is [(out_string ?trim o |> success)]. *)

    val to_file : fpath -> run_out -> unit result
    (** [to_file f o] is [(out_file f o |> success)] *)

    val run_out : ?err:fpath -> Cmd.t -> run_out
    (** [run_out cmd] represents the standard output of the command run [cmd].
        [std{i,err}] are connected to the invoking prcoess stream and standard
        output can be consumed with {!to_string}, {!to_lines} or {!to_file}.
        If [err] is specified [stderr] is redirected to the given file. *)
  end

  val exit : 'a result -> unit
  (** [exit r] is
      {ul
      {- [exit 0] if [r = Ok _]}
      {- Prints [m] on stderr and [exit 1] if [r = Error (`Msg m)].}} *)
end

(** {1 VCS repositories} *)

(** Version control system repositories. *)
module Vcs : sig

  (** {1:vcsops Version control system repositories} *)

  type kind = [ `Git | `Hg ]
  (** The type for version control systems (VCS). *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on [ppf]. *)

  type t
  (** The type for version control systems repositories. *)

  val v : kind -> dir:fpath -> t
  (** [v k ~dir] is a VCS repository of kind [k] with repository directory [dir]
      (for git this is the [.git] directory, not the VCS working directory). *)

  val kind : t -> kind
  (** [kind r] is [r]'s VCS kind. *)

  val dir : t -> fpath
  (** [dir r] is [r]'s repository directory. *)

  val find : ?dir:fpath -> unit -> t option result
  (** [find ~dir ()] looks for a VCS repository in working directory [dir]
      (not the repository directory like [.git], default is guessed
       automatically). *)

  val get : ?dir:fpath -> unit -> t result
  (** [get] is like {!find} but returns an error if no VCS was found. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf r] prints an unspecified representation of [r] on [ppf]. *)

  (** {1:state Repository state} *)

  val is_dirty : t -> bool result
  (** [is_dirty r] is [Ok true] iff the working tree of [r] is dirty. *)

  val not_dirty : t -> unit result
  (** [not_dirty] is [Ok ()] iff the working directory of [r] is not dirty and
      an error that enjoins to stash or commit otherwise. *)

  val head : ?dirty:bool -> t -> string result
  (** [head ~dirty r] is the HEAD commit identifier of the repository
      [r]. If [dirty] is [true] (default), and indicator is appended to
      the commit identifier if the working tree of [r] {!is_dirty}. *)

  val describe : ?dirty:bool -> ?commitish:string -> t -> string result
  (** [describe ~dirty ~commitish r] identifies [commitish] (defaults to
      ["HEAD"]) using tags from the repository [r]. If [commitish] is
      ["HEAD"] and [dirty] is [true] (default) an indicator is appended to
      the identifier if the working tree is dirty. *)

  val tracked_files : ?treeish:string -> t -> fpath list result
  (** [tracked_files ~treeish r] are the files tracked by the tree object
      [treeish] (defaults to ["HEAD"]). *)
end

(** {1 Package description} *)

(** Build environment. *)
module Env : sig

  (** {1 Environment keys} *)

  val bool : ?quiet:bool -> ?absent:(unit -> bool result) -> string -> bool
  (** [bool key] declares [key] as being a boolean key in the
      environment.  A pair of positional arguments [key value] with
      [value] either ["true"] or ["false"] must now be specified
      on the command line. If [quiet] is [true] (defaults to [false])
      the absence of the pair on the command line does not generate a warning.
      If [absent] is specified (defaults to [(fun () -> Ok true)], the function
      is used to determined the key value in case it is missing on the
      command line. *)

  val string :
    ?quiet:bool -> ?absent:(unit -> string result) -> string -> string
  (** [string key] is like {!bool} but gets an arbitrary string from the
      command line. [absent] defaults to [(fun () -> Ok "undefined")]. *)

  (** {1 OCaml configuration} *)

  (** OCaml configuration. *)
  module OCaml : sig

    (** {1 Tools} *)

    type os = [ `Build_os | `Host_os ]
    (** The type for operating systems.
        {ul
        {- [`Build_os] is the build OS, the OS on which the package is built.}
        {- [`Host_os] is the host OS, the OS on which the package is hosted
           and runs.}} *)

    val tool : string -> os -> Cmd.t
    (** [tool bin os] is a build OS binary [bin] which produces output
        suitable for the OS [os].
        The executable name for [tool "ocamlc" `Host_os] (resp.
        [`Build_os]) is determined, in order, as follows:
        {ul
        {- If the environment variable [HOST_OS_OCAMLC] (resp.
           [BUILD_OS_OCAMLC])
           is set to ["bin"], ["bin"] is used for the executable name.}
        {- If the environment variable [HOST_OS_XBIN] (resp. [BUILD_OS_BIN]) is
           set to ["path"], the value ["path/ocamlc"] is used for the
           executable name.}
        {- If the environment variable [HOST_OS_SUFF] (resp. [BUILD_OS_SUFF])
           is set to [suff], the value [path/ocamlcsuff] is used.}c
        {- Otherwise ["ocamlc"] is used.}} *)

    (** {1 OCaml system configuration} *)

    type t
    (** The type for OCaml configurations. *)

    val v : os -> t
    (** [v os] is the configuration of the OCaml system for the OS
        [os] obtained by reading the output of [tool "ocamlc" os] with
        the [-config] option. *)

    val find : string -> t -> string option
    (** [find k c] looks up key [k] in configuration [c]. *)

    val version : t -> int * int * int * string option
    (** [version] is the compiler version string
        ["major.minor[.patchlevel][+additional-info]"] parsed into
        [(major, minor, patch, additional-info)]. *)

    val ext_asm : t -> string
    (** [ext_asm] is the file extension for assembly files, e.g. [".s"]. *)

    val ext_obj : t -> string
    (** [ext_asm] is the file extension for C object files, e.g. [".o"]. *)

    val ext_lib : t -> string
    (** [ext_lib] is the file extension for C static libraries, e.g. [".a"]. *)

    val ext_dll : t -> string
    (** [ext_dll] is the file extension for C dynamically linked libraries,
        e.g. [".so"]. *)

    val ext_exe : t -> string
    (** [ext_exe] is the file extension for binary executables, e.g. [".exe"]
        or [""]. Until at least OCaml 4.03 this information is not readily
        available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        discovered as described in
        {{:http://lists.ocaml.org/pipermail/wg-windows/2015-July/000037.html}
        this message}. *)

    val native : t -> bool
    (** [native] is [true] if native compilation (i.e. [ocamlopt]) is
        available. Until at least OCaml 4.03 this information is not
        readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        [true] is returned iff the standard library directory has the
        [libasmrun] C static library. *)

    val native_dynlink : t -> bool
    (** [native_dynlink] is [true] if native dynamic linking is
        available.  Until at least OCaml 4.03 this information is not
        readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        [true] is returned iff the standard library directory has the
        [dynlink.cmxa] library. *)

    val dump : Format.formatter -> t -> unit
    (** [dump ppf c] prints an unspecified representation of [c] on [ppf]. *)
  end

  (** {1:build_context Build context}

      The two variables {!vcs} and {!human} are used to detect build
      contexts.  *)

  val vcs : bool
  (** [vcs] is [bool "vcs"]. Usually this variable should not be specified
      manually, it is determined automatically by using {!Vcs.find}. *)

  val human : bool
  (** [human] is [bool "human"]. Must be set to [false] if the
      build is initiated by a package installer like OPAM.  *)

  val build : [ `Release | `Dev | `Pin ]
  (** [build] is:
      {ul
      {- [`Release] iff [not vcs]. This is a build from a release.
         If there are configuration bits they should be setup according to
         the build environment.}
      {- [`Dev] iff [vcs && human]. This is a development build invoked
         manually from a repository checkout. The repository checkout
         should likely not be touched and configuration bits not be setup.
         This is happening for example for testing the package description.}
      {- [`Pin] iff [vcs && not human]. This is a package manager pin build.
         In this case the repository checkout may need to be massaged
         into a pseudo-release for the package to be installed. For example
         to specify the version watermarks or if the install depends on files
         that are generated at release time. Existing configuration bits
         should also be setup according to the build environment.}} *)
end

(** Exts defines sets of file extensions. *)
module Exts : sig

  type ext = [`Ext of string | `Obj | `Lib | `Dll | `Exe]
  (** The type for extensions. *)

  type t = ext list
  (** The type for sets (list) of file extensions. *)

  val interface_opt_opaque : ext list
  (** [interface_opt_opaque] is [[".mli"; ".cmi"; ".cmti"]] *)

  val interface : ext list
  (** [interface] is [".cmx" :: interface] *)

  val c_library : ext list
  (** [c_library] is the extension for C libraries. This is determined
      from {!Env.OCaml.host}. *)

  val c_dll_library : ext list
  (** [c_dll_library] is the extension for C dynamic libraries. This
      is determined from {!Env.OCaml.host}. *)

  val library : ext list
  (** [library] is [[".cma"; ".cmxa"; ".cmxs"] @ c_library] *)

  val module_library : ext list
  (** [module_library] is [(interface @ library)]. *)

  val exe : ext list
  (** [exe] is the extension for executables. This is determined from
      {!Env.OCaml.host}. *)

  val exts : string list -> ext list
  (** [exts sl] is [sl] as a list of extensions. *)

  val ext : string -> ext list
  (** [ext s] is [s] as a list of extensions. *)
end

(** Package description. *)
module Pkg : sig

  (** {1:builder Package builder} *)

  type build_cmd =
  [ `OCamlbuild of string list
  | `OCamlbuild_no_ocamlfind of string list
  | `Other of Cmd.t * string ]
  (** The type for build command.
      {ul
      {- [`OCamlbuild args], [ocamlbuild] is invoked with [-use-ocamlfind],
         [-classic-display] and [args].}
      {- [`OCamlbuild_no_ocamlfind args], [ocamlbuild] is invoked with
         [args]}
      {- [`Other (cmd, bdir)], command [cmd] is invoked that generates
         its build artefacts in [bdir].}} *)

  type builder
  (** The type for package builder specifications. *)

  val builder :
    ?watermark_on_pin:bool -> ?pre:(unit -> unit result) ->
    ?post:(unit -> unit result) -> build_cmd -> builder
  (** [builder ~watermark_on_pin ~pre ~post cmd] defines the command that will
      be invoked with the targets to build as determined by {{!install}install
      moves}. The optional arguments are:
      {ul
      {- [watermark_on_pin] if [true] (default) {{!watermark}distribution
         watermarking}
         is performed if a [`Pin] {{!Env.build}build context} is detected.}
      {- [pre] is a hook that is invoked, after watermarking if applicable,
         but before the build command. It can be used to adapt the build
         setup according to the {{!Env.build}build context}. Default is a nop.}
      {- [post] is a hook that is invoked after the build command.
         Default is a nop.}}. *)

  (** {1:install Package install fields}

      Package install field function generate [.install] file moves
      from the build directory to the install directories. In turns
      this determines the files that need to be build by the
      {{!builder}package builder}. *)

  type install
  (** The type for install moves. *)

  type field =
    ?built:bool -> ?cond:bool -> ?exts:Exts.t ->
    ?dst:string -> string -> install
  (** The type for field install functions. A call
      [field ~cond ~exts ~dst path] generates install moves as follows:
      {ul
      {- If [built] is [true] (defaults), [path] is expressed relative
         to the build directory of the {{!builder}package builder}.
         If [false], [path] is relative to the root of the distribution.}
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- If [exts] is present, generates a move for each path in
         the list: [List.map (fun e -> path ^ e) exts].}
      {- [dst] is the path used as the move destination, relative to
         directory corresponding to the field. If unspecified this is
         [Filename.basename path]}} *)

  val lib : field
  (** [lib] package specific directory. *)

  val libexec : ?auto:bool -> field
  (** [lib] global directory with executable bit set. See {!bin}. *)

  val bin : ?auto:bool -> field
  (** [bin] global directory. If [auto] is [true], selects the native
      binary over the byte code one according to the value of
      {!Env.native} and adds {!Exts.exe} to the destination (which
      does the right thing on Windows). *)

  val sbin : ?auto:bool -> field
  (** [sbin] global directory. See {!bin}. *)

  val toplevel : field
  (** [toplevel] package specific directory. *)

  val share : field
  (** [share] package specific directory. *)

  val share_root : field
  (** [share_root] global share directory. *)

  val etc : field
  (** [etc] package specific directory. *)

  val doc : field
  (** [doc] package specific directory. *)

  val stublibs : field
  (** [stublibs] global OCaml C stub directory. *)

  val misc : field
  (** [misc] absolute path, interactive, install,
      see the {{:https://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
      OPAM manual} for details. *)

  val man : field
  (** [man] global directory,
      see the {{:https://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
      OPAM manual} for details. *)

  (** {1 Package distribution creation} *)

  type watermark = string * [ `String of string | `Version | `Name
                            | `Opam of string * string * string]
  (** The type for watermarks. A watermark identifier, e.g. ["ID"] and its
      definition:
      {ul
      {- [`String s], [s] is the definition.}
      {- [`Version], is the version of the distribution.}
      {- [`Name], is the name of the distribution.}
      {- [`Opam (file, field, sep)], is the values of the field [field]
         concatenated with separator [sep] of the OPAM file [file], expressed
         relative to the root of the source repository. Not all fields are
         supported see the value of {!Topkg_care.Opam.field_names}.
         {b Warning.} In [`Pin] {!Env.build}s, [`Opam] watermarks will
         only get substituted if the package [topkg-care] is installed.}}

      When a file is watermarked with an identifier ["ID"], any occurence of
      the sequence [%%ID%%] in its content is substituted by its definition. *)

  val watermarks : watermark list
  (** [watermarks] is the default list of watermarks. It has the following
      elements:
      {ul
      {- [("NAME", `Name)], the name of the package.}
      {- [("VERSION", `Version)], the version of the distribution.}
      {- [("PKG_MAINTAINER", `Opam ("opam", "maintainer", ", "))]}
      {- [("PKG_AUTHORS", `Opam ("opam", "authors", ", ")]}
      {- [("PKG_HOMEPAGE", `Opam ("opam", "homepage", " ")]}
      {- [("PKG_ISSUES", `Opam ("opam", "bug-reports", " ")]}
      {- [("PKG_DOC", `Opam ("opam", "doc", " "))]}
      {- [("PKG_LICENSE", `Opam ("opam", "license", ", ")]}
      {- [("PKG_REPO", `Opam ("opam", "dev-repo", " "))]}}
      Prepending to the list will override default definitions. *)

  val files_to_watermark : unit -> fpath list result
  (** [files_to_watermark ()] is the default list of files to watermark,
      obtained by {{!Vcs.get}getting} the VCS repo of the release checkout
      and consulting the set of {{!Vcs.tracked_files}tracked files}. *)

  val commitish : unit -> string result
  val version : string -> string result

  val paths_to_remove : unit -> fpath list result
  (** [paths_to_remove ()] is the default list of paths to remove once
      the repository has been checked out to create a distribution.
      The default is:
      {[fun () -> Ok [".git"; ".gitignore"; "build"; "Makefile"; "_build"]} *)

  type distrib
  (** The type for specifying the distribution creation. *)

  val distrib :
    ?commitish:(unit -> string result) ->
    ?version:(string -> string result) ->
    ?watermarks:watermark list ->
    ?files_to_watermark:(unit -> fpath list result) ->
(*
    ?massage:(unit -> unit result) ->
    ?paths_to_remove:(unit -> fpath list result) ->
    ?lint:bool ->
    ?test_build:bool ->
*)
    unit -> distrib
  (** [distrib ~commitish ~version ~watermarks ~files_to_watermark ~prepare
      ~paths_to_remove ~lint ~test_build ()] describes the distribution
      creation process as it will be performed by the [topkg] tool.

      Note that the [topkg] tool allows to override (e.g. the
      version string) or disable (e.g. skip linting) part of the of
      the process via command line arguments; see [topkg distrib
      --help] for more information.

      In a repository checkout [HEAD] for a package {{!describe}named}
      [$NAME], with [$BUILD] the build directory specified by the
      {{!builder}package builder}, the [topkg] tool will construct the
      distribution as described below.
      {ol
      {- Invoke the [commitish] function (defaults to {!commitish}) of
         [HEAD]'s [pkg/pkg.ml] to determine the commitish
         [$COMMIT] to checkout to make the distribution.}
      {- Make a private repo checkout of [commitish] in
         [$BUILD/$NAME-$COMMIT].}
      {- {b From now on we use the definitions of the description
         [$BUILD/$NAME-$COMMIT/pkg/pkg.ml] of this checkout as it has
         the correct description for this state of the package.}}
      {- Invoke [version] with [$COMMIT] to get a version string.
         [version] defaults to {!version} (the [topkg] tool also
         allows to specify a version string manually on the command
         line).}
      {- Watermark the files mentioned [files_to_watermark ()] with
         [watermarks] (respectively default to {!files_to_watermark} and
         {!watermarks}.)}
      {- Invoke the [massage] hook (default is a nop). This can be used
         to create distribution time build artefacts. The current working
         directory when running this is the root of the private repo checkout}
      {- Remove the paths mentioned by [paths_to_remove].}
      {- Create a distribution tarball [name-version.tbz] with the resulting
         file hierarchy and deletes the private repo checkout.}} *)

  (** {1:pkglint Package distribution linting} *)

  type lint
  (** The type for specifying distribution linting.  *)

  val lint :
    ?deps_excluding:string list option ->
    ?files:fpath list option ->
    ?custom:(unit -> R.msg result list) -> unit -> lint
  (** [lint] specifies the package's distribution lint:
    {ul
    {- [deps_excluding] if [Some excludes], performs
       {{!Topkg_care.Lint.ldeps}dependency
       linting} except for the package identifiers mentioned in [excludes].
       Defaults to [Some []]. If [None] disables dependency linting for
       the package.}
    {- [files] if [Some files], performs
       {{!Topkg_care.Lint.lfile_exist}file existence linting}
       and also checks that [files] are in the distribution. Defaults
       to [Some []]. If [None] disables file existence linting.}
    {- [custom] defines a custom linting process run with the current
       directory set at the root of the distribution. Successes and errors
       in the returned list are reported as such and any error in the list
       makes the lint fail. Defaults to [(fun () -> [])].}} *)

  (** {1 Package description} *)

  val describe :
    ?lint:lint ->
    ?distrib:distrib ->
    string -> builder:builder -> install list -> unit
  (** [describe name ~lint ~distrib ~builder installs] describes a package named
      [name] with lint specifiation [lint] distribution specification
      [distrib], builder [builder] and install moves [installs]. *)

  (**/**)
  type t
  val pkg : t option ref
  val prevent_standalone_main : unit -> unit
  (**/**)
end

(** Private definitions.

    {b Warning.} [Topkg] users {b must not} use these definitions
    to describe their package. *)
module Private : sig

  (** {1 Private} *)

  (** Topkg interprocess communication codec.

      Codecs for communication between the topkg tools and the topkg
      description [pkg/pkg.ml]. *)
  module Codec : sig

    (** {1 Decode errors} *)

    (** The type for decode errors.
        {ul
        {- [Corrupted (kind, data)], an error occured while decoding
           [data] for [kind].}
        {- [Version (exp, fnd)], a {{!version}versioned} decoder
           expected version [exp] but found [fnd]}} *)
    type error = Corrupted of (string * string) | Version of int * int

    val pp_error : Format.formatter -> error -> unit
    (** [pp_error ppf e] prints an unspecified representation of [e]
        on [ppf]. *)

    exception Error of error
    (** Raised on decode errors. *)

    (** {1 Codecs} *)

    type 'a t
    (** The type for codec for OCaml values of type ['a]. *)

    val v : kind:string -> enc:('a -> string) -> dec:(string -> 'a) -> 'a t
    (** [v kind enc dec] is a codec for value identified as [kind] using
        [enc] to encode and [dec] to decode. *)

    val kind : 'a t -> string
    (** [kind c] is [c]'s kind. *)

    val enc : 'a t -> 'a -> string
    (** [enc c] is [c]'s encoder. *)

    val dec : 'a t -> string -> 'a
    (** [dec c] is [c]'s decoder. The decoder @raise Error in case of
        decode error *)

    val dec_result : 'a t -> string -> 'a result
    (** [dec c] is like {!dec} but doesn't raise. The exception is
        turned into an error message using {!pp_error}. *)

    val with_kind : string -> 'a t -> 'a t
    (** [with_kind k c] is [c] with kind [k]. *)

    val write : fpath -> 'a t -> 'a -> unit result
    (** [write f c v] encodes value [v] with [c] to [f]. *)

    val read : fpath -> 'a t -> 'a result
    (** [read f c] reads a value with [c] from [f]. *)

    (** {1 Base type codecs} *)

    val bool : bool t
    (** [bool] codecs booleans. *)

    val int : int t
    (** [int] codecs integers. *)

    val string : string t
    (** [string] codecs strings. *)

    val option : 'a t -> 'a option t
    (** [option el] codecs [el] options. *)

    val result : ok:'a t -> error:'b t -> ('a, 'b) Result.result t
    (** [result ~ok ~error] codecs [ok], [error] results. *)

    val list : 'a t -> 'a list t
    (** [list el] codecs [el] lists. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair c0 c1] codecs [c0], [c1] pairs. *)

    val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
    (** [t3] is like {!pair} but for triples. *)

    val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
    (** [t4] is like {!pair} but for quadruples. *)

    val t5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t ->
      ('a * 'b * 'c * 'd * 'e) t
    (** [t5] is like {!pair} but for qintuples. *)

    val version : int -> 'a t -> 'a t
    (** [version num c] versions codec [c] with number [num].
        On decode a version number mismatch raises an error
        see {!error}. *)

    val view : ?kind:string -> ('a -> 'b) * ('b -> 'a) -> 'b t -> 'a t
    (** [view kind t c] views [t] as [c] for codecing. *)
  end

  (** Package distribution linting. *)
  module Lint : sig

    (** {1 Linting} *)

    type t = Pkg.lint
    (** The type for package distribution linting descriptions. *)

    val deps_excluding : t -> string list option
    (** [deps_excluding l] is [l]'s dependency linting description,
        see {!Pkg.lint}. *)

    val files : t -> fpath list option
    (** [files l] is [l]'s file existence linting descriptions,
        see {!Pkg.lint}. *)

    val custom_result : t -> R.msg result list
    (** [custom l] is [l]'s custom linting process result (if run),
        see {!Pkg.lint}. *)

    val codec : t Codec.t
    (** [codec] is a codec for linting descriptions. *)

    val ipc_cmd : custom:bool -> Cmd.t
    (** [ipc_cmd custom] is the command line arguments to give to the
        package description to run and get the linting description
        encoded on [stdout]. If [custom] is [false] the custom linting
        process is not run. *)
  end

  (** OPAM helpers. *)
  module Opam : sig

    (** {1 OPAM} *)

    (** OPAM package file access.

        Normally OPAM metadata access is only needed at distribution
        time and this is handled by {!Topkg_care.Opam.File} using the
        [opam-lib] library.

        However there is one case where we want to be able to access
        the metadata from [Topkg]: on pin builds where the
        {{!Pkg.watermark}watermarking} process needs to be run to turn
        the repo into a pseudo-release.

        Since we don't want [Topkg] to have any dependency and that
        [opam] currently doesn't allow to consult the fields of arbitrary
        OPAM files (see
        {{:https://github.com/ocaml/opam/issues/2446} issue #2446}) we
        assume a pin build will have the [topkg] tool installed and call
        to it to get the OPAM fields for watermarking (if [topkg] is
        unavailable the watermarks will simply be undefined). *)
    module File : sig

      (** {1 OPAM file} *)

      type t = (string * string list) list
      (** The type for a simplified model the fields of an OPAM
          file. See {!Topkg_care.Opam.File}. *)

      val codec : t Codec.t
      (** [codec] is a codec for OPAM file fields. *)

      val ipc_cmd : fpath -> Cmd.t
      (** [ipc_cmd file] are the command line arguments to give to
          [topkg] to run and get the fields of the OPAM file [file]
          encoded on [stdout]. *)

      val fields : fpath -> ((string * string list) list) result
      (** [fields file] are the fields of the OPAM file [file] which
          are obtained by calling [topkg] with [ipc_cmd file]. *)
    end

    (** OPAM install file.

        A module to generate OPAM install files.

        {b Reference}.
        {{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
        Syntax and semantics} of OPAM install files. *)
    module Install : sig

      (** {1 OPAM install files} *)

      type field =
      [ `Bin
      | `Doc
      | `Etc
      | `Lib
      | `Libexec
      | `Man
      | `Misc
      | `Sbin
      | `Share
      | `Share_root
      | `Stublibs
      | `Toplevel
      | `Unknown of string ]
      (** The type for OPAM install file fields. *)

      type move
      (** The type for file moves. *)

      val move : ?maybe:bool -> ?dst:fpath -> fpath -> move
      (** [move ~maybe ~dst src] moves [src] to [dst], where [dst] is
          a path relative to the directory corresponding to the
          {{!field}field}.  If [maybe] is [true] (defaults to
          [false]), then [src] may not exist, otherwise an install
          error will occur if the file doesn't exist. *)

      type t = [ `Header of string option ] * (field * move) list
      (** The type for opam install files. An optional starting header
          comment and a list of field moves. *)

      val to_string : t -> string
      (** [to_string t] is [t] as syntactically valid OPAM install file. *)
    end
  end
end

(** {1:manual Manual}

{!Topkg} is a package builder for distributing OCaml software. It
provides a mechanism to describe an opam
{{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}[.install]
file} according to the build environment and make corresponding
invocations in your build system. This simple mechanism brings the
following advantages:
{ol
{- It frees you from implementing an install procedure in your build
   system: this task is delegated to {{:http://opam.ocaml.org}OPAM} or
   to the [opam-installer] tool.}
{- It doesn't reclaim control over your build system. It will only
   invoke it {e once} with a list of targets determined from a package
   description and a build environment explicitely specified on the
   command line.}}

{!Topkg} has been designed with [ocamlbuild] in mind but it should be
usable with any other build system as long as it is able to understand
the targets topkg {{!map}generates}.

{!Topkg} is a single ISC licensed OCaml library without dependencies
that you add to you project build dependencies. It also provides the
[topkg] command line tool that will help you to streamline your
release process.

{1:setup Basic setup}

Your repository and release archive should have the following files.
{ul
{- [pkg/META], a
    {{:http://projects.camlcity.org/projects/findlib.html}Findlib}
    [META] file. That you should install in the [lib] directory
    of the package (see below).}
{- [pkg/pkg.ml], the package description written with {!Topkg}.
   See {{!descr}package description.}}
{- [_tags] ocamlbuild file with at least the [true : bin_annot] line.
   See {{!cmt}handling cmt and cmti} files for details.}
{- [opam], your package metadata, its dependencies and the instructions
   to build it. The [depends:] fields must have a ["topkg" {build}] line.}}

{b Note.} If you start a new library,
{{:http://erratique.ch/software/carcass}[carcass]} allows
you to:
{[
carcass body topkg/pkg mypkg
(cd mypkg && git init && git add . && git commit -m "First commit.")
opam pin add -kgit mypkg mypkg#master
]}
to make your library [{opam,ocamlfind}]-able with correct version
watermarks on releases and opam pins.

{1:build Package build instructions}

The build instructions of your package are simply an invocation of
[pkg/pkg.ml] with the [build] argument and a specification of the
build environment on the command line. Here is for example how you
should write that invocation in the [build:] field of the [opam] file
of a hypothetical [mypkg] package:
{v
build: [[
  "ocaml" "pkg/pkg.ml" "build"
          "human" "false" ]]
v}

This invocation will execute your build system with a set of targets
determined from the build environment specified on the command line
and generate, in the root directory of your distribution a
[mypkg.install] file that OPAM will use to install and uninstall your
package.

Note that there is no need to specify anything in the [remove:] field
of the [opam] file and there is no need to invoke [ocamlfind] with
your [META] file, simply install the latter in the [lib] section (see below).

If you need to support another package system you can invoke
[pkg/pkg.ml] as above and then manage the installation and
uninstallation at a given [$DESTDIR] using the generated
[mypkg.install] file and the [opam-installer] tool distributed with
OPAM or you own [.install] file interpreter.

{1:descr Package description}

An OPAM [.install] file is a description of a standard UNIX
install. It has fields for each of the standard directories [lib],
[bin], [man], [etc], etc. Each of these fields lists the files to
install in the corresponding directory (or subdirectories). See the
{{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}[.install]
file specification} in the OPAM developer manual for more information.

In {!Topkg}, the package description file [pkg/pkg.ml] is simply:
{ol
{- A {{!section:Pkg.builder}builder specification}. It specifies the
   build tool that is invoked once with the files to build and install
   as determined by the current build environment.}
{- A specification of the files to install according to the build
   environment by specifying invoking install field functions.

one wants build to put in each field
   of the package's [.install] file by invoking install field
   functions [Pkg.{lib,bin,doc,...}]}}

Here is an example of a [pkg/build.ml] description for a single module
library that also installs a command line tool called [jsontrip]. Don't
worry about the sheer length of the description we'll soon cut down
on the number of lines.
{[
#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "jsonm" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib "src/jsonm.mli";
    Pkg.lib "src/jsonm.cmti";
    Pkg.lib "src/jsonm.cmi";
    Pkg.lib "src/jsonm.cma";
    Pkg.lib "src/jsonm.cmx";
    Pkg.lib "src/jsonm.a";
    Pkg.lib "src/jsonm.cmxa";
    Pkg.lib "src/jsonm.cmxs";
    Pkg.bin "test/jsontrip.byte";
    Pkg.bin "test/jsontrip.native";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
]}

This says that we are declaring a package named [jsonm] which means
that the generated install file will be [jsonm.install]. It also says
that we are using [ocamlbuild] as a build tool and that we want all the
files specified with [Pkg.lib] to be installed the [lib] directory,
those with [Pkg.bin] in [bin], those with [Pkg.doc] in [doc] etc.

{b Tip.} If you are using
{{:https://github.com/the-lambda-church/merlin}[merlin]} and [emacs],
issue a [M-x merlin-use topkg], to get merlin support for editing
the package description.

Now there are two things that are unsatisfactory with the above
declaration.

{ol
{- The set of files to generate for a library is usually always the
   same and it's painful to write them down explicitely. This
   is solved by [extension sets](#extension-sets).}
{- For the binaries, we usually don't want to install both byte and
   native code. We want to install one tool without the [byte] or
   [native] suffix, and the native one if available. This is solved
   by [auto binaries](#auto-binaries)}}

Using these features the above declaration can be reduced to:
{[
let () =
  Pkg.describe "jsonm" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/jsonm";
    Pkg.bin ~auto:true "test/jsontrip";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
]}
Optional builds and installs are handled by declaring boolean keys
specified on the command line (see [Environment](#environment)) and
using the [cond] optional argument of install field functions (see
[Conditions](#conditions)). The following example compiles the
[vgr_pdf] library only if both Uutf and Otfm are present and
[vgr_htmlc] only if js_of_ocaml is present:

{[
let uutf = Env.bool "uutf"
let otfm = Env.bool "otfm"
let jsoo = Env.bool "jsoo"
let vgr_pdf = uutf && otfm
let () =
  Pkg.describe "vg" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/vg";
    Pkg.lib ~exts:Exts.module_library "src/vgr_svg";
    Pkg.lib ~cond:vgr_pdf ~exts:Exts.module_library "src/vgr_pdf";
    Pkg.bin ~cond:vgr_pdf ~auto:true "test/vecho";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library "src/vgr_htmlc";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/min_htmlc.html";
    Pkg.doc "test/min_htmlc.ml";
    Pkg.doc "test/min_pdf.ml";
    Pkg.doc "test/min_svg.ml"; ]
]}

{1:exts Extension sets}

The install field functions have an optional [exts] argument. If
present these extensions are appended to the path given to the
function. The module [[Exts]](pkg/topkg.ml#L23) defines a few
predefined extension sets. For example a module library implemented in
[src/mylib.ml] can be declared by:
{[
Pkg.lib ~exts:Exts.module_library "src/mylib"
]}
which is, effectively, a shortcut for:
{[
Pkg.lib "src/mylib.mli";
Pkg.lib "src/mylib.cmti";
Pkg.lib "src/mylib.cmi";
Pkg.lib "src/mylib.cmx";
Pkg.lib "src/mylib.cma";
Pkg.lib "src/mylib.a";
Pkg.lib "src/mylib.cmxa";
Pkg.lib "src/mylib.cmxs";
]}

{1:autobin Auto binaries}

For generating an installing native binaries if native code
compilation is available and byte code binaries if not you can use the
[auto] optional argument of [Pkg.bin] and [Pkg.sbin]. Using it with
[true] you can simply specify the binary name prefix. It will use the
base name as the name of the tool and ask for either a [.native] or
[.byte] target depending if native compilation is available or not.

{[
Pkg.bin ~auto:true "src/mybinary"
]}

{1:conds Conditions}

Conditional installation is handled through the optional argument
[cond] of install field functions. If [cond] is [false] it's neither
built nor installed. For example for a library that depends on the
presence of another:

{[
let otherlib = Env.bool "otherlib"
...
Pkg.lib ~cond:otherlib ~exts:Exts.module_library "src/mylib"
]}

Conditions related to native code and native code dynamic linking
availability happen automatically:

* In [Pkg.lib] paths ending with [.cmxs] are dropped if
  [Env.native_dynlink] is [false] and paths ending with
  [.a], [.cmx], [.cmxa] and [.cmxs] are dropped if
  [Env.native] is [false].

* In [Pkg.{bin,sbin}] path ending with [.native] are dropped
  if [Env.native] is false.

{1:env Environment}

New boolean keys are added to the environment by calling [Env.bool
key]. To output a sample environment you can invoke the build script with
[--help]:

    ocaml pkg/pkg.ml --help

{1:rename Renaming and installing in subdirectories}

By default install field functions use the basename of the path given
to the function as the install name. If you need to rename the build
artefact or install to a subdirectory you can use the [dst]
optional argument of install field functions. For example for a
library that needs to be installed in a [subdir] subdirectory of
[lib] use:

{[
Pkg.lib ~exts:Exts.module_library ~dst:"subdir/mylib" "src/mylib"
]}

{1:cmt Handling cmt and cmti files}

Since the OCaml tools generate [.cmt] and [.cmti] files only as a side
effect they are treated specially: they are not built. For
[ocamlbuild] you should add this line to your [_tags] file:
{[
true : bin_annot
]}
this will build them as a side effect of other build invocations. In
the [$PKG.install] file generated by topkg the [cmt] and [cmti] files
are prefixed by a ? so that if they are not built (pre OCaml 4.01 for
[ocamlbuild]) the install doesn't fail.

{1:verify Verifying the build invocation and .install generation}

For verifying the build invocation and the generated [.install] file
you can invoke [build.ml] with [explain] as the first argument:

    ocaml pkg/pkg.ml explain human false ...

This will not invoke the build system but output on stdout diagnostic
information aswell as the build invocation and the generated
[$PKG.install] file.

In contrast to the build invocation it is allowed to invoke [explain]
with an empty or partial environment.

    ocaml pkg/pkg.ml explain

In that case it will use [true] for each unspecified environment
boolean key.

{1:map Map from descriptions to targets and build artefacts}

Given an invocation [Pkg.{lib,bin,...} "$PATH"], the system generates
a target [$PATH] for your build system and expects to find the build
artefact in [$BUILD/$PATH] where [$BUILD] is the build directory of
the build tool (e.g. [_build] for [ocamlbuild]).

{1:config Storing build environment information in a build}

The following sample setup shows how to store build environment
information in a build. The setup also works seamlessly during
development if build artefacts are invoked from the root directory of
the source repository.

In this example we store the location of the install's [etc] directory
in the build artefacts. We have the following file layout in our
source repository:
{v
etc/mypkg.conf       # Configuration file
src/mypkg_etc.ml     # Module with path to the etc dir
v}

the contents of [src/mypkg_etc.ml] is simply:
{[
(* This file is overwritten by release builds. During development it
   refers to the [etc] directory at the root directory of the
   source repository. *)

let dir = "etc"
]}
the value [Mypkg_etc.dir] is used in the sources to refer to the [etc]
directory of the install. In the package description file
[pkg/pkg.ml] we have the following lines:
{[
let etc_dir = Env.string "etc-dir"
let etc_config () = match Env.build with
| `Dev -> Ok () (* Do nothing, the repo src/mypkg_etc.ml will do *)
| `Pin | `Release ->
    let config = strf "let dir = %S" etc_dir in
    OS.File.write "src/mypkg_etc.ml" config

let () =
  let builder = Pkg.builder ~pre:etc_config (`OCamlbuild []) in
  Pkg.describe ~builder [
  ...
  Pkg.etc "etc/mpypkg.conf";
  ... ]
]}
and the [opam] build instructions are:
{v
build:
[[ "ocaml" "pkg/pkg.ml" "build"
           "human" "false"
           "etc-dir" "%{mypkg:etc}%" ]]
v}

{1:trouble Troubleshooting}

More information run with TOPKG_VERBOSITY=debug, if you are troubleshooting
a pin build dont forget to add [--verbose] to [opam] itself.

*)

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

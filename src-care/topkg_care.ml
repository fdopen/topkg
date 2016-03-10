(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

module Opam = struct

  module File = struct

    (* Try to compose with the OpamFile.OPAM API *)

    let id x = x
    let list f = fun v -> [f v]
    let field name field conv =
      name, fun acc o -> String.Map.add name (conv (field o)) acc

    let opt_field name field conv =
      name, fun acc o -> match field o with
      | None -> acc
      | Some v -> String.Map.add name (conv v) acc

    let deps_conv d =
      let add_pkg acc (n, _) = OpamPackage.Name.to_string n :: acc in
      OpamFormula.fold_left add_pkg [] d

    let fields = [
      opt_field "name" OpamFile.OPAM.name_opt (list OpamPackage.Name.to_string);
      opt_field "version" OpamFile.OPAM.version_opt
        (list OpamPackage.Version.to_string);
      field "opam-version" OpamFile.OPAM.opam_version
        (list OpamVersion.to_string);
      field "available" OpamFile.OPAM.available (list OpamFilter.to_string);
      field "maintainer" OpamFile.OPAM.maintainer id;
      field "homepage" OpamFile.OPAM.homepage id;
      field "authors" OpamFile.OPAM.author id;
      field "license" OpamFile.OPAM.license id;
      field "doc" OpamFile.OPAM.doc id;
      field "tags" OpamFile.OPAM.tags id;
      field "bug-reports" OpamFile.OPAM.bug_reports id;
      opt_field "dev-repo"
        OpamFile.OPAM.dev_repo (list OpamTypesBase.string_of_pin_option);
      field "depends" OpamFile.OPAM.depends deps_conv;
      field "depopts" OpamFile.OPAM.depopts deps_conv;
    ]

    let field_names =
      let add acc (name, field) = String.Set.add name acc in
      List.fold_left add String.Set.empty fields

    let fields file =
      let parse file  =
        let file = OpamFilename.of_string (Fpath.to_string file) in
        let opam = OpamFile.OPAM.read file in
        let known_fields =
          let add_field acc (_, field) = field acc opam in
          List.fold_left add_field String.Map.empty fields
        in
        (* TODO add OpamFile.OPAM.extensions when supported *)
        known_fields
      in
      try Ok (parse file) with
      | exn ->
          (* Apparently in at least opam-lib 1.2.2, the error will be logged
             on stdout. *)
          R.error_msgf "%a: could not parse OPAM file" Fpath.pp file

    let deps ?(opts = true) fields =
      let deps = match String.Map.find "depends" fields with
      | None -> [] | Some deps -> deps
      in
      let dep_opts =
        if not opts then [] else
        match String.Map.find "depopts" fields with
        | None -> []  | Some deps -> deps
      in
      String.Set.of_list (List.rev_append dep_opts deps)
  end
end

module OCamlbuild = struct

   let find_next_package_id =
     let package = String.sub "package(" in
     let not_rpar c = not (Char.equal ')' c) in
     let not_dot c = not (Char.equal '.' c) in
     fun ~root s -> match String.Sub.find_sub ~sub:package s with
     | None -> None
     | Some s ->
         let rest = String.Sub.(extend (stop s)) in
         let id, rest = String.Sub.span ~sat:not_rpar rest in
         let id = if root then String.Sub.take ~sat:not_dot id else id in
         Some (String.Sub.to_string id, String.Sub.tail rest)

   let find_packages ~roots s =
     let rec loop acc s = match find_next_package_id ~root:roots s with
     | None -> acc
     | Some (id, rest) -> loop (String.Set.add id acc) rest
     in
     loop String.Set.empty (String.sub s)

   let package_tags ?(roots = false) file =
     OS.File.read file >>= fun contents -> Ok (find_packages ~roots contents)
end

module OCamlfind = struct

   let base_root_packages = String.Set.of_list
       [ "bigarray"; "bytes"; "compiler-libs"; "dynlink"; "graphics"; "num";
         "ocamldoc"; "stdlib"; "str"; "threads"; "unix" ]
end

module Lint = struct

  (* Dependencies *)

  let deps ?(exclude = []) ~opam ~tags =
    begin
      OCamlbuild.package_tags ~roots:true tags
      >>= fun tags -> Opam.File.fields opam
      >>| fun fields ->
      let exclude = String.Set.of_list exclude in
      let exclude = String.Set.union exclude OCamlfind.base_root_packages in
      let keep id =
        not (String.Set.mem id exclude) &&
        not (String.is_prefix "conf-" id)
      in
      let opam = String.Set.filter keep (Opam.File.deps ~opts:true fields) in
      let tags = String.Set.filter keep tags in
      let opam_only = String.Set.diff opam tags in
      let tags_only = String.Set.diff tags opam in
      if String.Set.is_empty opam_only && String.Set.is_empty tags_only
      then None
      else Some (opam_only, tags_only)
    end
    |> R.reword_error_msg ~replace:true
      (fun msg -> R.msgf "could not lint dependencies: %s" msg)

  let pp_deps_mismatches ppf (opam_only, tags_only) =
    let pp_miss present absent ppf id =
      Fmt.pf ppf "@[%a: %a present but %a absent@]"
        Fmt.(styled `Bold string) id
        Fmt.(styled `Green string) present
        Fmt.(styled `Red string) absent
    in
    let sep =
      if String.Set.(is_empty opam_only || is_empty tags_only)
      then Fmt.nop else Fmt.cut
    in
    Fmt.pf ppf "@[<v>%a%a%a@]"
      (String.Set.pp (pp_miss "opam" "_tags")) opam_only
      sep ()
      (String.Set.pp (pp_miss "_tags" "opam")) tags_only

  (* File existence *)

  type file = string * Fpath.set

  let files =
    let files l = Fpath.Set.of_list (List.map Fpath.v l) in
    let text_files l =
      let add acc l =
        let base = Fpath.v l in
        acc
        |> Fpath.Set.add base
        |> Fpath.Set.add (Fpath.add_ext ".md" base)
        |> Fpath.Set.add (Fpath.add_ext ".adoc" base)
        |> Fpath.Set.add (Fpath.add_ext ".txt" base)
      in
      List.fold_left add Fpath.Set.empty l
    in
    [ "package description", files ["pkg/pkg.ml"];
      "ocamlfind META", files ["pkg/META"];
      "OPAM", files ["opam"];
      "README", text_files ["README"; "Readme"; "ReadMe"];
      "LICENSE", text_files ["LICENSE"; "License"];
      "CHANGES",
      text_files ["CHANGES"; "Changes"; "CHANGELOG"; "Changelog"; "ChangeLog"]]

  let files_exist ?(err = fun f err -> ()) ~dir fs = []

  let pp_file_miss ppf ((name, stds), exists) =
    if exists then () else
    match Fpath.Set.cardinal stds with
    | 0 -> assert false
    | 1 ->
        Fmt.pf ppf
          "@[Missing@ %s@ file. File@ %a@ was@ not@ found.@]"
          name Fpath.pp (Fpath.Set.get_any_elt stds)
    | n ->
        Fmt.pf ppf
          "@[Missing@ %s@ file. None@ of@ these@ files@ was@ found:@ %a@]"
          name Fpath.Set.(pp ~sep:Fmt.(unit ",@ ") Fpath.pp) stds
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

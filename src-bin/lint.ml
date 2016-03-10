(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let result_header errs = match errs with
| 0 -> Fmt.(strf_like stdout "%a" (styled_unit `Green "OK") ())
| n -> Fmt.(strf_like stdout "%a" (styled_unit `Red "FAIL") ())

let check_packages pkg_file = ()
(*
  let opam = Fpath.v "opam" in
  let tags = Fpath.v "_tags" in
  let exclude =
    begin
      OS.Cmd.(run_out Cmd.(v "ocaml" % p pkg_file % "lint" % "config")
             |> to_string)
      >>= fun c -> Topkg.Pkg.dec_lint_config c
      >>| fun c -> Topkg.Pkg.lint_deps_excluding c
    end
    |> Logs.on_error_msg ~level:Logs.Warning ~use:(fun () -> Some [])
  in
  match exclude with
  | None ->
      Logs.info (fun m -> m "deps linting disabled by package")
  | Some exclude ->
      begin Topkg_care.Lint.deps ~exclude ~opam ~tags >>| function
        | None -> ()
        | Some miss ->
            Logs.err
              (fun m -> m "@[<v>Dependencies mismatches:@,%a@]"
                  Topkg_care.Lint.pp_deps_mismatches miss);
      end
      |> Logs.on_error_msg ~use:(fun () -> ())
*)

let check_custom pkg_file = ()
(*
  begin
    OS.Cmd.(run_out Cmd.(v "ocaml" % p pkg_file % "lint" % "custom")
            |> to_string)
    >>= fun c -> Ok (Topkg.Pkg.dec_lint_custom c)
    >>= function
    | [] -> Ok ()
    | msgs ->
        List.iter (fun msg -> Logs.err (fun m -> m "%a" Fmt.text msg)) msgs;
        Ok ()
  end
  |> Logs.on_error_msg ~use:(fun () -> ())
*)

let has_std_files ~errs pkg_files =
  0
(*
  let has_std_file (name, stds) fs = Fpath.Set.(not (is_empty (inter stds fs)))
  in
  let has_std_file errs std =
    if Topkg_care.Lint.(has_std_file std pkg_files) then errs else
    (Logs.err (fun m -> m "%a" Topkg_care.Lint.pp_std_file_miss std); errs + 1)
  in
  List.fold_left has_std_file 0 Topkg_care.Lint.std_files
*)

let lint pkg_file ignore_pkg tests =
  begin
    OS.Dir.current ()
    >>= fun dir -> OS.Dir.contents ~rel:true dir
    >>= fun pkg_files ->
    let pkg_files = Fpath.Set.of_list pkg_files in
    let errs = has_std_files ~errs:0 pkg_files in
    let () = check_packages pkg_file in
    let () = check_custom pkg_file in
    let header = result_header errs in
    Logs.app (fun m -> m ~header "topkg lint %a" Fpath.pp dir);
    Ok errs
  end
  |> Logs.on_error_msg ~use:(fun _ -> 1)

(* Command line interface *)

open Cmdliner

let test =
  let test = ["deps", `Deps; "files", `Files; "custom", `Custom ] in
  let doc = strf "Test to perform. $(docv) must be one of %s. If unspecified
                  all tests are performed." (Arg.doc_alts_enum test)
  in
  let test = Arg.enum test in
  Arg.(value & pos_all test [`All] & info [] ~doc ~docv:"TEST")

let doc = "checks package distribution consistency and conventions"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command checks that the OPAM dependencies
        of a distribution are consistent with those of its build system
        and that a few standard files are part of it.
        Linting is automatically performed on distribution generation,
        see topkg-distrib(2) for more details.";
    `P "Linting can be invoked on the source repository during development.
        In particular invoking:";
    `Pre "  > $(mname) lint deps";
    `P "may report out of sync build system and OPAM dependencies. Note
        however that during development other linting tests
        may fail due the different file layout between the repository
        and a distribution.";

  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:["topkg-distrib"]

let cmd =
  let info = Term.info "lint" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure lint $ Cli.setup $ Cli.ignore_pkg $ test) in
  (t, info)

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

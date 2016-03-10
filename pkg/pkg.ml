#!/usr/bin/env ocaml

(* Bootstrap from source, note #mod_use is 4.01 *)

#use "topfind"
#require "result"
#directory "src"
#mod_use "topkg_result.ml"
#mod_use "topkg_string.ml"
#mod_use "topkg_log.ml"
#mod_use "topkg_fpath.ml"
#mod_use "topkg_cmd.ml"
#mod_use "topkg_os.ml"
#mod_use "topkg_vcs.ml"
#mod_use "topkg_conf.ml"
#mod_use "topkg_conf_ocaml.ml"
#mod_use "topkg_fexts.ml"
#mod_use "topkg_codec.ml"
#mod_use "topkg_opam.ml"
#mod_use "topkg_install.ml"
#mod_use "topkg_lint.ml"
#mod_use "topkg_build.ml"
#mod_use "topkg_distrib.ml"
#mod_use "topkg.ml"

open Topkg

let () =
  let custom () = [Error (`Msg "bla")] in
  let lint = Pkg.lint ~deps_excluding:(Some ["ocamlbuild"]) ~custom () in
  let pkg_name = Env.string "pkg-name" in
  let care = pkg_name = "topkg-care" in
  let builder = Pkg.builder (`OCamlbuild []) in
  Pkg.describe pkg_name ~lint ~builder [
      Pkg.lib ~cond:(not care) "pkg/META";
      Pkg.lib ~cond:(not care) ~exts:Exts.module_library "src/topkg";
      Pkg.lib ~cond:care "pkg/META.care" ~dst:"META";
      Pkg.lib ~cond:care ~exts:Exts.module_library "src-care/topkg_care";
      Pkg.bin ~cond:care ~auto:true "src-bin/topkg_bin" ~dst:"topkg";
      Pkg.bin ~cond:care "support/topkg-reload-browser";
      Pkg.doc "README.md";
      Pkg.doc "CHANGES.md"; ]

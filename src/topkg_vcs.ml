(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

module Cmd = Topkg_cmd
module OS = Topkg_os

(* Version control system repsitories *)

type kind = [ `Git | `Hg ]

let pp_kind ppf = function
| `Git -> Format.pp_print_string ppf "git"
| `Hg -> Format.pp_print_string ppf "hg"

let dirtify id = id ^ "-dirty"

type t = kind * Cmd.t

let git = Cmd.v (OS.Env.opt_var "TOPKG_GIT" ~absent:"git")
let hg = Cmd.v (OS.Env.opt_var "TOPKG_HG" ~absent:"hg")

let vcs_cmd kind dir = match kind, dir with
| (`Git, dir) -> Cmd.(git % "--git-dir" % dir)
| (`Hg, dir) -> Cmd.(hg % "--repository" % dir)

let v k ~dir = (k, vcs_cmd k dir)
let kind r = fst r
let cmd r = snd r
let dir r = List.hd (snd r)

(* Git support *)

let find_git () = OS.Cmd.exists git >>= function
| false -> Ok None
| true ->
    let git_dir = Cmd.(git % "rev-parse" % "--git-dir") in
    OS.Cmd.(run_out ~err:OS.File.null git_dir |> out_string) >>= function
    | (dir, (_, `Exited 0)) -> Ok (Some (v `Git dir))
    | _ -> Ok None

let err_git r c =
  R.error_msgf "%s: not a git repository (git exited with %d)" (dir r) c

let run_git (_, git as r) args =
  let git = Cmd.(git %% args) in
  OS.Cmd.(run_out ~err:OS.File.null git |> out_string) >>= function
  | (v, (_, `Exited 0)) -> Ok v
  | (_, (_, `Exited c)) -> err_git r c

let git_is_dirty (_, git as r) =
  let diff = Cmd.(git % "diff" % "--quiet" % "HEAD") in
  OS.Cmd.(run_status ~err:OS.File.null diff) >>= function
  | `Exited 0 -> Ok false
  | `Exited 1 -> Ok true
  | `Exited c -> err_git r c

let git_head ~dirty r =
  run_git r Cmd.(v "show-ref" % "--hash" % "HEAD") >>= fun id ->
  if not dirty then Ok id else
  git_is_dirty r >>= fun is_dirty ->
  if is_dirty then Ok (dirtify id) else Ok id

let git_describe ~dirty r commitish =
  let dirty = dirty && commitish = "HEAD" in
  run_git r Cmd.(v "describe" % "--always" %% on dirty (v "--dirty") %%
                 on (not dirty) (v commitish))

let git_tracked_files r ~treeish =
  run_git r Cmd.(v "ls-tree" % "--name-only" % "-r" % treeish)
  >>= fun files -> Ok (Topkg_string.cuts ~sep:'\n' files)

(* Hg support *)

let find_hg () = OS.Cmd.exists hg >>= function
  | false -> Ok None
  | true ->
      let hg_root = Cmd.(hg % "root") in
      OS.Cmd.(run_out ~err:OS.File.null hg_root |> out_string) >>= function
      | (dir, (_, `Exited 0)) -> Ok (Some (v `Hg dir))
      | _ -> Ok None

let err_hg r c =
  R.error_msgf "%s: not an hg repository (hg exited with %d)" (dir r) c

let run_hg (_, hg as r) args =
  let hg = Cmd.(hg %% args) in
  OS.Cmd.(run_out ~err:OS.File.null hg |> out_string) >>= function
  | (v, (_, `Exited 0)) -> Ok v
  | (_, (_, `Exited c)) -> err_hg r c

let hg_id r =
  run_hg r Cmd.(v "id" % "-i") >>= fun id ->
  let len = String.length id in
  let is_dirty = String.length id > 0 && id.[len - 1] = '+' in
  let id = if is_dirty then String.sub id 0 (len - 1) else id in
  Ok (id, is_dirty)

let hg_is_dirty r = hg_id r >>= function (id, is_dirty) -> Ok is_dirty
let hg_head ~dirty (_, hg as r) =
  hg_id r >>= function (id, is_dirty) ->
    Ok (if is_dirty && dirty then dirtify id else id)

let hg_rev ~rev = if rev = "HEAD" then Cmd.empty else Cmd.(v "--rev" % rev)

let hg_describe ~dirty r ~rev =
  let get_distance s = try Ok (int_of_string s) with
  | Failure _ -> R.error_msgf "%s: could not parse hg tag distance." (dir r)
  in
  let parent t = run_hg r Cmd.(v "parent" %% hg_rev rev % "--template" % t) in
  parent "{latesttagdistance}" >>= get_distance
  >>= begin function
  | 1 -> parent "{latesttag}"
  | n -> parent "{latesttag}-{latesttagdistance}-{node|short}"
  end
  >>= fun descr ->
  if not dirty then Ok descr else
  hg_id r >>= fun (_, is_dirty) ->
  Ok (if is_dirty then dirtify descr else descr)

let hg_tracked_files r ~rev =
  run_hg r Cmd.(v "manifest" %% hg_rev ~rev)
  >>= fun files -> Ok (Topkg_string.cuts ~sep:'\n' files)

(* Generic VCS support *)

let find ?dir () = match dir with
| None ->
    begin find_git () >>= function
      | Some _ as v -> Ok v
      | None -> find_hg ()
    end
| Some dir ->
    let git_dir = Topkg_fpath.append dir ".git" in
    OS.Dir.exists git_dir >>= function
    | true -> Ok (Some (v `Git git_dir))
    | false ->
        let hg_dir = Topkg_fpath.append dir ".hg" in
         OS.Dir.exists hg_dir >>= function
         | true -> Ok (Some (v `Hg hg_dir))
         | false -> Ok None

let get ?dir () = find ?dir () >>= function
  | Some r -> Ok r
  | None ->
      let dir = match dir with None -> OS.Dir.current () | Some dir -> Ok dir in
      dir >>= function dir ->
        R.error_msgf "%s: could not find a VCS repository" dir

let pp ppf r = Format.fprintf ppf "(%a, %s)" pp_kind (kind r) (dir r)

(* Repository state *)

let is_dirty = function
| (`Git, _  as r) -> git_is_dirty r
| (`Hg, _ as r ) -> hg_is_dirty r

let not_dirty r = is_dirty r >>= function
| false -> Ok ()
| true -> R.error_msgf "The VCS repo is dirty, commit or stash your changes."

let head ?(dirty = true) = function
| (`Git, _ as r) -> git_head ~dirty r
| (`Hg, _ as r) -> hg_head ~dirty r

let describe ?(dirty = true) ?(commitish = "HEAD") = function
| (`Git, _ as r) -> git_describe ~dirty r commitish
| (`Hg, _ as r) -> hg_describe ~dirty r ~rev:commitish

let tracked_files ?(treeish = "HEAD") = function
| (`Git, _ as r) -> git_tracked_files r ~treeish
| (`Hg, _ as r) -> hg_tracked_files r ~rev:treeish

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
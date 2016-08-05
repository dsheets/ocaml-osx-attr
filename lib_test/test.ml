(*
 * Copyright (c) 2016 David Sheets <dsheets@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let (/) = Filename.concat

module Vtype : Alcotest.TESTABLE with type t = Osx_attr.Vnode.Vtype.t = struct
  type t = Osx_attr.Vnode.Vtype.t

  let pp fmt v = Format.pp_print_string fmt (Osx_attr.Vnode.Vtype.to_string v)

  let equal x y = Osx_attr.Vnode.Vtype.compare x y = 0
end

let vtype =
  (module Vtype : Alcotest.TESTABLE with type t = Osx_attr.Vnode.Vtype.t)

let vreg = Osx_attr.Vnode.Vtype.VREG
let vlnk = Osx_attr.Vnode.Vtype.VLNK
let vdir = Osx_attr.Vnode.Vtype.VDIR

module Basic = struct
  let path = "file"
  let link_path = "Link"
  let cap_link_path = String.uppercase link_path

  let prepare () =
    Unix.(close (openfile path [O_CREAT] 0o600));
    Unix.symlink path link_path

  let cleanup () =
    Unix.unlink path;
    Unix.unlink link_path

  let name () =
    let cmn_name = Osx_attr.(Select.Common Common.NAME) in
    (match Osx_attr.(get cmn_name cap_link_path) with
     | Some p -> Alcotest.(check string) "normalize cap link target" path p
     | None -> Alcotest.fail ("no CMN_NAME for "^cap_link_path)
    );
    (match Osx_attr.(get ~no_follow:true cmn_name cap_link_path) with
     | Some p -> Alcotest.(check string) "normalize cap link path" link_path p
     | None -> Alcotest.fail ("no CMN_NAME for "^cap_link_path)
    )

  let objtype () =
    let cmn_objtype = Osx_attr.(Select.Common Common.OBJTYPE) in
    (match Osx_attr.(get cmn_objtype cap_link_path) with
     | Some t -> Alcotest.check vtype "type of cap link target" vreg t
     | None -> Alcotest.fail ("no CMN_OBJTYPE for "^cap_link_path)
    );
    (match Osx_attr.(get ~no_follow:true cmn_objtype cap_link_path) with
     | Some t -> Alcotest.check vtype "type of cap link path" vlnk t
     | None -> Alcotest.fail ("no CMN_OBJTYPE for "^cap_link_path)
    )

  let name_and_objtype () =
    let cmn_name = Osx_attr.(Query.Common Common.NAME) in
    let cmn_objtype = Osx_attr.(Query.Common Common.OBJTYPE) in
    let attrs = [cmn_name; cmn_objtype] in
    let values = Osx_attr.getlist attrs cap_link_path in
    Alcotest.(check int)
      "attribute list length" (List.length attrs) (List.length values);
    List.iter Osx_attr.(function
      | Value.Common (Common.NAME, name) ->
        Alcotest.(check string) "normalize cap link target" path name
      | Value.Common (Common.OBJTYPE, t) ->
        Alcotest.(check vtype) "type of cap link target" vreg t
      | _ -> Alcotest.fail ("unexpected result attribute for "^cap_link_path)
    ) values;
    let values = Osx_attr.getlist ~no_follow:true attrs cap_link_path in
    Alcotest.(check int)
      "attribute list length" (List.length attrs) (List.length values);
    List.iter Osx_attr.(function
      | Value.Common (Common.NAME, name) ->
        Alcotest.(check string) "normalize cap link target" link_path name
      | Value.Common (Common.OBJTYPE, t) ->
        Alcotest.(check vtype) "type of cap link target" vlnk t
      | _ -> Alcotest.fail ("unexpected result attribute for "^cap_link_path)
    ) values

  let tests = [
    "prepare", `Quick, prepare;
    "name", `Quick, name;
    "objtype", `Quick, objtype;
    "name_and_objtype", `Quick, name_and_objtype;
    "cleanup", `Quick, cleanup;
  ]
end

module Bulk = struct
  let dir_path = "dir"
  let dir_contents = [
    "Link", vlnk;
    "reg",  vreg;
    "DIR",  vdir;
  ]

  let prepare () =
    Unix.mkdir dir_path 0o700;
    List.iter Osx_attr.Vnode.Vtype.(function
      | name, VLNK -> Unix.symlink "target" (dir_path / name)
      | name, VREG -> Unix.(close (openfile (dir_path / name) [O_CREAT] 0o600))
      | name, VDIR -> Unix.mkdir (dir_path / name) 0o700
      | name, typ  ->
        let msg =
          Printf.sprintf "don't know how to make %s of type %s"
            name (to_string typ)
        in
        Alcotest.fail msg
    ) dir_contents

  let cleanup () =
    let rc = Sys.command ("rm -rf "^dir_path) in
    Alcotest.(check int) "cleanup directory" 0 rc

  let readdirbulk () =
    let dirfd = Unix.openfile dir_path Unix.[O_RDONLY] 0 in
    let dir = Osx_attr.(getbulk (Select.Common Common.OBJTYPE)) dirfd in
    List.iter (function
      | (name, None) -> Alcotest.fail ("couldn't get type for "^name)
      | (name, Some typ) ->
        match List.assoc name dir_contents with
        | exception Not_found -> Alcotest.fail ("unexpected entry "^name)
        | t -> Alcotest.(check vtype) ("type of "^name) t typ
    ) dir

  let tests = [
    "prepare", `Quick, prepare;
    "readdirbulk", `Quick, readdirbulk;
    "cleanup", `Quick, cleanup;
  ]
end

let tests = [
  "Basic", Basic.tests;
  "Bulk", Bulk.tests;
]

;;
Alcotest.run "OSX attr" tests

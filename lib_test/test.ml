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

let path = "file"
let link_path = "Link"
let cap_link_path = String.uppercase link_path

module Vtype : Alcotest.TESTABLE with type t = Osx_attr.Vnode.Vtype.t = struct
  type t = Osx_attr.Vnode.Vtype.t

  let pp fmt v = Format.pp_print_string fmt (Osx_attr.Vnode.Vtype.to_string v)

  let equal x y = Osx_attr.Vnode.Vtype.compare x y = 0
end

let vtype =
  (module Vtype : Alcotest.TESTABLE with type t = Osx_attr.Vnode.Vtype.t)

let vreg = Osx_attr.Vnode.Vtype.VREG
let vlnk = Osx_attr.Vnode.Vtype.VLNK

module Basic = struct
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

let tests = [
  "Basic", Basic.tests;
]

;;
Alcotest.run "OSX attr" tests

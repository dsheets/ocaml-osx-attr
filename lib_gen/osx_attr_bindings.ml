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

open Ctypes

let (|||) = (lor)

let (??>) flag int = if flag then int else 0

let (??<) field int = field land int <> 0

module Type = Osx_attr_types.C(Osx_attr_types_detected)

module type S =
sig
  open Unsigned

  type 'a t

  val getattrlist :
    string -> Type.AttrList.t structure ptr -> unit ptr -> size_t -> ulong -> (int * Signed.sint) t

  val fgetattrlist :
    int -> Type.AttrList.t structure ptr -> unit ptr -> size_t -> ulong -> (int * Signed.sint) t

  val getattrlistat :
    int -> string -> Type.AttrList.t structure ptr -> unit ptr -> size_t -> ulong -> (int * Signed.sint) t

  val setattrlist :
    string -> Type.AttrList.t structure ptr -> unit ptr -> size_t -> ulong -> (int * Signed.sint) t

  val fsetattrlist :
    int -> Type.AttrList.t structure ptr -> unit ptr -> size_t -> ulong -> (int * Signed.sint) t
end

module C(F: Cstubs.FOREIGN) =
struct

  let getattrlist = F.(foreign "getattrlist" (
    string @-> ptr Type.AttrList.t @-> ptr void @-> size_t @-> ulong @->
    returning int
  ))

  let fgetattrlist = F.(foreign "fgetattrlist" (
    int @-> ptr Type.AttrList.t @-> ptr void @-> size_t @-> ulong @->
    returning int
  ))

  let getattrlistat = F.(foreign "getattrlistat" (
    int @-> string @-> ptr Type.AttrList.t @-> ptr void @-> size_t @->
    ulong @-> returning int
  ))

  let setattrlist = F.(foreign "setattrlist" (
    string @-> ptr Type.AttrList.t @-> ptr void @-> size_t @-> ulong @->
    returning int
  ))

  let fsetattrlist = F.(foreign "fsetattrlist" (
    int @-> ptr Type.AttrList.t @-> ptr void @-> size_t @-> ulong @->
    returning int
  ))
end

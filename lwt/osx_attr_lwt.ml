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

module Gen = Osx_attr_lwt_generated
module G = Osx_attr_bindings.C(Gen)
module C =
struct
  type 'a t = 'a Lwt.t

  let getattrlist s a p l u = G.(getattrlist s a p l u).Gen.lwt
  let fgetattrlist x a p l u = G.(fgetattrlist x a p l u).Gen.lwt
  let getattrlistat x y a p l u = G.(getattrlistat x y a p l u).Gen.lwt
  let setattrlist s a p l u = G.(setattrlist s a p l u).Gen.lwt
  let fsetattrlist s a p l u = G.(fsetattrlist s a p l u).Gen.lwt
end
type 'a t = 'a Lwt.t
include Osx_attr.Make(Lwt)(C)

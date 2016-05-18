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

module Common : sig
  type _ t =
    | NAME : string t
    | CRTIME : Time.Timespec.t t
    | MODTIME : Time.Timespec.t t
    | CHGTIME : Time.Timespec.t t
    | ACCTIME : Time.Timespec.t t
    | BKUPTIME : Time.Timespec.t t
    | FNDRINFO : string t
    | ADDEDTIME : Time.Timespec.t t
end

module Volume : sig
  type _ t =
    | FSTYPE : int32 t
    | SIGNATURE : int32 t
    | IOBLOCKSIZE : int32 t
    | OBJCOUNT : int32 t
    | FILECOUNT : int32 t
    | DIRCOUNT : int32 t
    | MAXOBJCOUNT : int32 t
    | MOUNTPOINT : string t
    | NAME : string t
    | MOUNTFLAGS : int32 t
    | MOUNTEDDEVICE : string t
end

module Directory : sig
  type _ t =
    | LINKCOUNT : int32 t
    | ENTRYCOUNT : int32 t
    | MOUNTSTATUS : int32 t
end

module File : sig
  type _ t =
    | LINKCOUNT : int32 t
    | TOTALSIZE : int64 t
    | ALLOCSIZE : int64 t
    | IOBLOCKSIZE : int32 t
    | CLUMPSIZE : int32 t
    | DEVTYPE : int32 t
    | FORKCOUNT : int32 t
end

module Query : sig
  type t =
    | Common : 'a Common.t -> t
    | Volume : 'b Volume.t -> t
    | Directory : 'c Directory.t -> t
    | File : 'd File.t -> t
end

module Select : sig
  type 'a t =
    | Common of 'a Common.t
    | Volume of 'a Volume.t
    | Directory of 'a Directory.t
    | File of 'a File.t
end

module Value : sig
  type t =
    | Common : 'a Common.t * 'a -> t
    | Volume : 'a Volume.t * 'a -> t
    | Directory : 'a Directory.t * 'a -> t
    | File : 'a File.t * 'a -> t
end

val getlist :
  ?no_follow:bool -> ?size:int -> Query.t list -> string -> Value.t list

val fgetlist :
  ?no_follow:bool -> ?size:int -> Query.t list -> Unix.file_descr ->
  Value.t list

val getlistat :
  ?no_follow:bool -> ?size:int -> Query.t list -> Unix.file_descr -> string ->
  Value.t list

val get : ?no_follow:bool -> ?size:int -> 'a Select.t -> string -> 'a option

val fget :
  ?no_follow:bool -> ?size:int -> 'a Select.t -> Unix.file_descr -> 'a option

val getat :
  ?no_follow:bool -> ?size:int -> 'a Select.t -> Unix.file_descr -> string ->
  'a option

val setlist : ?no_follow:bool -> Value.t list -> string -> unit

val fsetlist : ?no_follow:bool -> Value.t list -> Unix.file_descr -> unit

val set : ?no_follow:bool -> Value.t -> string -> unit

val fset : ?no_follow:bool -> Value.t -> Unix.file_descr -> unit

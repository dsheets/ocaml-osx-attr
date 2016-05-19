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

module Types = Osx_attr_types.C(Osx_attr_types_detected)
module C = Osx_attr_bindings.C(Osx_attr_generated)

let int_of_fd = Unix_representations.int_of_file_descr

let uint32_of_int = Unsigned.UInt32.of_int

let read_string data_p =
  let attrref_p = from_voidp Types.AttrReference.t data_p in
  let attrref = !@ attrref_p in
  let offset = getf attrref Types.AttrReference.dataoffset in
  let length = getf attrref Types.AttrReference.length in
  let length = Unsigned.UInt32.to_int length - 1 in
  let p = (from_voidp char data_p) +@ (Int32.to_int offset) in
  string_from_ptr p ~length, to_voidp (attrref_p +@ 1)

let read_timespec data_p =
  let timespec_p = from_voidp Time_unix.Timespec.t data_p in
  let timespec = !@ timespec_p in
  timespec, to_voidp (timespec_p +@ 1)

(* ASSUMES 4 BYTE ALIGNED (k = 4 * j where j is in N) *)
let read_bytes k data_p =
  let bytes_p = from_voidp char data_p in
  string_from_ptr bytes_p ~length:k, to_voidp (bytes_p +@ k)

let read_uint32 data_p =
  let uint32_p = from_voidp uint32_t data_p in
  Unsigned.UInt32.to_int32 (!@ uint32_p), to_voidp (uint32_p +@ 1)

let read_off data_p =
  let off_p = from_voidp PosixTypes.off_t data_p in
  PosixTypes.Off.to_int64 (!@ off_p), to_voidp (off_p +@ 1)

let size_string_trailer s =
  let cstrlen = String.length s + 1 in
  let up = 4 - (cstrlen mod 4) in
  if up = 4 then cstrlen else (cstrlen + up)  

let size_string s = sizeof Types.AttrReference.t, size_string_trailer s

let write_string (data_p, trailer_p) s =
  let attr_ref_p = from_voidp Types.AttrReference.t data_p in
  let attr_ref = !@ attr_ref_p in
  let offset = ptr_diff (from_voidp char data_p) trailer_p in
  let cstrlen = String.length s + 1 in
  setf attr_ref Types.AttrReference.dataoffset (Int32.of_int offset);
  setf attr_ref Types.AttrReference.length (uint32_of_int cstrlen);
  (coerce (ptr char) (ptr string) trailer_p) <-@ s;
  (to_voidp (attr_ref_p +@ 1), (trailer_p +@ size_string_trailer s))

let write_timespec (data_p, trailer_p) t =
  let timespec_p = from_voidp Time_unix.Timespec.t data_p in
  timespec_p <-@ t;
  (to_voidp (timespec_p +@ 1), trailer_p)

let write_bytes k (data_p, trailer_p) bytes =
  let len = String.length bytes in
  let p = from_voidp (array len char) data_p in
  let arr = CArray.from_ptr (coerce string (ptr char) bytes) len in
  p <-@ arr;
  (to_voidp (p +@ 1), trailer_p)

let write_uint32 (data_p, trailer_p) i =
  let p = from_voidp int32_t data_p in
  p <-@ i;
  (to_voidp (p +@ 1), trailer_p)

let write_off (data_p, trailer_p) off =
  let p = from_voidp PosixTypes.off_t data_p in
  p <-@ (PosixTypes.Off.of_int64 off);
  (to_voidp (p +@ 1), trailer_p)

module Common = struct
  type _ t =
    | NAME : string t
(*    | DEVID : dev_t t
    | FSID : fsid_t t
    | OBJTYPE : fsobj_type_t t
    | OBJTAG : fsobj_tag_t t
    | OBJID : fsobj_id_t t
    | OBJPERMANENTID : fsobj_id_t t
    | PAROBJID : fsobj_id_t t
      | SCRIPT : text_encoding_t t *)
    | CRTIME : Time.Timespec.t t
    | MODTIME : Time.Timespec.t t
    | CHGTIME : Time.Timespec.t t
    | ACCTIME : Time.Timespec.t t
    | BKUPTIME : Time.Timespec.t t
    | FNDRINFO : string t
(*    | OWNERID : uid_t t
    | GRPID : gid_t t
    | ACCESSMASK : int32 t
    | FLAGS : int32 t
    | GEN_COUNT : int32 t
    | DOCUMENT_ID : int32 t
    | USERACCESS : int32 t
    | EXTENDED_SECURITY : ?? t
    | UUID
    | GRPUUID
    | FILEID
    | PARENTID
      | FULLPATH *)
    | ADDEDTIME : Time.Timespec.t t
      (*| DATA_PROTECT_FLAGS : int32 t*)

  let to_group (type a) : a t -> Unsigned.UInt32.t =
    Types.Attributes.Common.(function
      | NAME -> name
      | CRTIME -> crtime
      | MODTIME -> modtime
      | CHGTIME -> chgtime
      | ACCTIME -> acctime
      | BKUPTIME -> bkuptime
      | FNDRINFO -> fndrinfo
      | ADDEDTIME -> addedtime
    )

  let compare (type a) (type b) (x : a t) (y : b t) = match x, y with
    | NAME, NAME -> 0
    | NAME, _ -> -1
    | _, NAME -> 1
    | CRTIME, CRTIME -> 0
    | CRTIME, _ -> -1
    | _, CRTIME -> 1
    | MODTIME, MODTIME -> 0
    | MODTIME, _ -> -1
    | _, MODTIME -> 1
    | CHGTIME, CHGTIME -> 0
    | CHGTIME, _ -> -1
    | _, CHGTIME -> 1
    | ACCTIME, ACCTIME -> 0
    | ACCTIME, _ -> -1
    | _, ACCTIME -> 1
    | BKUPTIME, BKUPTIME -> 0
    | BKUPTIME, _ -> -1
    | _, BKUPTIME -> 1
    | FNDRINFO, FNDRINFO -> 0
    | FNDRINFO, _ -> -1
    | _, FNDRINFO -> 1
    | ADDEDTIME, ADDEDTIME -> 0

  let unpack (type a) : a t -> unit ptr -> a * unit ptr = function
    | NAME      -> read_string
    | CRTIME    -> read_timespec
    | MODTIME   -> read_timespec
    | CHGTIME   -> read_timespec
    | ACCTIME   -> read_timespec
    | BKUPTIME  -> read_timespec
    | FNDRINFO  -> read_bytes 32
    | ADDEDTIME -> read_timespec

  let same (type a) (type b) (v : a) (x : b t) (y : a t) : b option =
    match x, y with
    | NAME,      NAME      -> Some v
    | CRTIME,    CRTIME    -> Some v
    | MODTIME,   MODTIME   -> Some v
    | CHGTIME,   CHGTIME   -> Some v
    | ACCTIME,   ACCTIME   -> Some v
    | BKUPTIME,  BKUPTIME  -> Some v
    | FNDRINFO,  FNDRINFO  -> Some v
    | ADDEDTIME, ADDEDTIME -> Some v
    | _,         _         -> None

  let pack_size (type a) (v : a) : a t -> int * int = function
    | NAME      -> size_string v
    | CRTIME    -> sizeof Time_unix.Timespec.t, 0
    | MODTIME   -> sizeof Time_unix.Timespec.t, 0
    | CHGTIME   -> sizeof Time_unix.Timespec.t, 0
    | ACCTIME   -> sizeof Time_unix.Timespec.t, 0
    | BKUPTIME  -> sizeof Time_unix.Timespec.t, 0
    | FNDRINFO  -> 32, 0
    | ADDEDTIME -> sizeof Time_unix.Timespec.t, 0

  let pack (type a)
    : a t -> (unit ptr * char ptr) -> a -> unit ptr * char ptr = function
    | NAME      -> write_string
    | CRTIME    -> write_timespec
    | MODTIME   -> write_timespec
    | CHGTIME   -> write_timespec
    | ACCTIME   -> write_timespec
    | BKUPTIME  -> write_timespec
    | FNDRINFO  -> write_bytes 32
    | ADDEDTIME -> write_timespec

end

module Volume = struct
  type _ t =
    | FSTYPE : int32 t
    | SIGNATURE : int32 t
(*    | SIZE : off_t t
    | SPACEFREE : off_t t
    | SPACEAVAIL : off_t t
    | MINALLOCATION : off_t t
      | ALLOCATIONCLUMP : off_t t *)
    | IOBLOCKSIZE : int32 t
    | OBJCOUNT : int32 t
    | FILECOUNT : int32 t
    | DIRCOUNT : int32 t
    | MAXOBJCOUNT : int32 t
    | MOUNTPOINT : string t
    | NAME : string t
    | MOUNTFLAGS : int32 t
    | MOUNTEDDEVICE : string t
    (*| ENCODINGSUSED : ?? t*)
    (*| CAPABILITIES
    | UUID
      | ATTRIBUTES*)

  let to_group (type a) : a t -> Unsigned.UInt32.t =
    Types.Attributes.Volume.(function
      | FSTYPE -> fstype
      | SIGNATURE -> signature
      | IOBLOCKSIZE -> ioblocksize
      | OBJCOUNT -> objcount
      | FILECOUNT -> filecount
      | DIRCOUNT -> dircount
      | MAXOBJCOUNT -> maxobjcount
      | MOUNTPOINT -> mountpoint
      | NAME -> name
      | MOUNTFLAGS -> mountflags
      | MOUNTEDDEVICE -> mounteddevice
    )

  let compare (type a) (type b) (x : a t) (y : b t) = match x, y with
    | FSTYPE, FSTYPE -> 0
    | FSTYPE, _ -> -1
    | _, FSTYPE -> 1
    | SIGNATURE, SIGNATURE -> 0
    | SIGNATURE, _ -> -1
    | _, SIGNATURE -> 1
    | IOBLOCKSIZE, IOBLOCKSIZE -> 0
    | IOBLOCKSIZE, _ -> -1
    | _, IOBLOCKSIZE -> 1
    | OBJCOUNT, OBJCOUNT -> 0
    | OBJCOUNT, _ -> -1
    | _, OBJCOUNT -> 1
    | FILECOUNT, FILECOUNT -> 0
    | FILECOUNT, _ -> -1
    | _, FILECOUNT -> 1
    | DIRCOUNT, DIRCOUNT -> 0
    | DIRCOUNT, _ -> -1
    | _, DIRCOUNT -> 1
    | MAXOBJCOUNT, MAXOBJCOUNT -> 0
    | MAXOBJCOUNT, _ -> -1
    | _, MAXOBJCOUNT -> 1
    | MOUNTPOINT, MOUNTPOINT -> 0
    | MOUNTPOINT, _ -> -1
    | _, MOUNTPOINT -> 1
    | NAME, NAME -> 0
    | NAME, _ -> -1
    | _, NAME -> 1
    | MOUNTFLAGS, MOUNTFLAGS -> 0
    | MOUNTFLAGS, _ -> -1
    | _, MOUNTFLAGS -> 1
    | MOUNTEDDEVICE, MOUNTEDDEVICE -> 0

  let unpack (type a) : a t -> unit ptr -> a * unit ptr = function
    | FSTYPE        -> read_uint32
    | SIGNATURE     -> read_uint32
    | IOBLOCKSIZE   -> read_uint32
    | OBJCOUNT      -> read_uint32
    | FILECOUNT     -> read_uint32
    | DIRCOUNT      -> read_uint32
    | MAXOBJCOUNT   -> read_uint32
    | MOUNTPOINT    -> read_string
    | NAME          -> read_string
    | MOUNTFLAGS    -> read_uint32
    | MOUNTEDDEVICE -> read_string

  let same (type a) (type b) (v : a) (x : b t) (y : a t) : b option =
    match x, y with
    | FSTYPE,        FSTYPE        -> Some v
    | SIGNATURE,     SIGNATURE     -> Some v
    | IOBLOCKSIZE,   IOBLOCKSIZE   -> Some v
    | OBJCOUNT,      OBJCOUNT      -> Some v
    | FILECOUNT,     FILECOUNT     -> Some v
    | DIRCOUNT,      DIRCOUNT      -> Some v
    | MAXOBJCOUNT,   MAXOBJCOUNT   -> Some v
    | MOUNTPOINT,    MOUNTPOINT    -> Some v
    | NAME,          NAME          -> Some v
    | MOUNTFLAGS,    MOUNTFLAGS    -> Some v
    | MOUNTEDDEVICE, MOUNTEDDEVICE -> Some v
    | _,             _             -> None

  let pack_size (type a) (v : a) : a t -> int * int = function
    | FSTYPE        -> 4, 0
    | SIGNATURE     -> 4, 0
    | IOBLOCKSIZE   -> 4, 0
    | OBJCOUNT      -> 4, 0
    | FILECOUNT     -> 4, 0
    | DIRCOUNT      -> 4, 0
    | MAXOBJCOUNT   -> 4, 0
    | MOUNTPOINT    -> size_string v
    | NAME          -> size_string v
    | MOUNTFLAGS    -> 4, 0
    | MOUNTEDDEVICE -> size_string v

  let pack (type a)
    : a t -> (unit ptr * char ptr) -> a -> unit ptr * char ptr = function
    | FSTYPE        -> write_uint32
    | SIGNATURE     -> write_uint32
    | IOBLOCKSIZE   -> write_uint32
    | OBJCOUNT      -> write_uint32
    | FILECOUNT     -> write_uint32
    | DIRCOUNT      -> write_uint32
    | MAXOBJCOUNT   -> write_uint32
    | MOUNTPOINT    -> write_string
    | NAME          -> write_string
    | MOUNTFLAGS    -> write_uint32
    | MOUNTEDDEVICE -> write_string

end

module Directory = struct
  type _ t =
    | LINKCOUNT : int32 t
    | ENTRYCOUNT : int32 t
    | MOUNTSTATUS : int32 t

  let to_group (type a) : a t -> Unsigned.UInt32.t =
    Types.Attributes.Directory.(function
      | LINKCOUNT -> linkcount
      | ENTRYCOUNT -> entrycount
      | MOUNTSTATUS -> mountstatus
    )

  let compare (type a) (type b) (x : a t) (y : b t) = match x, y with
    | LINKCOUNT, LINKCOUNT -> 0
    | LINKCOUNT, _ -> -1
    | _, LINKCOUNT -> 1
    | ENTRYCOUNT, ENTRYCOUNT -> 0
    | ENTRYCOUNT, _ -> -1
    | _, ENTRYCOUNT -> 1
    | MOUNTSTATUS, MOUNTSTATUS -> 0

  let unpack (type a) : a t -> unit ptr -> a * unit ptr = function
    | LINKCOUNT   -> read_uint32
    | ENTRYCOUNT  -> read_uint32
    | MOUNTSTATUS -> read_uint32

  let same (type a) (type b) (v : a) (x : b t) (y : a t) : b option =
    match x, y with
    | LINKCOUNT,   LINKCOUNT   -> Some v
    | ENTRYCOUNT,  ENTRYCOUNT  -> Some v
    | MOUNTSTATUS, MOUNTSTATUS -> Some v
    | _,           _           -> None

  let pack_size (type a) (v : a) : a t -> int * int = function
    | LINKCOUNT   -> 4, 0
    | ENTRYCOUNT  -> 4, 0
    | MOUNTSTATUS -> 4, 0

  let pack (type a)
    : a t -> (unit ptr * char ptr) -> a -> unit ptr * char ptr = function
    | LINKCOUNT   -> write_uint32
    | ENTRYCOUNT  -> write_uint32
    | MOUNTSTATUS -> write_uint32
end

module File = struct
  type _ t =
    | LINKCOUNT : int32 t
    | TOTALSIZE : int64 t
    | ALLOCSIZE : int64 t
    | IOBLOCKSIZE : int32 t
    | CLUMPSIZE : int32 t
    | DEVTYPE : int32 t
    (*| FILETYPE*)
    | FORKCOUNT : int32 t
(*    | FORKLIST
    | DATALENGTH
    | DATAALLOCSIZE
    | DATAEXTENTS
    | RSRCLENGTH
    | RSRCALLOCSIZE
      | RSRCEXTENTS*)

  let to_group (type a) : a t -> Unsigned.UInt32.t =
    Types.Attributes.File.(function
      | LINKCOUNT -> linkcount
      | TOTALSIZE -> totalsize
      | ALLOCSIZE -> allocsize
      | IOBLOCKSIZE -> ioblocksize
      | CLUMPSIZE -> clumpsize
      | DEVTYPE -> devtype
      | FORKCOUNT -> forkcount
    )

  let compare (type a) (type b) (x : a t) (y : b t) = match x, y with
    | LINKCOUNT, LINKCOUNT -> 0
    | LINKCOUNT, _ -> -1
    | _, LINKCOUNT -> 1
    | TOTALSIZE, TOTALSIZE -> 0
    | TOTALSIZE, _ -> -1
    | _, TOTALSIZE -> 1
    | ALLOCSIZE, ALLOCSIZE -> 0
    | ALLOCSIZE, _ -> -1
    | _, ALLOCSIZE -> 1
    | IOBLOCKSIZE, IOBLOCKSIZE -> 0
    | IOBLOCKSIZE, _ -> -1
    | _, IOBLOCKSIZE -> 1
    | CLUMPSIZE, CLUMPSIZE -> 0
    | CLUMPSIZE, _ -> -1
    | _, CLUMPSIZE -> 1
    | DEVTYPE, DEVTYPE -> 0
    | DEVTYPE, _ -> -1
    | _, DEVTYPE -> 1
    | FORKCOUNT, FORKCOUNT -> 0

  let unpack (type a) : a t -> unit ptr -> a * unit ptr = function
    | LINKCOUNT   -> read_uint32
    | TOTALSIZE   -> read_off
    | ALLOCSIZE   -> read_off
    | IOBLOCKSIZE -> read_uint32
    | CLUMPSIZE   -> read_uint32
    | DEVTYPE     -> read_uint32
    | FORKCOUNT   -> read_uint32

  let same (type a) (type b) (v : a) (x : b t) (y : a t) : b option =
    match x, y with
    | LINKCOUNT,   LINKCOUNT   -> Some v
    | TOTALSIZE,   TOTALSIZE   -> Some v
    | ALLOCSIZE,   ALLOCSIZE   -> Some v
    | IOBLOCKSIZE, IOBLOCKSIZE -> Some v
    | CLUMPSIZE,   CLUMPSIZE   -> Some v
    | DEVTYPE,     DEVTYPE     -> Some v
    | FORKCOUNT,   FORKCOUNT   -> Some v
    | _,           _           -> None

  let pack_size (type a) (v : a) : a t -> int * int = function
    | LINKCOUNT   -> 4, 0
    | TOTALSIZE   -> sizeof PosixTypes.off_t, 0
    | ALLOCSIZE   -> sizeof PosixTypes.off_t, 0
    | IOBLOCKSIZE -> 4, 0
    | CLUMPSIZE   -> 4, 0
    | DEVTYPE     -> 4, 0
    | FORKCOUNT   -> 4, 0

  let pack (type a)
    : a t -> (unit ptr * char ptr) -> a -> unit ptr * char ptr = function
    | LINKCOUNT   -> write_uint32
    | TOTALSIZE   -> write_off
    | ALLOCSIZE   -> write_off
    | IOBLOCKSIZE -> write_uint32
    | CLUMPSIZE   -> write_uint32
    | DEVTYPE     -> write_uint32
    | FORKCOUNT   -> write_uint32

end

module Query = struct
  type t =
    | Common : _ Common.t -> t
    | Volume : _ Volume.t -> t
    | Directory : _ Directory.t -> t
    | File : _ File.t -> t

  let (|||) = Unsigned.UInt32.logor

  (* TODO: ATTR_VOL_INFO mandatory for volume attrs? *)
  let groups_of_attrs =
    let rec next (common, vol, dir, file, fork) = function
      | (Common c)::rest ->
        next (common ||| (Common.to_group c), vol, dir, file, fork) rest
      | (Volume v)::rest ->
        next (common, vol ||| (Volume.to_group v), dir, file, fork) rest
      | (Directory d)::rest ->
        next (common, vol, dir ||| (Directory.to_group d), file, fork) rest
      | (File f)::rest ->
        next (common, vol, dir, file ||| (File.to_group f), fork) rest
      | [] -> (common, vol, dir, file, fork)
    in
    Unsigned.UInt32.(next (zero, zero, zero, zero, zero))

  let compare x y = match x, y with
    | Common x, Common y -> Common.compare x y
    | Common _, _ -> -1
    | _, Common _ -> 1
    | Volume x, Volume y -> Volume.compare x y
    | Volume _, _ -> -1
    | _, Volume _ -> 1
    | Directory x, Directory y -> Directory.compare x y
    | Directory _, _ -> -1
    | _, Directory _ -> 1
    | File x, File y -> File.compare x y

  let sort_filter filter attrs =
    let attrs = List.sort_uniq compare attrs in
    let common    = getf filter Types.AttrSet.commonattr in
    let volume    = getf filter Types.AttrSet.volattr in
    let directory = getf filter Types.AttrSet.dirattr in
    let file      = getf filter Types.AttrSet.fileattr in
    List.filter Unsigned.UInt32.(function
      | Common c    ->
        0 <> compare zero (logand common (Common.to_group c))
      | Volume v    ->
        0 <> compare zero (logand volume (Volume.to_group v))
      | Directory d ->
        0 <> compare zero (logand directory (Directory.to_group d))
      | File f      ->
        0 <> compare zero (logand file (File.to_group f))
    ) attrs
end

module Select = struct
  type 'a t =
    | Common of 'a Common.t
    | Volume of 'a Volume.t
    | Directory of 'a Directory.t
    | File of 'a File.t

  let to_query = function
    | Common c    -> Query.Common c
    | Volume v    -> Query.Volume v
    | Directory d -> Query.Directory d
    | File f      -> Query.File f
end

module Value = struct
  type t =
    | Common : 'a Common.t * 'a -> t
    | Volume : 'a Volume.t * 'a -> t
    | Directory : 'a Directory.t * 'a -> t
    | File : 'a File.t * 'a -> t

  let unpack q data_p = match q with
    | Query.Common q ->
      let v, next = Common.unpack q data_p in
      Common (q, v), next
    | Query.Volume q ->
      let v, next = Volume.unpack q data_p in
      Volume (q, v), next
    | Query.Directory q ->
      let v, next = Directory.unpack q data_p in
      Directory (q, v), next
    | Query.File q ->
      let v, next = File.unpack q data_p in
      File (q, v), next

  let select (type a) (select : a Select.t) result : a option =
    match select, result with
    | Select.Common q,    Common (q',v)    -> Common.same v q q'
    | Select.Volume q,    Volume (q',v)    -> Volume.same v q q'
    | Select.Directory q, Directory (q',v) -> Directory.same v q q'
    | Select.File q,      File (q',v)      -> File.same v q q'
    | _,                  _                -> None

  let to_query = function
    | Common (c, _)    -> Query.Common c
    | Volume (v, _)    -> Query.Volume v
    | Directory (d, _) -> Query.Directory d
    | File (f, _)      -> Query.File f

  let groups_of_attrs attrs = Query.groups_of_attrs (List.map to_query attrs)

  let pack_size = function
    | Common (q, v)    -> Common.pack_size v q
    | Volume (q, v)    -> Volume.pack_size v q
    | Directory (q, v) -> Directory.pack_size v q
    | File (q, v)      -> File.pack_size v q

  let pack p = function
    | Common (q, v)    -> Common.pack q p v
    | Volume (q, v)    -> Volume.pack q p v
    | Directory (q, v) -> Directory.pack q p v
    | File (q, v)      -> File.pack q p v
end

let attrlist_of_groups ~returned (common, vol, dir, file, fork) =
  let attrlist_p = allocate_n Types.AttrList.t ~count:1 in
  let attrlist = !@ attrlist_p in
  let common =
    if returned
    then Unsigned.UInt32.logor common Types.Attributes.Common.returned_attrs
    else common
  in
  setf attrlist Types.AttrList.bitmapcount Types.AttrList.attr_bit_map_count;
  setf attrlist Types.AttrList.commonattr common;
  setf attrlist Types.AttrList.volattr vol;
  setf attrlist Types.AttrList.dirattr dir;
  setf attrlist Types.AttrList.fileattr file;
  setf attrlist Types.AttrList.forkattr fork;
  attrlist_p

let xgetlist ~no_follow ~size attrs f call label =
  let rec try_call count =
    let attrlist_p =
      attrlist_of_groups ~returned:true (Query.groups_of_attrs attrs)
    in
    let buffer = to_voidp (allocate_n char ~count) in
    let count_ = Unsigned.Size_t.of_int count in
    let options =
      if no_follow
      then Types.Options.(Unsigned.ULong.logor report_fullsize nofollow)
      else Types.Options.report_fullsize
    in
    Errno_unix.raise_on_errno ~call ~label (fun () ->
      let rc = f attrlist_p buffer count_ options in
      if rc < 0 then None else Some ()
    );
    let data_p = from_voidp uint32_t buffer in
    let returned_size = Unsigned.UInt32.to_int (!@ data_p) in
    match compare returned_size count with
    | x when x >= 1 -> (* too small, try again *)
      try_call returned_size
    | _ -> (* unpack *)
      let data_p = data_p +@ 1 in
      let returned_p = from_voidp Types.AttrSet.t (to_voidp data_p) in
      let attrs = Query.sort_filter (!@ returned_p) attrs in
      let data_p = to_voidp (returned_p +@ 1) in
      fst (List.fold_left (fun (results, data_p) attr ->
        let result, next = Value.unpack attr data_p in
        (result::results, next)
      ) ([], data_p) attrs)
  in
  try_call size

let getlist ?(no_follow=false) ?(size=64) attrs path =
  xgetlist ~no_follow ~size attrs (C.getattrlist path) "getattrlist" path

let fgetlist ?(no_follow=false) ?(size=64) attrs fd =
  let fd = int_of_fd fd in
  xgetlist ~no_follow ~size attrs (C.fgetattrlist fd) "fgetattrlist"
    (string_of_int fd)

let getlistat ?(no_follow=false) ?(size=64) attrs dirfd path =
  let fd = int_of_fd dirfd in
  xgetlist ~no_follow ~size attrs (C.getattrlistat fd path) "getattrlistat"
    (Printf.sprintf "%d:%s" fd path)

let xget ?no_follow ?size selector f v =
  let attrs = f ?no_follow ?size [Select.to_query selector] v in
  let rec check = function
    | [] -> None
    | next::rest -> match Value.select selector next with
      | Some v -> Some v
      | None -> check rest
  in
  check attrs

let get ?(no_follow=false) ?(size=64) selector path =
  xget ~no_follow ~size selector getlist path

let fget ?(no_follow=false) ?(size=64) selector fd =
  xget ~no_follow ~size selector fgetlist fd

let getat ?(no_follow=false) ?(size=64) selector fd path =
  let f ?no_follow ?size attrs = getlistat ?no_follow ?size attrs fd in
  xget ~no_follow ~size selector f path

let xsetlist ~no_follow attrs f call label =
  let attrlist_p =
    attrlist_of_groups ~returned:false (Value.groups_of_attrs attrs)
  in
  let attrs = List.sort_uniq compare attrs in
  let count_list, count_trailer =
    List.fold_left (fun (size_list, size_trailer) attr ->
      let attr_size, attr_trailer = Value.pack_size attr in
      size_list + attr_size, size_trailer + attr_trailer
    ) (0, 0) attrs
  in
  let count = count_list + count_trailer in
  let buffer_p = allocate_n char ~count in
  let buffer = to_voidp buffer_p in
  let trailer = buffer_p +@ count_list in
  ignore (List.fold_left Value.pack (buffer, trailer) attrs);
  let options =
    if no_follow then Types.Options.nofollow else Unsigned.ULong.zero
  in
  Errno_unix.raise_on_errno ~call ~label (fun () ->
    let rc = f attrlist_p buffer (Unsigned.Size_t.of_int count) options in
    if rc < 0 then None else Some ()
  )

let setlist ?(no_follow=false) attrs path =
  xsetlist ~no_follow attrs (C.setattrlist path) "setattrlist" path

let fsetlist ?(no_follow=false) attrs fd =
  let fd = int_of_fd fd in
  xsetlist ~no_follow attrs (C.fsetattrlist fd) "fsetattrlist"
    (string_of_int fd)

let set ?(no_follow=false) attr path = setlist ~no_follow [attr] path

let fset ?(no_follow=false) attr fd = fsetlist ~no_follow [attr] fd

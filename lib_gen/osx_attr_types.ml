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

module C(F: Cstubs.Types.TYPE) = struct
  module Options = struct
    type t = Unsigned.ULong.t

    let t = F.ulong

    let nofollow          = F.constant "FSOPT_NOFOLLOW" t
    let report_fullsize   = F.constant "FSOPT_REPORT_FULLSIZE" t
    let pack_inval_attrs  = F.constant "FSOPT_PACK_INVAL_ATTRS" t
    let attr_cmn_extended = F.constant "FSOPT_ATTR_CMN_EXTENDED" t
  end

  module AttrGroup = struct
    let t = F.typedef F.uint32_t "attrgroup_t"
  end

  module AttrList = struct
    type t

    let t : t Ctypes_static.structure F.typ = F.structure "attrlist"
    let ( -:* ) s x = F.field t s x
    let bitmapcount = "bitmapcount" -:* F.ushort
    let commonattr  = "commonattr"  -:* AttrGroup.t
    let volattr     = "volattr"     -:* AttrGroup.t
    let dirattr     = "dirattr"     -:* AttrGroup.t
    let fileattr    = "fileattr"    -:* AttrGroup.t
    let forkattr    = "forkattr"    -:* AttrGroup.t
    let () = F.seal t

    let attr_bit_map_count = F.constant "ATTR_BIT_MAP_COUNT" F.ushort
  end

  module AttrSet = struct
    type t

    let t : t Ctypes_static.structure F.typ = F.structure "attribute_set"
    let ( -:* ) s x = F.field t s x
    let commonattr  = "commonattr"  -:* AttrGroup.t
    let volattr     = "volattr"     -:* AttrGroup.t
    let dirattr     = "dirattr"     -:* AttrGroup.t
    let fileattr    = "fileattr"    -:* AttrGroup.t
    let forkattr    = "forkattr"    -:* AttrGroup.t
    let () = F.seal t
  end

  module AttrReference = struct
    type t

    let t : t Ctypes_static.structure F.typ = F.structure "attrreference"
    let ( -:* ) s x = F.field t s x
    let dataoffset = "attr_dataoffset" -:* F.int32_t
    let length     = "attr_length"     -:* F.uint32_t
    let () = F.seal t
  end

  module Attributes = struct
    module Common = struct
      let returned_attrs = F.constant "ATTR_CMN_RETURNED_ATTRS" AttrGroup.t
      let name           = F.constant "ATTR_CMN_NAME" AttrGroup.t
      let devid          = F.constant "ATTR_CMN_DEVID" AttrGroup.t
      let fsid           = F.constant "ATTR_CMN_FSID" AttrGroup.t
      let objtype        = F.constant "ATTR_CMN_OBJTYPE" AttrGroup.t
      let objtag         = F.constant "ATTR_CMN_OBJTAG" AttrGroup.t
      let objid          = F.constant "ATTR_CMN_OBJID" AttrGroup.t
      let objpermanentid = F.constant "ATTR_CMN_OBJPERMANENTID" AttrGroup.t
      let parobjid       = F.constant "ATTR_CMN_PAROBJID" AttrGroup.t
      let script         = F.constant "ATTR_CMN_SCRIPT" AttrGroup.t
      let crtime         = F.constant "ATTR_CMN_CRTIME" AttrGroup.t
      let modtime        = F.constant "ATTR_CMN_MODTIME" AttrGroup.t
      let chgtime        = F.constant "ATTR_CMN_CHGTIME" AttrGroup.t
      let acctime        = F.constant "ATTR_CMN_ACCTIME" AttrGroup.t
      let bkuptime       = F.constant "ATTR_CMN_BKUPTIME" AttrGroup.t
      let fndrinfo       = F.constant "ATTR_CMN_FNDRINFO" AttrGroup.t
      let ownerid        = F.constant "ATTR_CMN_OWNERID" AttrGroup.t
      let grpid          = F.constant "ATTR_CMN_GRPID" AttrGroup.t
      let accessmask     = F.constant "ATTR_CMN_ACCESSMASK" AttrGroup.t
      let flags          = F.constant "ATTR_CMN_FLAGS" AttrGroup.t
      let gen_count      = F.constant "ATTR_CMN_GEN_COUNT" AttrGroup.t
      let document_id    = F.constant "ATTR_CMN_DOCUMENT_ID" AttrGroup.t
      let useraccess     = F.constant "ATTR_CMN_USERACCESS" AttrGroup.t
      let extended_security =
        F.constant "ATTR_CMN_EXTENDED_SECURITY" AttrGroup.t
      let uuid           = F.constant "ATTR_CMN_UUID" AttrGroup.t
      let grpuuid        = F.constant "ATTR_CMN_GRPUUID" AttrGroup.t
      let fileid         = F.constant "ATTR_CMN_FILEID" AttrGroup.t
      let parentid       = F.constant "ATTR_CMN_PARENTID" AttrGroup.t
      let fullpath       = F.constant "ATTR_CMN_FULLPATH" AttrGroup.t
      let addedtime      = F.constant "ATTR_CMN_ADDEDTIME" AttrGroup.t
      let data_protect_flags =
        F.constant "ATTR_CMN_DATA_PROTECT_FLAGS" AttrGroup.t
    end

    module Volume = struct
      let info            = F.constant "ATTR_VOL_INFO" AttrGroup.t
      let fstype          = F.constant "ATTR_VOL_FSTYPE" AttrGroup.t
      let signature       = F.constant "ATTR_VOL_SIGNATURE" AttrGroup.t
      let size            = F.constant "ATTR_VOL_SIZE" AttrGroup.t
      let spacefree       = F.constant "ATTR_VOL_SPACEFREE" AttrGroup.t
      let spaceavail      = F.constant "ATTR_VOL_SPACEAVAIL" AttrGroup.t
      let minallocation   = F.constant "ATTR_VOL_MINALLOCATION" AttrGroup.t
      let allocationclump = F.constant "ATTR_VOL_ALLOCATIONCLUMP" AttrGroup.t
      let ioblocksize     = F.constant "ATTR_VOL_IOBLOCKSIZE" AttrGroup.t
      let objcount        = F.constant "ATTR_VOL_OBJCOUNT" AttrGroup.t
      let filecount       = F.constant "ATTR_VOL_FILECOUNT" AttrGroup.t
      let dircount        = F.constant "ATTR_VOL_DIRCOUNT" AttrGroup.t
      let maxobjcount     = F.constant "ATTR_VOL_MAXOBJCOUNT" AttrGroup.t
      let mountpoint      = F.constant "ATTR_VOL_MOUNTPOINT" AttrGroup.t
      let name            = F.constant "ATTR_VOL_NAME" AttrGroup.t
      let mountflags      = F.constant "ATTR_VOL_MOUNTFLAGS" AttrGroup.t
      let mounteddevice   = F.constant "ATTR_VOL_MOUNTEDDEVICE" AttrGroup.t
      let encodingsused   = F.constant "ATTR_VOL_ENCODINGSUSED" AttrGroup.t
      let capabilities    = F.constant "ATTR_VOL_CAPABILITIES" AttrGroup.t
      let uuid            = F.constant "ATTR_VOL_UUID" AttrGroup.t
      let attributes      = F.constant "ATTR_VOL_ATTRIBUTES" AttrGroup.t
    end

    module Directory = struct
      let linkcount       = F.constant "ATTR_DIR_LINKCOUNT" AttrGroup.t
      let entrycount      = F.constant "ATTR_DIR_ENTRYCOUNT" AttrGroup.t
      let mountstatus     = F.constant "ATTR_DIR_MOUNTSTATUS" AttrGroup.t
    end

    module File = struct
      let linkcount       = F.constant "ATTR_FILE_LINKCOUNT" AttrGroup.t
      let totalsize       = F.constant "ATTR_FILE_TOTALSIZE" AttrGroup.t
      let allocsize       = F.constant "ATTR_FILE_ALLOCSIZE" AttrGroup.t
      let ioblocksize     = F.constant "ATTR_FILE_IOBLOCKSIZE" AttrGroup.t
      let clumpsize       = F.constant "ATTR_FILE_CLUMPSIZE" AttrGroup.t
      let devtype         = F.constant "ATTR_FILE_DEVTYPE" AttrGroup.t
      let filetype        = F.constant "ATTR_FILE_FILETYPE" AttrGroup.t
      let forkcount       = F.constant "ATTR_FILE_FORKCOUNT" AttrGroup.t
      let forklist        = F.constant "ATTR_FILE_FORKLIST" AttrGroup.t
      let datalength      = F.constant "ATTR_FILE_DATALENGTH" AttrGroup.t
      let dataallocsize   = F.constant "ATTR_FILE_DATAALLOCSIZE" AttrGroup.t
      let dataextents     = F.constant "ATTR_FILE_DATAEXTENTS" AttrGroup.t
      let rsrclength      = F.constant "ATTR_FILE_RSRCLENGTH" AttrGroup.t
      let rsrcallocsize   = F.constant "ATTR_FILE_RSRCALLOCSIZE" AttrGroup.t
      let rsrcextents     = F.constant "ATTR_FILE_RSRCEXTENTS" AttrGroup.t
    end
  end

  module Capabilities = struct
    module Set = struct
      let t = F.typedef (F.array 4 F.uint32_t) "vol_capabilities_set_t"

      let format     = F.constant "VOL_CAPABILITIES_FORMAT" F.int
      let interfaces = F.constant "VOL_CAPABILITIES_INTERFACES" F.int
    end

    module Attr = struct
      type t

      let t : t Ctypes_static.structure F.typ =
        F.structure "vol_capabilities_attr"
      let ( -:* ) s x = F.field t s x
      let capabilities = "capabilities" -:* Set.t
      let valid        = "valid"        -:* Set.t
      let () = F.seal t
    end

    module Format = struct
      let persistentobjectids =
        F.constant "VOL_CAP_FMT_PERSISTENTOBJECTIDS" F.uint32_t
      let symboliclinks   = F.constant "VOL_CAP_FMT_SYMBOLICLINKS" F.uint32_t
      let hardlinks       = F.constant "VOL_CAP_FMT_HARDLINKS" F.uint32_t
      let journal         = F.constant "VOL_CAP_FMT_JOURNAL" F.uint32_t
      let journal_active  = F.constant "VOL_CAP_FMT_JOURNAL_ACTIVE" F.uint32_t
      let no_root_times   = F.constant "VOL_CAP_FMT_NO_ROOT_TIMES" F.uint32_t
      let sparse_files    = F.constant "VOL_CAP_FMT_SPARSE_FILES" F.uint32_t
      let zero_runs       = F.constant "VOL_CAP_FMT_ZERO_RUNS" F.uint32_t
      let case_sensitive  = F.constant "VOL_CAP_FMT_CASE_SENSITIVE" F.uint32_t
      let case_preserving = F.constant "VOL_CAP_FMT_CASE_PRESERVING" F.uint32_t
      let fast_statfs     = F.constant "VOL_CAP_FMT_FAST_STATFS" F.uint32_t
      let c2tb_filesize   = F.constant "VOL_CAP_FMT_2TB_FILESIZE" F.uint32_t
      let opendenymodes   = F.constant "VOL_CAP_FMT_OPENDENYMODES" F.uint32_t
      let hidden_files    = F.constant "VOL_CAP_FMT_HIDDEN_FILES" F.uint32_t
      let path_from_id    = F.constant "VOL_CAP_FMT_PATH_FROM_ID" F.uint32_t
      let no_volume_sizes = F.constant "VOL_CAP_FMT_NO_VOLUME_SIZES" F.uint32_t
      let c64bit_object_ids =
        F.constant "VOL_CAP_FMT_64BIT_OBJECT_IDS" F.uint32_t
    end

    module Interfaces = struct
      let searchfs      = F.constant "VOL_CAP_INT_SEARCHFS" F.uint32_t
      let attrlist      = F.constant "VOL_CAP_INT_ATTRLIST" F.uint32_t
      let nfsexport     = F.constant "VOL_CAP_INT_NFSEXPORT" F.uint32_t
      let readdirattr   = F.constant "VOL_CAP_INT_READDIRATTR" F.uint32_t
      let exchangedata  = F.constant "VOL_CAP_INT_EXCHANGEDATA" F.uint32_t
      let copyfile      = F.constant "VOL_CAP_INT_COPYFILE" F.uint32_t
      let allocate      = F.constant "VOL_CAP_INT_ALLOCATE" F.uint32_t
      let vol_rename    = F.constant "VOL_CAP_INT_VOL_RENAME" F.uint32_t
      let advlock       = F.constant "VOL_CAP_INT_ADVLOCK" F.uint32_t
      let flock         = F.constant "VOL_CAP_INT_FLOCK" F.uint32_t
      let extended_security =
        F.constant "VOL_CAP_INT_EXTENDED_SECURITY" F.uint32_t
      let useraccess    = F.constant "VOL_CAP_INT_USERACCESS" F.uint32_t
      let manlock       = F.constant "VOL_CAP_INT_MANLOCK" F.uint32_t
      let extended_attr = F.constant "VOL_CAP_INT_EXTENDED_ATTR" F.uint32_t
      let namedstreams  = F.constant "VOL_CAP_INT_NAMEDSTREAMS" F.uint32_t
    end

    module Attributes = struct
      module Attr = struct
        type t

        let t : t Ctypes_static.structure F.typ =
          F.structure "vol_attributes_attr"
        let ( -:* ) s x = F.field t s x
        let validattr  = "validattr"  -:* Set.t
        let nativeattr = "nativeattr" -:* Set.t
        let () = F.seal t        
      end
    end
  end
end

int osx_attr_getattrlist
(const char *path, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options);

int osx_attr_fgetattrlist
(int fd, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options);

int osx_attr_getattrlistat
(int fd, const char *path, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options);

int osx_attr_getattrlistbulk
(int dirfd, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, uint64_t options);

int osx_attr_setattrlist
(const char *path, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options);

int osx_attr_fsetattrlist
(int fd, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options);

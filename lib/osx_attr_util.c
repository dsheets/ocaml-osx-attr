#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <sys/attr.h>
#include <unistd.h>

int osx_attr_getattrlist
(const char *path, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options)
{
  int r;
  caml_release_runtime_system();
  r = getattrlist(path, attrList, attrBuf, attrBufSize, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_attr_fgetattrlist
(int fd, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options)
{
  int r;
  caml_release_runtime_system();
  r = fgetattrlist(fd, attrList, attrBuf, attrBufSize, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_attr_getattrlistat
(int fd, const char *path, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options)
{
  int r;
  caml_release_runtime_system();
  r = getattrlistat(fd, path, attrList, attrBuf, attrBufSize, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_attr_getattrlistbulk
(int dirfd, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, uint64_t options)
{
  int r;
  caml_release_runtime_system();
  r = getattrlistbulk(dirfd, attrList, attrBuf, attrBufSize, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_attr_setattrlist
(const char *path, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options)
{
  int r;
  caml_release_runtime_system();
  r = setattrlist(path, attrList, attrBuf, attrBufSize, options);
  caml_acquire_runtime_system();
  return r;
}

int osx_attr_fsetattrlist
(int fd, struct attrlist * attrList, void * attrBuf,
 size_t attrBufSize, unsigned long options)
{
  int r;
  caml_release_runtime_system();
  r = fsetattrlist(fd, attrList, attrBuf, attrBufSize, options);
  caml_acquire_runtime_system();
  return r;
}

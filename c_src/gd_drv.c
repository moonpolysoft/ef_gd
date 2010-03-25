#include <stdio.h>
#include <erl_driver.h>
#include <ei.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <gd.h>

#define SIZE 's'
#define READ 'r'
#define RESIZE 'e'
#define CROP 'c'
#define BLOB 'b'

#define JPG 'j'
#define GIF 'g'
#define PNG 'p'

static void * gdImageGifPtrWrap(gdImagePtr image, int *size, int quality) {
  return gdImageGifPtr(image, size);
}

typedef gdImagePtr (*imageReadFun)(int, void *);
typedef void * (*blobOutputFun)(gdImagePtr, int *, int);
typedef void (*asyncFun)(void*);

typedef struct {
  ErlDrvPort port;
  char format;
  gdImagePtr image;
  imageReadFun read;
  blobOutputFun blob;
} Gd;

typedef struct {
  Gd *gd;
  int size;
  char data[];
} Cmd;

//callbacks
static ErlDrvData init(ErlDrvPort port, char *cmd);
static void stop(ErlDrvData handle);
static void output(ErlDrvData handle, char* buff, int len);

//actions
static void read_image(Cmd *command);
static void get_size(Cmd *command);
static void resize(Cmd *command);
static void get_blob(Cmd *command);
static void crop(Cmd *command);

static void send_atom(ErlDrvPort port, const char *str) {
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_atom(&x, str);
  driver_output(port, x.buff, x.index);
  ei_x_free(&x);
}

static ErlDrvData init(ErlDrvPort port, char *cmd) {
  Gd *gd;
  
  gd = driver_alloc(sizeof(Gd));
  
  if (NULL == gd) {
    return ERL_DRV_ERROR_GENERAL;
  }
  
  gd->port = port;
  gd->image = NULL;
  return (ErlDrvData)gd;
}

static void stop(ErlDrvData handle) {
  Gd *gd = (Gd*)handle;
  
  if (NULL != gd) {
    if (NULL != gd->image) gdImageDestroy(gd->image);
    driver_free(gd);
  }
}

static void output(ErlDrvData handle, char *buff, int len) {
  Gd *gd = (Gd*)handle;
  char cmd = buff[0];
  char *data = &buff[1];
  int size = len-1;
  asyncFun function;
  
  Cmd *command = driver_alloc(sizeof(Cmd) + size);
  command->gd = gd;
  command->size = size;
  memcpy(command->data, data, size);
  
  switch(cmd) {
  case SIZE:
    function = get_size;
    break;
  case READ:
    function = read_image;
    break;
  case RESIZE:
    function = resize;
    break;
  case BLOB:
    function = get_blob;
    break;
  case CROP:
    function = crop;
    break;
  }
  //function(command);
  //driver_free(command);
  driver_async(gd->port, NULL, function, command, driver_free);
}

static void read_image(Cmd *cmd) {
  char *buff = cmd->data;
  int len = cmd->size;
  Gd *gd = cmd->gd;
  
  gd->format = buff[0];
  char *data = &buff[1];
  int size = len;
  
  if (gd->image != NULL) {
    gdImageDestroy(gd->image);
    gd->image = NULL;
  }
  
  switch (gd->format) {
  case JPG:
    gd->read = gdImageCreateFromJpegPtr;
    gd->blob = gdImageJpegPtr;
    
    break;
  case PNG:
    gd->read = gdImageCreateFromPngPtr;
    gd->blob = gdImagePngPtrEx;
    break;
  case GIF:
    gd->read = gdImageCreateFromGifPtr;
    gd->blob = gdImageGifPtrWrap;
    break;
  }
  
  gd->image = gd->read(size, data);
  if (NULL == gd->image) {
    driver_failure_atom(gd->port, "gd_create_failed");
    return;
  }
  
  send_atom(gd->port, "ok");
}

static void get_size(Cmd *cmd) {
  char *buff = cmd->data;
  int len = cmd->size;
  Gd *gd = cmd->gd;
  unsigned int x;
  unsigned int y;
  ei_x_buff b;
  
  if (gd->image == NULL) {
    driver_failure_atom(gd->port, "null_image");
    return;
  }
  
  x = gdImageSX(gd->image);
  y = gdImageSY(gd->image);
  
  ei_x_new_with_version(&b);
  ei_x_encode_tuple_header(&b, 2);
  ei_x_encode_long(&b, x);
  ei_x_encode_long(&b, y);
  driver_output(gd->port, b.buff, b.index);
  ei_x_free(&b);
}

static void resize(Cmd *cmd) {
  char *buff = cmd->data;
  int len = cmd->size;
  Gd *gd = cmd->gd;
  int index = 0;
  unsigned long width, height, srcW, srcH;
  gdImagePtr destination = NULL;
  
  ei_decode_version(buff, &index, NULL);
  ei_decode_tuple_header(buff, &index, NULL);
  ei_decode_ulong(buff, &index, &width);
  ei_decode_ulong(buff, &index, &height);
  
  if (NULL == gd->image) {
    driver_failure_atom(gd->port, "null_image");
    return;
  }
  
  srcW = gdImageSX(gd->image);
  srcH = gdImageSY(gd->image);
  
  destination = gdImageCreateTrueColor(width, height);
  
  if (NULL == destination) {
    driver_failure_posix(gd->port, ENOMEM);
    return;
  }
  
  gdImageCopyResampled(destination, gd->image, 0, 0, 0, 0, width, height, srcW, srcH);
  gdImageDestroy(gd->image);
  gd->image = destination;
  
  send_atom(gd->port, "ok");
}

static void get_blob(Cmd *cmd) {
  char *buff = cmd->data;
  int len = cmd->size;
  Gd *gd = cmd->gd;
  int index = 0;
  void *imgData = NULL;
  int size = 0;
  ErlDrvBinary * bin;
  long quality;
  
  ei_decode_version(buff, &index, NULL);
  ei_decode_long(buff, &index, &quality);
  
  if (NULL == gd->image) {
    driver_failure_atom(gd->port, "null_image");
    return;
  }
  
  imgData = gd->blob(gd->image, &size, quality);
  
  if (NULL == imgData) {
    driver_failure_posix(gd->port, ENOMEM);
    return;
  }
  
  bin = driver_alloc_binary(size);
  if (NULL == bin) {
    driver_failure_posix(gd->port, ENOMEM);
    return;
  }
  
  memcpy(bin->orig_bytes, imgData, size);
  gdFree(imgData);
  ErlDrvTermData spec[] = {
    ERL_DRV_PORT, driver_mk_port(gd->port),
    ERL_DRV_ATOM, driver_mk_atom("ok"),
    ERL_DRV_BINARY, bin, size, 0,
    ERL_DRV_TUPLE, 3};
  driver_output_term(gd->port, spec, sizeof(spec) / sizeof(ERL_DRV_PORT));
  driver_free_binary(bin);
}

static void crop(Cmd *cmd) {
  char *buff = cmd->data;
  int len = cmd->size;
  Gd *gd = cmd->gd;
  int index = 0;
  long width, height;
  int srcW, srcH, srcX, srcY, destX, destY, playX, playY;
  gdImagePtr destination = NULL;
  
  ei_decode_version(buff, &index, NULL);
  ei_decode_tuple_header(buff, &index, NULL);
  ei_decode_long(buff, &index, &width);
  ei_decode_long(buff, &index, &height);
  
  if (NULL == gd->image) {
    driver_failure_atom(gd->port, "null_image");
    return;
  }
  
  srcW = gdImageSX(gd->image);
  srcH = gdImageSY(gd->image);
  
  destination = gdImageCreateTrueColor(width, height);
  if (NULL == destination) {
    driver_failure_posix(gd->port, ENOMEM);
    return;
  }
  gdImageFilledRectangle(destination, 0, 0, width, height, gdImageColorAllocate(destination, 255, 255, 255));
  destX = (width - srcW) / 2;
  destY = (height - srcH) / 2;
  gdImageCopy(destination, gd->image, destX, destY, 0, 0, srcW, srcH);
  gdImageDestroy(gd->image);
  gd->image = destination;
  send_atom(gd->port, "ok");
}

static ErlDrvEntry gd_driver_entry = {
    NULL,                             /* init */
    init, 
    stop, 
    output,                           /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */ 
    (char*)"gd_drv",                    /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    NULL,                             /* outputv */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

DRIVER_INIT(gd_driver_entry) {
  return &gd_driver_entry;
}

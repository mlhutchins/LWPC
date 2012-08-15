#include <windows.h>

#include <stdio.h>

#define FULLSCREEN 10
#define WINDOWSCREEN 11

#define MxColors 32
#define MxFills 12
#define MAX_POLY_POINTS 4800

#define PCL_RESOLUTION 300
#define XSIZE 10*PCL_RESOLUTION
#define YSIZE 10*PCL_RESOLUTION

#define ESC 0x1B

/* Define pcl_file_pointer as global variable */
extern FILE *pcl_file_pointer;

/* Define pcl_array as global -extern - should be defined in main */
extern char pcl_array[XSIZE][YSIZE];
extern char pcl_array_land [XSIZE][YSIZE];

/* Define hps as a global variable that should be defined in the main
   driver routine of all pm programs */

//extern HPS hps;
//extern HAB hab;


/* The following structure defines a FORTRAN common block called
   graphics that is used by the LWPC GRF routines */

#pragma aux graphics "^";

//extern struct graphics
extern struct graphics
{
  char          device[200];
  long          logicalunit;
  long          landscape;
  float         units[2];
  float         extent[2];
  unsigned char colorlist[MxColors*24]; /* char*12: name,rgb  */
  long          colorpen[MxColors];
  long          nrcolors;
  unsigned char filllist[MxFills*16];   /* char* 8: name,type */
  long          nrfills;
  float         origin[2];
  float         position[2];
  unsigned char color[24];              /* char*12: name,rgb  */
  unsigned char fillname[8];            /* char* 8: type      */
  long          fillindx;
  unsigned char pen[4];
  HDC           hdc;
  HBRUSH        hBrush;       /* handle to fill brush */
  HPEN          hPen;
  int           screen_type;
  HPALETTE      palette_handle;

}graphics;



/* End of structure graphics definition */

/* The following structure is used for printing to a graphics device */

typedef struct
{

    HDC hdcPrn;

} PRINTGRF;

/* define structure */
#ifdef DEFINE_PRINTGRF
#define GLOBAL1
#else
#define GLOBAL1 extern
#endif

GLOBAL1 PRINTGRF printgrf;

/************************************************************************
  FUNCTION:
     sys_setup_pallete

  DATE:
     28 July 1997

  LANGUAGE:
     C

  Function:
    Creates a logical color pallete for the colors found in the graphics.ini file

  Parameters Passed:

  Parameters returned:
     None

  Structures Referenced (FORTRAN common blocks)
     graphics

  Functions and Subroutines referenced:

  Entry:

  References:

  Change History:


********************!*****************************************************/


#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "graphics.h"

int sys_setup_palette (void)
{

   /* Local variables used in setting up the color pallete */
   struct {
      LOGPALETTE lp;
      PALETTEENTRY ape[MxColors];
   } pal;

   int             i,j;
   char            red[4], green[4], blue[4];
   //HPALETTE        palette_handle;

   LOGPALETTE*     pLP = (LOGPALETTE*) &pal;     // Create the logical palette

   /*  LOGPALETTE version number - should be 0x300 for all current releases of windows */
   pLP->palVersion = 0x300;

   /* number of colors we want to put in the logical pallete */
   pLP->palNumEntries = MxColors;

   /* Finally, lets put the colrs specified in the graphics.ini file into */
   /* logical palettes color entries */
   for (i = 0; i < MxColors; i++) {

      strset (red,  '\0');
      strset (green,'\0');
      strset (blue, '\0');
      for (j=0; j<3;j++) {
         red[j] = graphics.colorlist[((i*24)+12) + j];
      }
      for (j=0; j<3;j++) {
         green[j] = graphics.colorlist[((i*24)+12) + j+4];
      }
      for (j=0; j<3;j++) {
         blue[j] = graphics.colorlist[((i*24)+12) + j+8];
      }

      pLP->palPalEntry[i].peRed   = atoi(red);
      pLP->palPalEntry[i].peGreen = atoi(green);
      pLP->palPalEntry[i].peBlue  = atoi(blue);
      pLP->palPalEntry[i].peFlags = 0;

   }

   /* create this palette */
   graphics.palette_handle = CreatePalette (pLP);

   /* Select this palette for drawing */
   SelectPalette (graphics.hdc, graphics.palette_handle, FALSE);
   return (0);
}


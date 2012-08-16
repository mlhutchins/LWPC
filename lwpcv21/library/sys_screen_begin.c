/*************************************************************************
  FUNCTION:
     sys_screen_begin

  DATE:
     11 September 1991

  LANGUAGE:
     C

  Function:
     Initializes variables used in plotting to the screen under OS/2
     The graphics screen must already have been opened by the "driver"
     program prior to calling this function.

  Parameters Passed:
     None

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphic

  Functions and Subroutines Referenced:
     GpiSetCharMode
     WinQueryWindowRect
     GpiErase

  References:

  Change History:
      02 Feb 1993   Added call to WinFillRect to paint window with
                    background color when this routine is called.

      06 Mar 1993   Moved initialization of LANDSCAPE and COLOR_LIST
                    to GRF_BEGIN.

      27 Jan 1994   Fixed problem when using resolution of 1024x768

      05 Jul 1995   Added presentation handle to graphics structure

      21 Sep 1995   Renamed to sys_screen_begin (from sys_begin) for
                    naming consistency.

********************!*****************************************************/

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "graphics.h"

void sys_screen_begin (void)
{
   static int       rc;
   static float     xsize, ysize;
   static RECT      rcl;
   static LONG      lHeight, lWidth;


   /* Setup the color pallete */
   rc = sys_setup_palette ();

   /* Set graphics mode */
   SetGraphicsMode (graphics.hdc, GM_ADVANCED);

   /* determine size of the drawing area                    */
   /* if this is to go directly to the screen, then use     */
   /* current screen coordinates                            */
   /* if this is to go to a bitmap first, then write to     */
   /* the full screen and the bitblt will crunch the picture*/
   if (graphics.screen_type == FULLSCREEN) {
      lHeight = GetSystemMetrics (SM_CYFULLSCREEN);
      lWidth  = GetSystemMetrics (SM_CXFULLSCREEN);

      /* Get size of current drawing window */
      rcl.top = 0;
      rcl.left = 0;
      rcl.bottom = GetSystemMetrics (SM_CYFULLSCREEN);
      rcl.right  = GetSystemMetrics (SM_CXFULLSCREEN);

   }
   else
   {
      lHeight = GetDeviceCaps (graphics.hdc, VERTRES);
      lWidth  = GetDeviceCaps (graphics.hdc, HORZRES);

      /* Get size of current drawing window */
      GetClientRect ((HWND) graphics.logicalunit, &rcl);

   }


   /* Set up the device units_per_inch to be a 7" x 5" virtual monitor */
   graphics.extent[0] = (float) rcl.right - rcl.left;
   graphics.extent[1] = (float) rcl.bottom   - rcl.top;

   /* Determine virtual page size */
   if (lWidth > 1000) {
      xsize = 11.0F;
      ysize = 8.5F;
   } else {
      if (lWidth > 650 && lWidth <= 1000) {
        xsize = 7.5F;
        ysize = 6.0F;
      } else {
         xsize = 5.5F;
         ysize = 4.5F;
      } /* endif */
   } /* endif */

   /* Plotter units per inch */
   graphics.units[0] = graphics.extent[0] / xsize;
   graphics.units[1] = graphics.extent[1] / ysize;

//   printf ("graphics.units = %f, %f, extent = %f, %f, xsize = %f, ysize = %f\n",
//           graphics.units[0], graphics.units[1], graphics.extent[0], graphics.extent[1],
//           xsize, ysize);
//   fflush (stdout);

   return;
}


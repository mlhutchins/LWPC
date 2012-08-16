/************************************************************************
  FUNCTION:
     sys_Poly_Fill

  DATE:
     26 March 1993

  LANGUAGE:
     C

  Function:
     Draws a single, closed complex figure and fill the figure with a requested
     fill pattern.

  Parameters Passed:
     num_points     [long]  number of x,y points in figure
     x_points       [float] array of x values in user units
     y_points       [float] array of y values in user units
     x_min          [float] graphical origin in user units
     y_min          [float] graphical origin in user units
     x_scale        [float] scale in user units per inch
     y_scale        [float] scale in user units per inch
     fill_pattern   [long]  the fill pattern index.  Valid indices are from 0 to 12.

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphics

  Functions and Subroutines Referenced:
     GpiBeginArea
     GpiMove
     GpiPolyLine
     GpiEndArea

  Entry:

  References:

  Change History:
      27 Apr 1994   Added scaling parameters to the argument list for
                    compatibility with GRF_CURVE; to allow the same
                    arrays to be used for contour lines and filled
                    contours.

      05 Jul 1995   Added presentation handle to graphics structure

********************!*****************************************************/


#include <windows.h>
#include <math.h>
#include <stdio.h>
#include "graphics.h"
#include "sysstrct.h"


LONG sys_poly_fill (long num_points,
                    float x_array[],
                    float y_array[],
                    float x_min,
                    float y_min,
                    float x_scale,
                    float y_scale,
                    long  fill_pattern)

{
   //static BYTE byBits[] = {0xee,
   //                        0xff,
   //                        0xbb,
   //                        0xff,
   //                        0xed,
   //                        0xff,
   //                        0xbb,
   //                        0xff};

   POINT aptl[MAX_POLY_POINTS];
   HBRUSH hBrush;
   //HBITMAP hBitmap;
   int   i;

//x
//x    int i, GPI_return;
//x
//x    LONG pattern_types [13] = {PATSYM_SOLID,   PATSYM_DENSE1,  PATSYM_DENSE2,
//x                               PATSYM_DENSE3,  PATSYM_DENSE4,  PATSYM_DENSE5,
//x                               PATSYM_DENSE6,  PATSYM_DENSE7,  PATSYM_NOSHADE,
//x                               PATSYM_VERT,    PATSYM_HORIZ,   PATSYM_DIAG1,
//x                               PATSYM_DIAG3};
//x
   float xx;
   float yy;




   if (fill_pattern == 0)
   {
      // Solid request
      SelectObject (graphics.hdc, graphics.hBrush);
      SetPolyFillMode (graphics.hdc, ALTERNATE);
   }
   else
   {
      //pattern request
      // create the bitmap
      hBrush = CreateHatchBrush (HS_VERTICAL, RGB(0,0,0)  );
      SelectObject (graphics.hdc, hBrush);
      SetPolyFillMode (graphics.hdc, ALTERNATE);
      SetBkMode (graphics.hdc, TRANSPARENT);
   }


   if (num_points > MAX_POLY_POINTS)
   {
      sys_error.sys_error_level = 10; // Fatal error level
      strcpy (sys_error.sys_error_msg,"[sys_Poly_Fill]: Number of points exceeds MAX_POLY_POINTS.");
      sys_error_message(sys_error.sys_error_level,sys_error.sys_error_msg); // This terminates program execution
   }

   /* Make sure index is valid - Invalid value gives SOLID */
   if ((fill_pattern < 0) || (fill_pattern > 12))
   {
      fill_pattern = 0;
   }

   /* Put points into proper array and display polygon */
   for (i=0;i<= num_points ;i++ )
   {
      /* scale data to screen */
      xx = (x_array[i]-x_min)/x_scale;
      yy = (y_array[i]-y_min)/y_scale;
      aptl[i].x = (LONG) (graphics.origin[0]+graphics.units[0]*xx);
      aptl[i].y = (LONG) (graphics.origin[1]+graphics.units[1]*yy);
      aptl[i].y = graphics.extent[1] - aptl[i].y;
//      printf ("aptl[%d].x = %d, aptl[%d].y = %d\n", i, aptl[i].x, i, aptl[i].y);
   }


   Polygon (graphics.hdc, aptl, num_points);

   //if (fill_pattern != 0) DeleteObject (hBrush); // must delete brush resource before exiting
   //if (fill_pattern == 0) DeleteObject (graphics.hBrush); // must delete brush resource before exiting

   return 0L;
}

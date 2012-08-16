/************************************************************************
  FUNCTION:
     Sys_fill_rect

  DATE:
     29 Dec 1992

  LANGUAGE:
     C

  Function:
     Draws a rectangle and fills with user specified fill pattern
     and color.

  Parameters Passed:
     x1             [float] value of xLeft position in inches
     y1             [float] value of yBottom position in inches
     x2             [float] value of xRight position in inches
     y2             [float] value of yTop position in inches
     fill_ndx       [int]   index of fill pattern to use;
                            valid values are 0 to 12.

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphics

  Functions and Subroutines Referenced:
     GpiBox
     GpiMove
     GpiSetPattern

  Entry:

  References:

  Change History:
     05 Jul 1995    Added presentation handle to graphics structure

********************!*****************************************************/

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include "graphics.h"

// extern HPS hps;

LONG Sys_fill_rect (float x1, float y1, float x2, float y2, LONG fill_ndx)
{
    int xx1, yy1, xx2, yy2;
    RECT rect;

//x    LONG pattern_types [13] = {PATSYM_SOLID,   PATSYM_DENSE1,  PATSYM_DENSE2,
//x                               PATSYM_DENSE3,  PATSYM_DENSE4,  PATSYM_DENSE5,
//x                               PATSYM_DENSE6,  PATSYM_DENSE7,  PATSYM_NOSHADE,
//x                               PATSYM_VERT,    PATSYM_HORIZ,   PATSYM_DIAG1,
//x                               PATSYM_DIAG3};
//x
    LONG ndx;


    /* Select the requested fill pattern */
    ndx = fill_ndx;

    /* Invalid value gives BLANK */
    if ((ndx < 0) || (ndx > 15))
    {
      ndx = 0;
    }

    /* Move to lower left hand corner of box */
    rect.right = (int) (graphics.origin[0]+graphics.units[0]*x1);
    rect.bottom = (int) (graphics.origin[1]+graphics.units[1]*y1);
    rect.bottom = (int) graphics.extent[1] - rect.bottom;


    /* Draw box from (x1,y1) to (x2,y2) */
    rect.left = (int) (graphics.origin[0]+graphics.units[0]*x2);
    rect.top = (int) (graphics.origin[1]+graphics.units[1]*y2);
    rect.top = (int) graphics.extent[1] - rect.top;

//x    GpiSetPattern (graphics.hps, pattern_types[ndx]);


    /* Draw the requested box */
    FillRect (graphics.hdc, &rect, graphics.hBrush);

	return (0);
}

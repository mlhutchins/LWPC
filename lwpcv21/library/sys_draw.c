/************************************************************************
  FUNCTION:
     sys_draw

  DATE:
     10 September 1991

  LANGUAGE:
     C

  Function:
     Moves the cursor to the specified position while drawing
     a line.

  Parameters Passed:
     xx             [float] value of x position in inches
     yy             [float] value of y position in inches

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphics

  Functions and Subroutines Referenced:
     GpiLine        OS/2 function to move cursor to requested position
                    while drawing a line.

  Entry:

  References:

  Change History:

      05 Jul 1995   Added presentation handle to graphics structure

********************!*****************************************************/

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include "graphics.h"

// extern HPS hps;

LONG sys_draw (float xx, float yy)
{
     int x, y;

    /* Determine end position of line */

    x = (LONG) (graphics.origin[0]+graphics.units[0]*xx);
    y = (LONG) (graphics.extent[1] - (graphics.origin[1]+graphics.units[1]*yy));


    LineTo (graphics.hdc, x, y);
    return (0);

}


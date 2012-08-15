/************************************************************************
  FUNCTION:
     Sys_move

  DATE:
     11 March 1997

  LANGUAGE:
     C

  Function:
     Moves the cursor to the specified position without drawing
     a line.

  Parameters Passed:
     xx             [float] value of x position in inches
     yy             [float] value of y position in inches

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphics

  Functions and Subroutines Referenced:
     MoveToEx       Function to move cursor to requested position
                    without drawing a line.

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

LONG Sys_move (float xx, float yy)
{
    int x, y;

    x = (LONG) (graphics.origin[0]+graphics.units[0]*xx);
    y = (LONG) (graphics.extent[1] - (graphics.origin[1]+graphics.units[1]*yy));
//    y = (int) (y * (graphics.units[0]/graphics.units[1]));
//    printf ("Sys_move: xx = %f, yy = %f, x = %d, y = %d\n", xx, yy, x, y);

    MoveToEx (graphics.hdc, x, y, NULL);
    fflush (stdout);

	return (0);
}


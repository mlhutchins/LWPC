/*************************************************************************
  FUNCTION:
     sys_print_done

  DATE:
     26 June 1997

  LANGUAGE:
     C

  Function:
     Closes the windows printer device opened by call to sys_print_begin

  Parameters Passed:
     None

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphic

  Functions and Subroutines Referenced:

  References:

  Change History:
  April 4, 1998:  Fixed problem where printing would work for all NT
                  printers, but only some Win 9x printers

********************!*****************************************************/

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "graphics.h"

void sys_print_done (void)
{

   EndPage (graphics.hdc);
   EndDoc (graphics.hdc);
   DeleteDC (graphics.hdc);


   /* release resources that we used */
   DeleteObject (graphics.hPen);
   DeleteObject (graphics.hBrush);


}

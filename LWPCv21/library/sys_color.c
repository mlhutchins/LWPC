/************************************************************************
  FUNCTION:
     sys_color

  DATE:
     10 March 1997

  LANGUAGE:
     C

  Function:
    Changes the current drawing color to the specified RGB color

  Parameters Passed:
     RGB_color      [long] index of selected color in RGB format
                           R*65536+G*256*B

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

// extern HPS hps;

LONG sys_color (long RGB1,long RGB2, long RGB3)
{
   //static HPEN hPen;

   /* Delete old pen */
   DeleteObject (graphics.hPen);

   /* Delete old brush */
   DeleteObject (graphics.hBrush);

   /* create colored pen    */
   if ((strnicmp (graphics.device, "sys-p",5) == 0) ||
       (strnicmp (graphics.device, "sys_p",5) == 0))
   {
      /* Use pen width of 3 for hard copy devices */
      graphics.hPen = CreatePen (PS_SOLID, 3, RGB(RGB1, RGB2, RGB3));
   }
   else
   {
      /* Use pen width of 1 for screen device */
      graphics.hPen = CreatePen (PS_SOLID, 1, RGB(RGB1, RGB2, RGB3));
   }
   SelectObject (graphics.hdc, graphics.hPen);

   /* create colored text as well */
   SetTextColor (graphics.hdc, RGB(RGB1, RGB2, RGB3));

   /* create colored solid fills also */
   graphics.hBrush = CreateSolidBrush (RGB (RGB1, RGB2, RGB3));

   return (0);
}


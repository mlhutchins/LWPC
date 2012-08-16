/************************************************************************
  FUNCTION:
     sys_string

  DATE:
     07 march 1997

  LANGUAGE:
     C

  Function:
     Draws the character string at the specified coordinates.


  Parameters Passed:
     xx             [float]  value of x position in inches
     yy             [float]  value of y position in inches
     height         [float]  height of characters, in inches
     char_string    [string] char string to display
     angle          [float] angle to display character string

  Parameters Returned:
     None

  Structures Referenced (FORTRAN common blocks):
     graphics

  Functions and Subroutines Referenced:

  Entry:

  References:

  Change History:


********************!*****************************************************/

#define TWO_PI (2 * 3.14159)
#define DTR 0.01745329

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <sysstrct.h>
#include <graphics.h>


float sys_string (float xx, float yy, float height, char * char_string,
                  float angle, char justify[3])
{

   static int   decipoint;
   static HFONT hFont;
   static float tmp_height, hgt, wth;
   static int   char_height, char_width;
   static float diag, gamma;
   static float delta_x, delta_y;
   static int   x,y;
   static float cxDpi, cyDpi;
   static TEXTMETRIC tm;
   static LOGFONT    lf;
   static float ratio1, ratio2;
   static float rangle;

   SetGraphicsMode (graphics.hdc, GM_ADVANCED);
   ModifyWorldTransform (graphics.hdc, NULL, MWT_IDENTITY);


   x = (int) (graphics.origin[0] + graphics.units[0]*xx);
   y = (int) (graphics.extent[1] - (graphics.origin[1] + graphics.units[1]*yy));

   /* Define the font we want to use for our characters */
   tmp_height = height * 1.23; /* need to account for descender in Windows */
   decipoint = (int) ((tmp_height / 0.01389) * 10.0);

   /* Determine the resolution of the device in dots per inch */
   cxDpi = (FLOAT) (25.4 * GetDeviceCaps (graphics.hdc, HORZRES) /
                    GetDeviceCaps (graphics.hdc, HORZSIZE));
   cyDpi = (FLOAT) (25.4 * GetDeviceCaps (graphics.hdc, VERTRES) /
                    GetDeviceCaps (graphics.hdc, VERTSIZE));

   ratio1 = graphics.units[1] / graphics.units[0];

   /* Determine angle to use to match screen ratio */

   lf.lfHeight = - (int) (fabs (decipoint * cyDpi/72) / 10.0 + 0.5);
   lf.lfWidth  = 0;

   if ((angle >= 0.0) && (angle <=90.0))
   {
      rangle = (atan(tan(angle*DTR)*ratio1))/DTR;
      lf.lfEscapement =  (int) (rangle * 10.0);
      lf.lfOrientation = (int) (rangle * 10.0);
   }
   if ((angle > 90.0) && (angle <=180.0))
   {
      rangle = (atan(tan(angle*DTR)*ratio1))/DTR;
      rangle = 180.0 + rangle;
      lf.lfEscapement =  (int) (rangle * 10.0);
      lf.lfOrientation = (int) (rangle * 10.0);
   }
   if ((angle > 180.0) && (angle <=270.0))
   {
      rangle = (atan(tan(angle*DTR)*ratio1))/DTR;
      rangle = 180.0 + rangle;
      lf.lfEscapement = (int) (rangle * 10.0);
      lf.lfOrientation =(int)  (rangle * 10.0);
   }
   if ((angle > 270.0) && (angle <=360.0))
   {
      rangle = (atan(tan(angle*DTR)*ratio1))/DTR;
      rangle = 360.0 + rangle;
      lf.lfEscapement = (int) (rangle * 10.0);
      lf.lfOrientation =(int) (rangle * 10.0);
   }

   lf.lfWeight = 400;
   lf.lfItalic = 0;
   lf.lfUnderline = 0;
   lf.lfStrikeOut = 0;
   lf.lfCharSet = 0;
   lf.lfOutPrecision = 0;
   lf.lfClipPrecision = 0;
   lf.lfPitchAndFamily = 0;
   strcpy (lf.lfFaceName, "Courier New");

   hFont = CreateFontIndirect (&lf);
   hFont = SelectObject (graphics.hdc, hFont);

   GetTextMetrics (graphics.hdc, &tm);

   char_width = tm.tmAveCharWidth;
   char_height = tm.tmAscent;
   wth = (float) (char_width * strlen(char_string));
   hgt = (float) char_height;

   diag = sqrt (wth*wth + hgt*hgt);
   gamma = atan(hgt / wth) / DTR;

   /* Set background mode to transparent so we don't block any of the picture */
   SetBkMode (graphics.hdc, TRANSPARENT);

   /*******************************************************************/
   /*             Setup the fonts characteristics                     */
   /*******************************************************************/


   /*******************************************************************/
   /*           Perform length justification requested                */
   /*******************************************************************/
   if ((justify[0] == 'C') || (justify[0] == 'c'))
   {
      /* Center justify - about specified x position*/
      x = (int) (x - 0.5*diag*cos((angle-gamma)*DTR) + hgt*sin(angle*DTR));
   }
   if ((justify[0] == 'R') || (justify[0] == 'r'))
   {
      /* Right justify */
      x = (int) (x - diag*cos((angle-gamma)*DTR) + hgt*sin(angle*DTR));
   }

   if ((justify[0] == 'L') || (justify[0] == 'l'))
   {
      /* Left justify */
      /* do nothing */
   }

   /*******************************************************************/
   /*          Preform Height Justification                           */
   /*******************************************************************/
   if ((justify[1] == 'C') || (justify[1] == 'c'))
   {
      /* Center justify - about specified y position*/
      y = (int)(y + 0.5*diag*sin((angle-gamma)*DTR) +hgt*cos(angle*DTR));
   }
   if ((justify[1] == 'T') || (justify[1] == 't'))
   {
      /* Top justify */
      y = (int)(y + diag*sin((angle-gamma)*DTR) +2.0*hgt*cos(angle*DTR));
   }

   if ((justify[1] == 'B') || (justify[1] == 'B'))
   {
      /* Bottom justify */
      /* do nothing */
   }


   delta_x = 0.0;
   delta_y = 0.0;
   /**********************************************************/
   /*       Now, adjust for character height/width deltas    */
   /**********************************************************/
   if ((angle >=0.0) && (angle <90.0))
   {
      delta_x = sin(angle*DTR) * tm.tmMaxCharWidth;
      delta_y = cos(angle*DTR) * tm.tmHeight-tm.tmInternalLeading;
   }
   if (angle == 90.0)
   {
      delta_x = sin(angle*DTR) * (tm.tmHeight-tm.tmInternalLeading);
      delta_y = cos(angle*DTR) * tm.tmAscent-(tm.tmInternalLeading + tm.tmDescent);
   }
   if ((angle >90.0) && (angle <=180.0))
   {
      delta_x = sin(angle*DTR) * tm.tmMaxCharWidth;
      delta_y = cos(angle*DTR) * tm.tmHeight+tm.tmDescent;
   }
   if ((angle >180.0) && (angle <270.0))
   {
      delta_x = sin(angle*DTR) * tm.tmMaxCharWidth;
      delta_y = cos(angle*DTR) * tm.tmHeight+tm.tmDescent;
   }
   if ((angle >=270.0) && (angle <360.0))
   {
      delta_x = sin(angle*DTR) * tm.tmMaxCharWidth;
      delta_y = cos(angle*DTR) * tm.tmHeight-tm.tmInternalLeading;
   }

   x = x - (int)delta_x;
   y = y - (int)delta_y;
   y = y + tm.tmDescent-tm.tmInternalLeading;



   /********************************************************************/
   /*               Output the requested text string                   */
   /********************************************************************/
   TextOut (graphics.hdc, x, y, char_string, strlen(char_string));


   /********************************************************************/
   /*          Release the fonts memory and clean things up            */
   /********************************************************************/
   DeleteObject (SelectObject (graphics.hdc, GetStockObject(SYSTEM_FONT)));
   SetBkMode (graphics.hdc, OPAQUE);

   return ((float)wth/graphics.units[1]);
}



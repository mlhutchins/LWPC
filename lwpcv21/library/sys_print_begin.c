/*************************************************************************
  FUNCTION:
     sys_print_begin

  DATE:
     26 June 1997

  LANGUAGE:
     C

  Function:
     Initializes variables used in plotting to a windows printer device.

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


//#define DEFINE_PRINTGRF

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "graphics.h"
#include "sysstrct.h"

void sys_print_begin (void)
{

   static HDC       InfoHDC;
   static float     xsize, ysize;
   static RECT      rcl;
   static int       lHeight, lWidth;
   static int       rc;
   static DOCINFO   di = {sizeof (DOCINFO), "NRaD Printing", NULL};
   static char      szPrinter[80];
   static char      *szDevice, *szDriver, *szOutput;
   static PRINTDLG  printDlg;
   static LPDEVMODE deviceMode;


   /* Setup the system pallete */
   //rc = sys_setup_palette ();

   /* Select a default brush */
   graphics.hBrush = CreateSolidBrush (RGB (0, 0, 0));

   /* Select a default pen */
   graphics.hPen = CreatePen (PS_SOLID, 1, RGB (0,0,0));

   /* Get the name of the default printer from the windows profile */
   memset (&printDlg, 0, sizeof (PRINTDLG));
   printDlg.lStructSize = sizeof (PRINTDLG);
   printDlg.Flags = PD_RETURNDC | PD_RETURNDEFAULT;
   printDlg.hwndOwner = 0;
   PrintDlg (&printDlg);

   /* Store the handle to the printers device context in the graphics handle DC */
   graphics.hdc = printDlg.hDC;


   if (graphics.hdc == NULL) {
      /* If we cant open the default printer display a message and return */
      sys_error_message ("E","A handle to the printer device could not be obtained");
   }

   /* Call StartDoc to start printing system */
   StartDoc (graphics.hdc, &di);

   /* Set printer to the requested orientation */
   deviceMode = (DEVMODE *) GlobalLock (printDlg.hDevMode);
   if (graphics.landscape)
   {
        deviceMode->dmOrientation = DMORIENT_LANDSCAPE;
   }
   else
   {
      deviceMode->dmOrientation = DMORIENT_PORTRAIT;
   }
   ResetDC (printDlg.hDC, deviceMode);
   GlobalUnlock (printDlg.hDevMode);
   StartPage (graphics.hdc);

   /* Get the physical extents for the selected printer in printer units */
   lHeight = GetDeviceCaps (graphics.hdc, VERTRES);
   lWidth  = GetDeviceCaps (graphics.hdc, HORZRES);


   graphics.extent[0] = (float) lWidth;
   graphics.extent[1] = (float) lHeight;


   /* Set size of paper to 8.5 x 11 inches */
   xsize = 11.0F;
   ysize = 8.5F;


   /* Plotter units per inch */
   graphics.units[0] = graphics.extent[0] / xsize;
   graphics.units[1] = graphics.extent[1] / ysize;



   // Debug printing
   //printf ("graphics.units = %f, %f, extent = %f, %f, xsize = %f, ysize = %f\n",
   //        graphics.units[0], graphics.units[1], graphics.extent[0], graphics.extent[1],
   //        xsize, ysize);
   //fflush (stdout);

    return;
}


#include <windows.h>
#include <stdlib.h>
#include "sysstrct.h"
#include "graphics.h"

void sys_error_message (void)
{
  char errormessage [201];
  ULONG temp;
  float ftemp, ftemp2;
  int *null_pointer;

  ftemp2 = 0.0;
  temp = graphics.logicalunit; // need to save lu, since call to
                                // WinMessageBox seems to set it to hwnd!!!

  if (sys_error.sys_error_level == 0)
  {
    sprintf(errormessage,"%s\n", sys_error.sys_error_msg);
    MessageBox ((HWND) sys_error.hwnd, &errormessage, sys_error.sys_szClientClass, MB_OK);
    return;
  }
  else
  {
    sprintf(errormessage,"Fatal Error: %s\n", sys_error.sys_error_msg);
    MessageBox ((HWND)sys_error.hwnd, &errormessage, sys_error.sys_szClientClass, MB_OK);
    /* lets generate a fatal error to display a traceback stack */
    ftemp = ftemp2 / *null_pointer;
    exit (10);
  }
}

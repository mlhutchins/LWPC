/********************************************************************/
/********************************************************************/
/* GRFDriver.c and FORDRIVE mixed language header definitions          */
/********************************************************************/
/********************************************************************/

#define MAX_ARGS 10
#define MAX_CHARS 20
#define MAX_ERROR_MSG 200

#pragma aux c_2_for "^";

extern struct c_2_for_strct
{
  char arglist [MAX_ARGS * MAX_CHARS + 1];
} c_2_for;

/* Structure definition for handling error conditions in both FORTRAN
   and windows graphic programs. */

#pragma aux sys_error "^";

extern struct sys_error_strct
{
  char  sys_error_msg [MAX_ERROR_MSG]; // contains error message string
  ULONG sys_error_level;               // contains error severity level
  ULONG hwnd;                         // the handle to the parent window
  char  sys_szClientClass [20];        // Windows client class
  char  program_type [8];             // Program type
  int   next_graph;                   // picture handling var:
                                      // 0 == first time in graphics routine
                                      // 1 == redraw current graph
                                      // 2 == draw next graph
} sys_error;


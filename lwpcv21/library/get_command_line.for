      SUBROUTINE GET_COMMAND_LINE
     &          (LenCMD,CMDLine)

c***********************************************************************
c                         subroutine get_command_line
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     04 Dec 1996

c  Function:
c     Returns current command line used to run the program

c  Parameters passed:
c     none

c  Parameters returned:
c     LenCMD           [i] length of the command line
c     cmdline          [s] the command line

c  Common blocks referenced:

c  Functions and subroutines referenced:

c     fgetcmd

c     getarg

c     iargc


c  References:

c  Change History:
c 2/16/10 MLH Added UNIX specific code get_command
c 2/17/10 MLH We actually just want the first arguement

c*******************!***************************************************

c     WATCOM specific code
c      integer       FGETCMD

      integer       LenCMD
      
      character*120 CMDLine

c     SUN SOLARIS specific code

c      character *60 argv(2)
c     GNU Gfortran code

      CALL GETARG(1,CMDLine)
      LenCMD = LEN_TRIM(CMDLine)
      
c     WATCOM specific code
c      LenCMD=FGETCMD (CMDLine)
c     WATCOM specific code

c     SUN SOLARIS specific code

c      n=IARGC ()
c      do i=1,n
c        call GETARG (i,argv)
c      end do
c      CMDLine=argv(1)
c     LenCMD=n
c     SUN SOLARIS specific code

      RETURN
      END      ! GET_COMMAND_LINE
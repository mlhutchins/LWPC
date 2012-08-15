      SUBROUTINE GET_DELIMITER
     &          (delimiter)

c***********************************************************************
c                   subroutine get_delimiter
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center

c  Date:
c     16 Dec 1996

c  Function:
c     Returns system dependent delimiters for file names;
c     format of file names is assumed to be of the form:

c     VMS:
c        drive:[directory]root_name.extension
c     or
c     DOS & OS/2:
c        drive:\directory\root_name.extension
c     or
c     Unix:
c        /directory/root_name.extension

c  Parameters passed:

c  Parameters returned:
c     delimiter     [c,1] delimiters:
c                         1: drive delimiter
c                         2: left  delimiter for directory specification
c                         3: right delimiter for directory specification

c  Common blocks referenced:

c  Functions and subroutines referenced:

c  References:

c  Change history:
c 2/17/10 MLH Changed to Unix delimiters

c*******************!***************************************************

      character*  1 delimiter(3)


c     Delimiters for VMS:
c     delimiter(1)=':'
c     delimiter(2)='['
c     delimiter(3)=']'

c     Delimiters for DOS & OS/2:
c      delimiter(1)=':'
c      delimiter(2)='\'
c      delimiter(3)='\'

c     Delimiters for Unix:
      delimiter(1)=' '
      delimiter(2)='/'
      delimiter(3)='/'

      RETURN
      END      ! GET_DELIMITER
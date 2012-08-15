      SUBROUTINE HPGL_DONE

c***********************************************************************

c  Function:
c     Terminate plot to HPGL device.

c  Parameters passed:

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     close
c     write

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

c     Put pen away.
      write(graphics_LogicalUnit,'('' sp;'')')

c     Close output file
      close(graphics_LogicalUnit)

      RETURN
      END      ! HPGL_DONE
c$pragma aux Sys_move "*_" parm (value)

      SUBROUTINE GRF_MOVE
     &          (xx,yy)

c***********************************************************************

c  Function:
c     Moves pen/cursor to new position without leaving a line

c  Parameters passed:
c     xx            [r] value of x position in inches
c     yy            [r] value of y position in inches

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     sys_move

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      integer       sys_move

c     New position
      if (graphics_device(1:3) .eq. 'sys')

     &   iresult=SYS_MOVE(xx,yy)

c     Update position array

      graphics_position(1)=xx
      graphics_position(2)=yy

c     Set pen status
      graphics_pen='up'

      RETURN
      END      ! GRF_MOVE
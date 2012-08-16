c$pragma aux Sys_draw "*_" parm (value)

      SUBROUTINE GRF_DRAW
     &          (xx,yy)

c***********************************************************************
c     23 Mar 1990

c  Function:
c     Moves pen/cursor to new position while leaving a line

c  Parameters passed:
c     xx            [r] value of x position in inches
c     yy            [r] value of y position in inches

c  Parameters returned:
c     origin        [r] coordinates of current  logical origin
c                       relative to physical origin

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     hpgl_draw
c     sys_draw

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      integer       sys_draw

      if (graphics_device(1:3) .eq. 'sys') then

         iresult=SYS_DRAW (xx,yy)
      else

         call HPGL_DRAW (xx,yy)
      end if

c     Store current position as the desired end point
      graphics_position(1)=xx
      graphics_position(2)=yy

c     Set pen status
      graphics_pen='down'

      RETURN
      END      ! GRF_DRAW
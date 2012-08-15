c$pragma aux Sys_fill_rect "*_" parm (value)

      SUBROUTINE GRF_FILL_RECT
     &          (x1,y1,x2,y2,fill_name)

c***********************************************************************

c  Function:
c     Draws a rectangle using specified fill name

c  Parameters passed:
c     x1            [r] X of lower left  corner of rectangle
c     y1            [r] Y of lower left  corner of rectangle
c     x2            [r] X of upper right corner of rectangle
c     y2            [r] Y of upper right corner of rectangle
c     fill_name     [s] fill pattern ( % shading )

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     grf_fill_index
c     hpgl_fill_rect
c     sys_fill_rect

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      character*(*) fill_name

      integer       fill_index
      integer       sys_fill_rect

c     Determine the fill pattern
      call GRF_FILL_INDEX (fill_name,fill_index)

      if (graphics_device(1:3) .eq. 'sys') then

         iresult=SYS_FILL_RECT (x1,y1,x2,y2,fill_index)
      else

         x1a=graphics_origin(1)+graphics_units(1)*x1
         y1a=graphics_origin(2)+graphics_units(2)*y1

         x2a=graphics_origin(1)+graphics_units(1)*x2
         y2a=graphics_origin(2)+graphics_units(2)*y2

         call HPGL_FILL_RECT (x1a,y1a,x2a,y2a,fill_index)
      end if

c     Store current fill
      graphics_fillname=fill_name
      graphics_fillindx=fill_index

      RETURN
      END      ! GRF_FILL_RECT
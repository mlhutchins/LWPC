c$pragma aux Sys_poly_fill "*_" parm (value)

      SUBROUTINE GRF_POLY_FILL
     &          (nrpts,xarray,yarray,xmin,ymin,xscale,yscale,fill_name)

c***********************************************************************

c  Function:
c     Draws a polygon and fills it with the specified fill name; the
c     calling routine must ensure that the polygon is closed.

c  Parameters passed:
c     nrpts         [i] number of points in arrays defining the polygon
c     xarray        [r] x coordinates of the polygon [user units]
c     yarray        [r] y coordinates of the polygon [user units]
c     xmin          [r] graphical origin [user units]
c     ymin          [r] graphical origin [user units]
c     xscale        [r] x scale [user units per inch]
c     yscale        [r] y scale [user units per inch]
c     fill_name     [s] fill pattern ( % shading )

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     grf_fill_index
c     hpgl_poly_fill
c     sys_poly_fill

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      character*(*) fill_name
      real          xarray(1),yarray(1),xmin,ymin,xscale,yscale

      integer       fill_index

c     Determine the fill pattern
      call GRF_FILL_INDEX (fill_name,fill_index)

      if (graphics_device(1:3) .eq. 'sys') then

         call SYS_POLY_FILL
     &       (nrpts,xarray,yarray,xmin,ymin,xscale,yscale,fill_index)
      else

         call HPGL_POLY_FILL
     &       (nrpts,xarray,yarray,xmin,ymin,xscale,yscale,fill_index)
      end if

c     Move to the first point of the polygon
      xx=(xarray(1)-xmin)/xscale
      yy=(yarray(1)-ymin)/yscale
      call GRF_MOVE (xx,yy)

c     Store current fill
      graphics_fillname=fill_name
      graphics_fillindx=fill_index

      RETURN
      END      ! GRF_POLY_FILL
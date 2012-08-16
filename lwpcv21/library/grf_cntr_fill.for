      SUBROUTINE GRF_CNTR_FILL
     &          (xmin,xscale,ymin,yscale,
     &           fill,color)

c***********************************************************************

c  Function:
c     This routine fills contour bands found by GRF_CNTR.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     xmin         [r]           minimum value along X axis
c     xscale       [r]           scale         along X axis
c     ymin         [r]           minimum value along Y axis
c     yscale       [r]           scale         along Y axis
c     fill         [c]           name of fill or percentage to use
c     color        [c]           name or RGB sequence of color to use

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     grf_cntr_join
c     grf_color
c     grf_poly_fill

c  References:

c  Change History:

c*******************!***************************************************

      IMPLICIT      NONE

      character*(*) color
      character*(*) fill

      include      'grf_cntr.cmn'

      real          xmin
      real          xscale
      real          ymin
      real          yscale

      if (nrpts(nrholes) .lt. 3) RETURN

      call GRF_COLOR (color)

c     Join the separate curves into a single polygon

      call GRF_CNTR_JOIN

c     Now, fill the polygon

      call GRF_POLY_FILL
     &    (nrcntrp,cntrpx,cntrpy,
     &     xmin,ymin,xscale,yscale,fill)

      RETURN
      END      ! GRF_CNTR_FILL
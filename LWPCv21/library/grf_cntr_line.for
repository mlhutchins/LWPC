      SUBROUTINE GRF_CNTR_LINE
     &          (xmin,xscale,ymin,yscale,
     &           line,color)

c***********************************************************************

c  Function:
c     This routine draws contour lines found by GRF_CNTR.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     xmin         [r]           minimum value along X axis
c     xscale       [r]           scale         along X axis
c     ymin         [r]           minimum value along Y axis
c     yscale       [r]           scale         along Y axis
c     line         [i]           line type to use
c     color        [c]           name or RGB sequence of color to use

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     grf_color
c     grf_curve

c  References:

c  Change History:

c*******************!***************************************************

      IMPLICIT      NONE

      character*(*) color

      include      'grf_cntr.cmn'

      integer       line
      integer       n1
      integer       nh
      integer       nrp

      real          xmin
      real          xscale
      real          ymin
      real          yscale

      if (nrpts(nrholes) .lt. 3) RETURN

c     Set the color
      call GRF_COLOR (color)

c     Draw curve for each contour
      n1=1
      do nh=0,nrholes

         nrp=nrpts(nh)-n1+1

         call GRF_CURVE
     &       (cntrpx(n1),cntrpy(n1),nrp,
     &        xmin,ymin,xscale,yscale,line,20)

         n1=nrpts(nh)+1
      end do
      RETURN
      END      ! GRF_CNTR_LINE
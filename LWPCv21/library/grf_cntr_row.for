      SUBROUTINE GRF_CNTR_ROW
     &          (v,mxx,mxy,nrx,
     &           vc,nx1,nx2,
     &           nx,ny,nrc,nc,xc,yc)

c***********************************************************************

c  Function:
c     This routine searches a row of the contour area in the
c     direction specified to find the next crossing of at least one of
c     the contour levels specified in VC.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nrx          [i]           number of elements of V along X
c     vc           [r,2]         definition of the contour band
c     nx1          [i]           starting index along the row
c     nx2          [i]           ending   index along the row
c     ny           [i]           index of the row along the Y axis

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}
c     nx           [i]           X index of the cell where a crossing is
c                                found
c     nrc          [i]           number of crossings found; =0 if none
c     nc           [i,2]         index of the contour level found; =1 if
c                                VC(1), =2 if VC(2)
c     xc           [r,2]         X coordinate of the crossing
c     yc           [r,2]         Y coordinate of the crossing

c  Common blocks referenced:

c  Functions and subroutines referenced:

c  References:

c  Change History:

c*******************!***************************************************

      IMPLICIT      NONE

      integer       inc
      integer       ix1
      integer       ix2
      integer       mxx
      integer       mxy
      integer       nc
      integer       nrc
      integer       nrx
      integer       ntemp
      integer       nx
      integer       nx1
      integer       nx2
      integer       ny

      real          v
      real          vc
      real          xc
      real          xtemp
      real          yc

      dimension     nc(2),
     &              v(mxx,mxy),
     &              vc(2),
     &              xc(2),
     &              yc(2)

c     Set up counters
      if (nx1 .lt. nx2) then

c        Search from left to right
         ix1=nx1
         if (nx2 .eq. nrx) then
            ix2=nrx-1
         else
            ix2=nx2
         end if
         inc=1
      else

c        Search from right to left
         if (nx1 .eq. nrx) then
            ix1=nx1-1
         else
            ix1=nx1
         end if
         ix2=nx2
         inc=-1
      end if

c     Starting position
      nx=ix1
      nrc=0

c     Search row for crossing of VC
11    if ((v(nx,ny) .lt. vc(1) .and. vc(1) .le. v(nx+1,ny)) .or.
     &    (v(nx,ny) .ge. vc(1) .and. vc(1) .gt. v(nx+1,ny)) .or.
     &    (v(nx,ny) .le. vc(1) .and. vc(1) .lt. v(nx+1,ny)) .or.
     &    (v(nx,ny) .gt. vc(1) .and. vc(1) .ge. v(nx+1,ny))) then

c        Found a crossing; interpolate the location
         nrc=nrc+1
         nc (nrc)=1
         xc (nrc)=nx+(vc(1)-v(nx,ny))/(v(nx+1,ny)-v(nx,ny))
         yc (nrc)=ny
      end if
      if (vc(1) .ne. vc(2)) then
         if ((v(nx,ny) .lt. vc(2) .and. vc(2) .le. v(nx+1,ny)) .or.
     &       (v(nx,ny) .ge. vc(2) .and. vc(2) .gt. v(nx+1,ny)) .or.
     &       (v(nx,ny) .le. vc(2) .and. vc(2) .lt. v(nx+1,ny)) .or.
     &       (v(nx,ny) .gt. vc(2) .and. vc(2) .ge. v(nx+1,ny))) then

c           Found a crossing; interpolate the location
            nrc=nrc+1
            nc (nrc)=2
            xc (nrc)=nx+(vc(2)-v(nx,ny))/(v(nx+1,ny)-v(nx,ny))
            yc (nrc)=ny
         end if
      end if

      if (nrc .eq. 0) then

c        No crossing in this cell
         if (nx .eq. ix2) then

c           At the end of the data and there has been no crossing
            RETURN
         else

c           Continue the search
            nx=nx+inc
            go to 11
         end if
      end if

c     Check for two crossings
      if (nrc .eq. 2) then

c        Put the crossings in order
         if (inc .gt. 0 .and. xc(1) .gt. xc(2) .or.
     &       inc .lt. 0 .and. xc(1) .lt. xc(2)) then

            ntemp=nc(2)
            nc(2)=nc(1)
            nc(1)=ntemp
            xtemp=xc(2)
            xc(2)=xc(1)
            xc(1)=xtemp
         end if
      end if
      RETURN
      END      ! GRF_CNTR_ROW
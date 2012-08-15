      SUBROUTINE GRF_CNTR_COL
     &          (v,mxx,mxy,nry,
     &           vc,ny1,ny2,
     &           nx,ny,nrc,nc,xc,yc)

c***********************************************************************

c  Function:
c     This routine searches a column of the contour area in the
c     direction specified to find the next crossing of at least one of
c     the contour levels specified in VC.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nry          [i]           number of elements of V along Y
c     vc           [r,2]         definition of the contour band
c     ny1          [i]           starting index along the column
c     ny2          [i]           ending   index along the column
c     nx           [i]           index of the column along the X axis

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}
c     ny           [i]           Y index of the cell where a crossing is
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
      integer       iy1
      integer       iy2
      integer       mxx
      integer       mxy
      integer       nc
      integer       nrc
      integer       nry
      integer       ntemp
      integer       nx
      integer       ny
      integer       ny1
      integer       ny2

      real          v
      real          vc
      real          xc
      real          yc
      real          ytemp

      dimension     nc(2),
     &              v(mxx,mxy),
     &              vc(2),
     &              xc(2),
     &              yc(2)

c     Set up counters
      if (ny1 .lt. ny2) then

c        Search from bottom to top
         iy1=ny1
         if (ny2 .eq. nry) then
            iy2=nry-1
         else
            iy2=ny2
         end if
         inc=1
      else

c        Search from top to bottom
         if (ny1 .eq. nry) then
            iy1=ny1-1
         else
            iy1=ny1
         end if
         iy2=ny2
         inc=-1
      end if

c     Starting position
      ny=iy1
      nrc=0

c     Search column for crossing of VC1 and VC2
11    if ((v(nx,ny) .lt. vc(1) .and. vc(1) .le. v(nx,ny+1)) .or.
     &    (v(nx,ny) .ge. vc(1) .and. vc(1) .gt. v(nx,ny+1)) .or.
     &    (v(nx,ny) .le. vc(1) .and. vc(1) .lt. v(nx,ny+1)) .or.
     &    (v(nx,ny) .gt. vc(1) .and. vc(1) .ge. v(nx,ny+1))) then

c        Found a crossing; interpolate the location
         nrc=1
         nc (1)=1
         xc (1)=nx
         yc (1)=ny+(vc(1)-v(nx,ny))/(v(nx,ny+1)-v(nx,ny))
      end if
      if (vc(1) .ne. vc(2)) then
         if ((v(nx,ny) .lt. vc(2) .and. vc(2) .le. v(nx,ny+1)) .or.
     &       (v(nx,ny) .ge. vc(2) .and. vc(2) .gt. v(nx,ny+1)) .or.
     &       (v(nx,ny) .le. vc(2) .and. vc(2) .lt. v(nx,ny+1)) .or.
     &       (v(nx,ny) .gt. vc(2) .and. vc(2) .ge. v(nx,ny+1))) then

c           Found a crossing; interpolate the location
            nrc=nrc+1
            nc (nrc)=2
            xc (nrc)=nx
            yc (nrc)=ny+(vc(2)-v(nx,ny))/(v(nx,ny+1)-v(nx,ny))
         end if
      end if

      if (nrc .eq. 0) then

c        No crossing in this cell
         if (ny .eq. iy2) then

c           At the end of the data and there has been no crossing
            RETURN
         else

c           Continue the search
            ny=ny+inc
            go to 11
         end if
      end if

c     Check for two crossings
      if (nrc .eq. 2) then

c        Put the crossings in order
         if (inc .gt. 0 .and. yc(1) .gt. yc(2) .or.
     &       inc .lt. 0 .and. yc(1) .lt. yc(2)) then

            ntemp=nc(2)
            nc(2)=nc(1)
            nc(1)=ntemp
            ytemp=yc(2)
            yc(2)=yc(1)
            yc(1)=ytemp
         end if
      end if
      RETURN
      END      ! GRF_CNTR_COL
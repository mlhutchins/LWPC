      SUBROUTINE GRF_CNTR_TRACE
     &          (v,mxx,mxy,nrx,nry,vc,
     &           nc,edge,nx,ny,xc,yc,source)

c***********************************************************************

c  Function:
c     This routine searches input array to find contour lines.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nrx          [i]           number of elements of V along X
c     nry          [i]           number of elements of V along Y
c     vc           [r,2]         definition of the contour band
c     nc           [i]           index of the contour level
c     edge         [i]           parameters of starting cell
c     nx           [i]
c     ny           [i]
c     xc           [r]
c     yc           [r]
c     source       [s]           calling routine [edge, interior, holes]

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     grf_cntr_cell
c     grf_cntr_edge
c     grf_cntr_flag
c     grf_cntr_search

c  References:

c  Change History:

c*******************!***************************************************

      include      'grf_cntr.cmn'
      include      'lwpc_lun.cmn'

      character*(*) source
      character* 10 label

      logical       bit
      logical       ccw
      logical       grf_cntr_flag      ! Function
      logical       new_curve
      logical       set

      integer       edge
      integer       ex
      integer       i
      integer       j
      integer       mxx
      integer       mxy
      integer       nc0
      integer       nex
      integer       nrp0
      integer       nrx
      integer       nry
      integer       nx
      integer       nx0
      integer       ny
      integer       ny0
      integer       ny1

      real          v(mxx,mxy)
      real          vc(2)
      real          xc
      real          xc0
      real          xc1
      real          xen
      real          xex
      real          yc
      real          yc0
      real          yc1
      real          yen
      real          yex

      data          set/.true./

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10,1x,a)') 'Trace',source

c     Store the indices of the first point
      nc0=nc
      nx0=nx
      ny0=ny
      xc0=xc
      yc0=yc

      nrp0=nrcntrp+1

      xen=xc
      yen=yc

      new_curve=.false.

c     Trace the contour
11    nx1=nx
      ny1=ny
      xc1=xc
      yc1=yc
      call GRF_CNTR_SEARCH
     &    (v,mxx,mxy,nrx,nry,vc,
     &     nc,xen,yen,nx,ny,edge)

c     Check if we have returned to the starting point
      if (nx .ne. nx1 .or. xc .ne. xc1 .or.
     &    ny .ne. ny1 .or. yc .ne. yc1) then

c        Check if we have come to an edge
         if ((ny .eq.  1  .and. edge .eq. 1) .or.
     &       (nx .eq. nrx .and. edge .eq. 2) .or.
     &       (ny .eq. nry .and. edge .eq. 3) .or.
     &       (nx .eq.  1  .and. edge .eq. 4)) then

c           Find out which way to go to continue the contour
            if (edge .eq. 1) then
               if (nx .eq. 1) then
                  if ((nc .eq. 1 .and.
     &                 v(nx+1,ny) .gt. v(nx,ny)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx+1,ny) .lt. v(nx,ny))) then
                     ccw=.false.
                  else
                     ccw=.true.
                  end if
               else
     &         if (nx .eq. nrx) then
                  if ((nc .eq. 1 .and.
     &                 v(nx,ny) .gt. v(nx-1,ny)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx,ny) .lt. v(nx-1,ny))) then
                     ccw=.false.
                  else
                     ccw=.true.
                  end if
               else
                  if (v(nx+1,ny) .ne. v(nx,ny)) then
                     if ((nc .eq. 1 .and.
     &                    v(nx+1,ny) .gt. v(nx,ny)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx+1,ny) .lt. v(nx,ny))) then
                        ccw=.false.
                     else
                        ccw=.true.
                     end if
                  else
                     if ((nc .eq. 1 .and.
     &                    v(nx,ny) .gt. v(nx-1,ny)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx,ny) .lt. v(nx-1,ny))) then
                        ccw=.false.
                     else
                        ccw=.true.
                     end if
                  end if
               end if
            else
     &      if (edge .eq. 2) then
               if (ny .eq. 1) then
                  if ((nc .eq. 1 .and.
     &                 v(nx,ny+1) .gt. v(nx,ny)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx,ny+1) .lt. v(nx,ny))) then
                     ccw=.false.
                  else
                     ccw=.true.
                  end if
               else
     &         if (ny .eq. nry) then
                  if ((nc .eq. 1 .and.
     &                 v(nx,ny) .gt. v(nx,ny-1)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx,ny) .lt. v(nx,ny-1))) then
                     ccw=.false.
                  else
                     ccw=.true.
                  end if
               else
                  if (v(nx,ny+1) .ne. v(nx,ny)) then
                     if ((nc .eq. 1 .and.
     &                    v(nx,ny+1) .gt. v(nx,ny)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx,ny+1) .lt. v(nx,ny))) then
                        ccw=.false.
                     else
                        ccw=.true.
                     end if
                  else
                     if ((nc .eq. 1 .and.
     &                    v(nx,ny) .gt. v(nx,ny-1)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx,ny) .lt. v(nx,ny-1))) then
                        ccw=.false.
                     else
                        ccw=.true.
                     end if
                  end if
               end if
            else
     &      if (edge .eq. 3) then
               if (nx .eq. 1) then
                  if ((nc .eq. 1 .and.
     &                 v(nx+1,ny) .gt. v(nx,ny)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx+1,ny) .lt. v(nx,ny))) then
                     ccw=.true.
                  else
                     ccw=.false.
                  end if
               else
     &         if (nx .eq. nrx) then
                  if ((nc .eq. 1 .and.
     &                 v(nx,ny) .gt. v(nx-1,ny)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx,ny) .lt. v(nx-1,ny))) then
                     ccw=.true.
                  else
                     ccw=.false.
                  end if
               else
                  if (v(nx+1,ny) .ne. v(nx,ny)) then
                     if ((nc .eq. 1 .and.
     &                    v(nx+1,ny) .gt. v(nx,ny)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx+1,ny) .lt. v(nx,ny))) then
                        ccw=.true.
                     else
                        ccw=.false.
                     end if
                  else
                     if ((nc .eq. 1 .and.
     &                    v(nx,ny) .gt. v(nx-1,ny)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx,ny) .lt. v(nx-1,ny))) then
                        ccw=.true.
                     else
                        ccw=.false.
                     end if
                  end if
               end if
            else
c              (edge .eq. 4)
               if (ny .eq. 1) then
                  if ((nc .eq. 1 .and.
     &                 v(nx,ny+1) .gt. v(nx,ny)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx,ny+1) .lt. v(nx,ny))) then
                     ccw=.true.
                  else
                     ccw=.false.
                  end if
               else
     &         if (ny .eq. nry) then
                  if ((nc .eq. 1 .and.
     &                 v(nx,ny) .gt. v(nx,ny-1)) .or.
     &                (nc .eq. 2 .and.
     &                 v(nx,ny) .lt. v(nx,ny-1))) then
                     ccw=.true.
                  else
                     ccw=.false.
                  end if
               else
                  if (v(nx,ny+1) .ne. v(nx,ny)) then
                     if ((nc .eq. 1 .and.
     &                    v(nx,ny+1) .gt. v(nx,ny)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx,ny+1) .lt. v(nx,ny))) then
                        ccw=.true.
                     else
                        ccw=.false.
                     end if
                  else
                     if ((nc .eq. 1 .and.
     &                    v(nx,ny) .gt. v(nx,ny-1)) .or.
     &                   (nc .eq. 2 .and.
     &                    v(nx,ny) .lt. v(nx,ny-1))) then
                        ccw=.true.
                     else
                        ccw=.false.
                     end if
                  end if
               end if
            end if

c           Search along the edge to find the continuation of the line
15          call GRF_CNTR_EDGE
     &          (v,mxx,mxy,nrx,nry,vc,
     &           nc,xc,yc,nx,ny,edge,
     &           ccw,new_curve)

c           Store point at the exit edge
            nrcntrp=nrcntrp+1
            cntrpx (nrcntrp)=xc
            cntrpy (nrcntrp)=yc

c           Check if we've returned to the starting point
            if (nc .ne. nc0 .or.
     &          nx .ne. nx0 .or. ny .ne. ny0 .or.
     &          xc .ne. xc0 .or. yc .ne. yc0) then

c              Find out if there is an exit
               xen=xc
               yen=yc

c              Adjust indices so that the lower left
c              corner of the current cell is consistent
c              with the numbering scheme for the edges
               i=nx
               j=ny
               if (edge .eq. 2) i=i-1
               if (edge .eq. 3) j=j-1

               call GRF_CNTR_CELL
     &             (v,mxx,mxy,nrx,vc,nc,
     &              edge,xen,yen,i,j,
     &              nex,ex,xex,yex,label)

               if (nex .gt. 0) go to 11

c              No exits;
c              tag this cell so it won't be used again
               bit=GRF_CNTR_FLAG
     &            (cells(1,nc),nrx,set,i,j,edge)

               nex=edge
               if (edge .eq. 2) nex=4
               if (edge .eq. 3) nex=1
               bit=GRF_CNTR_FLAG
     &            (cells(1,nc),nrx,set,nx,ny,nex)

               if (edge .eq. 1) then
                  if (ccw) then
                     if (nx .lt. nrx) nx=nx+1
                  else
                     if (nx .gt.   1) nx=nx-1
                  end if
               else
     &         if (edge .eq. 2) then
                  if (ccw) then
                     if (ny .lt. nry) ny=ny+1
                  else
                     if (ny .gt.   1) ny=ny-1
                  end if
               else
     &         if (edge .eq. 3) then
                  if (ccw) then
                     if (nx .gt.   1) nx=nx-1
                  else
                     if (nx .lt. nrx) nx=nx+1
                  end if
               else
     &         if (edge .eq. 4) then
                  if (ccw) then
                     if (ny .gt.   1) ny=ny-1
                  else
                     if (ny .lt. nry) ny=ny+1
                  end if
               end if
               if (edge .eq. 1) then
                  if (ccw) then
                     if (nx .eq. nrx) edge=2
                  else
                     if (nx .eq.   1) edge=4
                  end if
               else
     &         if (edge .eq. 2) then
                  if (ccw) then
                     if (ny .eq. nry) edge=3
                  else
                     if (ny .eq.   1) edge=1
                  end if
               else
     &         if (edge .eq. 3) then
                  if (ccw) then
                     if (nx .eq.   1) edge=4
                  else
                     if (nx .eq. nrx) edge=2
                  end if
               else
     &         if (edge .eq. 4) then
                  if (ccw) then
                     if (ny .eq.   1) edge=1
                  else
                     if (ny .eq. nry) edge=3
                  end if
               end if
               go to 15
            end if
         end if
      end if

c     Done with this contour line
      if (cntrpx(nrcntrp) .ne. cntrpx(nrp0+1) .and.
     &    cntrpy(nrcntrp) .ne. cntrpy(nrp0+1)) then

c        Close the curve
         nrcntrp=nrcntrp+1
         cntrpx (nrcntrp)=cntrpx(nrp0+1)
         cntrpy (nrcntrp)=cntrpy(nrp0+1)
      end if

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10,1x,a)') 'Trace','close'

      RETURN
      END      ! GRF_CNTR_TRACE
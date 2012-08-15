      SUBROUTINE GRF_CNTR_HOLES
     &          (v,mxx,mxy,nrx,nry,vc)

c***********************************************************************

c  Function:
c     This routine searches for holes inside an area enclosed by the
c     curve defined by XP(1),YP(1) through XP(NRP),YP(NRP). The holes
c     are defined by areas where V is greater than VC(1) or is less than
c     VC(2).

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     v            [r,mxx,mxy]   array containing values for contours
c     mxx          [i]           X dimension of V in calling program
c     mxy          [i]           Y dimension of V in calling program
c     nrx          [i]           number of elements of V along X
c     nry          [i]           number of elements of V along Y
c     vc           [r,2]         definition of the contour band

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:
c     grf_cntr_cell
c     grf_cntr_flag
c     grf_cntr_row
c     grf_cntr_trace

c     lwpc_error
c     sortr

c  References:

c  Change History:

c     27 May 98     Call to GRF_CNTR_CELL to ensure that an interior
c                   cell actually has exits before starting a contour
c                   trace.

c     25 Aug 98     Modified to use new GRF_CNTR_TRACE.

c*******************!***************************************************

      include      'grf_cntr.cmn'
      include      'lwpc_lun.cmn'

      character* 10 label
      character*200 error_msg

      logical       grf_cntr_flag      ! Function
      logical       tst

      integer       crossings
      integer       edge
      integer       ex
      integer       mxx
      integer       mxy
      integer       n
      integer       nc
      integer       ncx(2)
      integer       ncross
      integer       nmax
      integer       nrc
      integer       nrow
      integer       nrp0
      integer       nrx
      integer       nry
      integer       nx
      integer       nx1
      integer       nx2
      integer       ny
      integer       nymax
      integer       nymin
      integer       trace

      real          dummy
      real          v(mxx,mxy)
      real          vc(2)
      real          xcx(2)
      real          xen
      real          xex
      real          ycx(2)
      real          yen
      real          yex

      data          tst/.false./

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10,i5)') 'Holes'

c     Scan the curve to find the minimum and maximum y indices
      nmax=nrcntrp-1

      nymin=INT(cntrpy(1))
      nymax=INT(cntrpy(1))
      do n=2,nmax
         if (nymin .gt. INT(cntrpy(n))) nymin=INT(cntrpy(n))
         if (nymax .lt. INT(cntrpy(n))) nymax=INT(cntrpy(n))
      end do

c     Counter for the number of holes
      nrholes=0
      nrpts(0)=nrcntrp

c     Set up search, row by row
      do nrow=nymin+1,nymax-1

cxxcDEBUG
cxx         write(lwpcLOG_lun,'(a10,i5)') 'row',nrow

c        Find all crossings of the exterior curve at the current row
         crossings=0
         do n=1,nrcntrp
            if (cntrpy(n)  .eq. FLOAT(nrow)) then

c              Store this crossing
               crossings=crossings+1
               crossing (crossings)=cntrpx(n)
            else
     &      if (INT(cntrpy(n)) .eq. FLOAT(nrow) .and.
     &              cntrpx(n)  .eq. FLOAT( 1  )) then

               if (v(1,nrow) .ge. vc(2)) then

c                 Store this crossing
                  crossings=crossings+1
                  crossing (crossings)=cntrpx(n)
               end if
            else
     &      if (INT(cntrpy(n)) .eq. FLOAT(nrow) .and.
     &              cntrpx(n)  .eq. FLOAT(nrx )) then

               if (v(nrx,nrow) .ge. vc(2)) then

c                 Store this crossing
                  crossings=crossings+1
                  crossing (crossings)=cntrpx(n)
               end if
            end if
         end do

         if (crossings .gt. 0) then

c           Sort crossings in order of increasing value
            call SORTR
     &          (crossing,crossings,dummy,1,1,crossings)

            if (crossings .gt. 2) then

c              Check for duplicates
               ncross=1
               do while (ncross .lt. crossings)
                  if (crossing(ncross) .eq. crossing(ncross+1)) then
                     crossings=crossings-1
                     do nx=ncross,crossings
                        crossing(nx)=crossing(nx+1)
                     end do
                  else
                     ncross=ncross+1
                  end if
               end do
            end if
cxxcDEBUG
cxx            write(lwpcLOG_lun,'(a10,i5/(10x,6f10.4))')
cxx     &           'crossings',crossings,(crossing(n),n=1,crossings)

            if (MOD(crossings,2) .eq. 1) then

c              Odd number of crossings; skip this row
               crossings=0
            end if

c           Search this row between each pair of crossings
            ncross=1
            do while (ncross .lt. crossings)

               nx1=crossing(ncross  )
               nx2=crossing(ncross+1)
               do while (nx1 .lt. nx2)

                  edge=1
                  ny=nrow

                  call GRF_CNTR_ROW
     &                (v,mxx,mxy,nrx,vc,nx1,nx2,
     &                 nx,ny,nrc,ncx,xcx,ycx)

                  if (nrc .eq. 0) then

c                    No crossings were found;
c                    terminate search along this row
                     nx1=nx2
                     trace=0
                  else

c                    Store starting point for next search
c                    along this row
                     nx1=nx+1

c                    Check the crossings which were found
                     n=1
11                   nc=ncx(n)

                     if (GRF_CNTR_FLAG
     &                  (cells(1,nc),nrx,tst,nx,ny,edge)) then

c                       This cell has been used
                        trace=0
                     else

c                       This cell hasn't been used yet
                        trace=1

c                       Find out if there is an exit
                        xen=xcx(n)
                        yen=ycx(n)
                        call GRF_CNTR_CELL
     &                      (v,mxx,mxy,nrx,vc,nc,
     &                       edge,xen,yen,nx,ny,
     &                       nex,ex,xex,yex,label)

c                       Start a contour only if there is one exit
                        if (nex .eq. 1) then

c                          Verify that this exit satisfies
c                          the tracing criteria
                           if (nc .eq. 1 .and.
     &                        ((ex .eq. 2 .and.
     &                          v(nx+1,ny  ) .gt. vc(nc)) .or.
     &                         (ex .eq. 3 .and.
     &                          v(nx+1,ny  ) .gt. vc(nc) .and.
     &                          v(nx+1,ny+1) .gt. vc(nc)) .or.
     &                         (ex .eq. 4 .and.
     &                          v(nx  ,ny  ) .lt. vc(nc)))) then
                              trace=0
                           else
     &                     if (nc .eq. 2 .and.
     &                        ((ex .eq. 2 .and.
     &                          v(nx+1,ny  ) .lt. vc(nc)) .or.
     &                         (ex .eq. 3 .and.
     &                          v(nx  ,ny  ) .lt. vc(nc) .and.
     &                          v(nx  ,ny+1) .lt. vc(nc)) .or.
     &                         (ex .eq. 4 .and.
     &                          v(nx  ,ny  ) .gt. vc(nc)))) then
                              trace=0
                           end if
                        else

c                          Zero or multiple exits
                           trace=0
                        end if
                     end if

c                    If the current crossing doesn't work
c                    look for another one
                     if (trace .eq. 0) then
                        if (n .lt. nrc) then
                           n=2
                           go to 11
                        end if
                     end if
                  end if

                  if (trace .eq. 1) then

c                    Store current number of points
                     nrp0=nrcntrp

c                    Trace the contour
                     call GRF_CNTR_TRACE
     &                   (v,mxx,mxy,nrx,nry,vc,
     &                    nc,edge,nx,ny,xen,yen,'holes')

c                    Done with this contour line;
c                    check for a valid number of points
                     if (nrcntrp-nrp0 .gt. 5) then

c                       Store the location within the list of points
                        if (nrholes .eq. mxholes) then
                           write(error_msg,
     &                         '(''ERROR [GRF_CNTR_HOLES]: '',
     &                           ''Too many holes'')')
                           call GRF_DONE
                           call LWPC_ERROR ('ERROR',error_msg)
                        end if
                        nrholes=nrholes+1
                        nrpts(nrholes)=nrcntrp
cxycDEBUG
cxy                        write(lwpcLOG_lun,'(/a10,i5)')
cxy     &                       'Hole',nrholes
cxy                        do n=nrpts(nrholes-1)+1,nrpts(nrholes)
cxy                           write(lwpcLOG_lun,'(2i5,2f10.4)')
cxy     &                           n-nrpts(nrholes-1),n,
cxy     &                           cntrpx(n),cntrpy(n)
cxy                        end do
cxy                        write(lwpcLOG_lun,'(1x)')
                     else

c                       Not enough points for a valid contour;
c                       reset the counter
                        nrcntrp=nrp0
                     end if
                  end if
               end do
               ncross=ncross+2
            end do
         end if
      end do

cxxcDEBUG
cxx      write(lwpcLOG_lun,'(a10,i5)') 'Holes done'

      RETURN
      END      ! GRF_CNTR_HOLES
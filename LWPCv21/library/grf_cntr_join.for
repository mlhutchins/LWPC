      SUBROUTINE GRF_CNTR_JOIN

c***********************************************************************

c  Function:
c     This routine connects holes inside an area enclosed by the
c     curve defined by XP(1),YP(1) through XP(NRP),YP(NRP). The holes
c     are defined by the coordinates:
c     XP(NRP+1),      YP(NRP+1)      through XP(NRPTS(1)), YP(NRPTS(1));
c     XP(NRPTS(1)+1), YP(NRPTS(1)+1) through XP(NRPTS(2)), YP(NRPTS(2));
c     etc. The output coordinates are those of a polygon with a single,
c     contiguous set of coordinates. Up to 50 holes can be included.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}

c  Common blocks referenced:
c     grf$cntr

c  Functions and subroutines referenced:

c  References:

c  Change History:

c*******************!***************************************************

cxxcDEBUG
cxx      include      'lwpc_lun.cmn'

      include      'grf_cntr.cmn'

      integer       closest
      integer       mc
      integer       n1
      integer       nc
      integer       np

      real          rc
      real          rsq

c     Set up counter for the search for the hole which is closest to
c     the exterior curve
      nrcntrp=nrpts(nrholes)
      nmin=1
      nmax=nrpts(0)
      do nh=1,nrholes
         closestr(nh)=1.e12
      end do
cxxcDEBUG
cxx      write(lwpcLOG_lun,'(/a10)') 'original'
cxx      do nh=0,nrholes
cxx         write(lwpcLOG_lun,'(2i5)') nh,nrpts(nh)
cxx      end do
cxx      do n=1,nrpts(nrholes)
cxx         write(lwpcLOG_lun,'(i5,2f10.4)') n,cntrpx(n),cntrpy(n)
cxx      end do

      do while (nrholes .gt. 0)

c        To create a polygon without a lot of crossing lines we must
c        connect each of the holes to another or to the exterior curve.
c        The procedure is to insert the curve for each hole into the
c        polygon at the point of minimum separation of the curves.

c        Each time, we must insert 2 new points and remove 1; the point
c        to be removed is the one which closes the curve for each hole;
c        the added points connect the curve with the exterior curve

c        Find the hole which is closest to the exterior curve

         rsqmin=1.e12
         do nh=1,nrholes
            n1=nrpts(nh-1)+1
            np=nrpts(nh  )
            rc=closestr(nh)
            do n=nmin+1,nmax
               do m=n1,np-1
                  rsq=(cntrpx(n)-cntrpx(m))**2+(cntrpy(n)-cntrpy(m))**2
                  if (rc .gt. rsq) then
                     rc=rsq
                     nc=n
                     mc=m
                  end if
               end do
            end do
            if (closestr(nh) .gt. rc) then

c              Store the indices for the closest approach of this hole
c              to the exterior curve
               closestr(nh)=rc
               closestn(nh)=nc
               closestm(nh)=mc
            end if

            if (rsqmin .gt. closestr(nh)) then

c              Store the index of the hole which is closest to the
c              exterior curve
               closest=nh
               rsqmin=closestr(nh)
            end if
cxxcDEBUG
cxx            write(lwpcLOG_lun,'(/a10)') 'holes'
cxx            write(lwpcLOG_lun,'(5i5,f10.4)')
cxx     &            nh,n1,np,closestn(nh),closestm(nh),rc
         end do

c        Next, shift the coordinates of the hole to put the insertion
c        point at the beginning of the curve; ignore the last point
c        since it is the one that closes the current curve and we are
c        going to change it

         n1=nrpts(closest-1)+1
         np=nrpts(closest  )

         call SHIFT_ARRAY
     &       (cntrpx,mxcntrp,n1,closestm(closest),np-1)

         call SHIFT_ARRAY
     &       (cntrpy,mxcntrp,n1,closestm(closest),np-1)

c        Close the curve by inserting the first point of the curve
c        at the end of the list of points for the same curve

         cntrpx(np)=cntrpx(n1)
         cntrpy(np)=cntrpy(n1)
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(/a10,3i5)') 'rotated',n1,np
cxx         do n=n1,np
cxx            write(lwpcLOG_lun,'(i5,2f10.4)') n,cntrpx(n),cntrpy(n)
cxx         end do

c        Next, move the coordinates of the new curve to the insertion
c        point of the exterior curve; also store the new range of this
c        section for the search for the hole which is closest to the
c        exterior curve

         n=closestn(closest)+1

         call SHIFT_ARRAY
     &       (cntrpx,mxcntrp,n,n1,np)

         call SHIFT_ARRAY
     &       (cntrpy,mxcntrp,n,n1,np)

c        Set the starting point for the search for the closest point
c        to be the beginning of the inserted curve

         nmin=n

c        Set the ending point for the search for the closest point
c        to be the end of the inserted curve

         nmax=nmin+nrpts(closest)-nrpts(closest-1)-1

c        Shift the coordinate arrays by 1 to make room for the extra
c        continuity point

         do n=nrcntrp,nmax+1,-1
            cntrpx(n+1)=cntrpx(n)
            cntrpy(n+1)=cntrpy(n)
         end do

c        Add a point to close the connection between the inserted curve
c        and the exterior curve

         nrcntrp=nrcntrp+1
         cntrpx(nmax+1)=cntrpx(closestn(closest))
         cntrpy(nmax+1)=cntrpy(closestn(closest))
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(/a10,3i5)') 'shifted',nmin,nmax
cxx         do n=nmin-1,nmax+1
cxx            write(lwpcLOG_lun,'(i5,2f10.4)') n,cntrpx(n),cntrpy(n)
cxx         end do

c        Adjust the number of holes

         nrholes=nrholes-1

c        Adjust the indices to account for the shift in the points of
c        the exterior curve

         nshift=nmax-nmin+1
         nrpts(0)=nrpts(0)+1+nshift

         do nh=1,closest-1
            if (closestn(nh) .ge. nmin)
     &         closestn(nh)=closestn(nh)+1+nshift
            closestm(nh)=closestm(nh)+1+nshift
            nrpts   (nh)=nrpts   (nh)+1+nshift
         end do

         do nh=closest,nrholes
            if (closestn(nh+1) .ge. nmin) then
               closestn(nh)=closestn(nh+1)+1+nshift
            else
               closestn(nh)=closestn(nh+1)
            end if
            closestm(nh)=closestm(nh+1)+1
            nrpts   (nh)=nrpts   (nh+1)+1
            closestr(nh)=closestr(nh+1)
         end do
cxxcDEBUG
cxx         write(lwpcLOG_lun,'(/a10,i5)') 'adjusted',nrcntrp
cxx         do nh=0,nrholes
cxx            if (nh .eq. 0) then
cxx               write(lwpcLOG_lun,'(4i5)')
cxx     &               nh,nrpts(nh),nmin
cxx            else
cxx               write(lwpcLOG_lun,'(4i5)')
cxx     &               nh,nrpts(nh),closestn(nh),closestm(nh)
cxx            end if
cxx         end do
cxx         do n=1,nrpts(nrholes)
cxx            write(lwpcLOG_lun,'(i5,2f10.4)') n,cntrpx(n),cntrpy(n)
cxx         end do
      end do

      RETURN
      END      ! GRF_CNTR_JOIN
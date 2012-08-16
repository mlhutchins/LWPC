      SUBROUTINE LAND_SEA
     &          (xlon,xlat,land)

c***********************************************************************
c                   subroutine land_sea
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Determines if a point is over sea or land

c  Parameters passed:
c     xlon          [r] West longitude of the point in degrees
c     xlat          [r] North latitude of the point in degrees

c  Parameters returned:
c     land          [l] .TRUE. if over land

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs

c     ground
c     lwpc_error

c     Requires COND$D.DAT

c  References:

c  Change History:
c     09 Dec 1991   Modified to use subroutine GROUND

c*******************!***************************************************

      logical       land
      character*200 error_msg


      phi=xlon
      if (phi .gt. 180.) then
         phi=phi-360.
      else
         if (phi .lt. -180.) phi=phi+360.
      end if

      if (ABS(phi) .gt. 180. .or. ABS(xlat) .gt. 90.01) then
         write(error_msg,
     &       '(''[LAND_SEA]: '',
     &         ''Xlon Xlat'',2f9.2)') xlon,xlat
         call LWPC_ERROR('ERROR',error_msg)
      end if

      call GROUND (xlon,xlat,ncode,sigma,epsr)

      if (ncode .eq. 10) then
         land=.false.
      else
         land=.true.
      end if

      RETURN
      END      ! LAND_SEA
      SUBROUTINE GCDBR
     &          (dphi,clt1,clt2,rho,br,inb)

c***********************************************************************
c                         subroutine gcdbr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns great circle distance and geographic bearing angle from
c     position 1 (P1) to position 2 (P2).

c  Parameters passed:
c     dphi          [r] longitude of P1 minus longitude of P2
c     clt1          [r] co-latitude of P1
c     clt2          [r] co-latitude of P2
c     rho           [r] great circle distance from P1 to P2
c     inb           [i] =0: RHO is computed and
c                           BR from P1 thru P2 is computed at 1
c                       =1: RHO is input    and
c                           BR from P1 thru P2 is computed at 2

c  Parameters returned:
c     rho           [r] great circle distance from P1 to P2
c     br            [r] geographic bearing angle of the path

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     acos
c     cos
c     min
c     mod
c     sign
c     sin

c  References:

c  Change History:

c  Notes:
c     Sign convention is + for West and North.
c     All coordinates are in radians.

c*******************!***************************************************

      data          pi/3.1415926535898/,twopi/6.2831853071796/

      REDUCE(arg)=SIGN(MIN(ABS(arg),1.),arg)


      cclt1=COS(clt1)
      sclt1=SIN(clt1)
      cclt2=COS(clt2)
      sclt2=SIN(clt2)

      dl=MOD(dphi,twopi)
      adl=ABS(dl)
      if (ABS(clt1) .eq. 0. .or. ABS(clt1-pi) .eq. 0.) then
c        Point 1 is at one of the poles
         gcd=ABS(clt1-clt2)
         if (ABS(clt1) .eq. 0.) then
            br=pi-dl
         else
            br=dl
         end if
         if (br .lt. 0.) br=br+twopi
      else
     &if (adl .eq. 0.) then
c        Coordinates are on same longitude
         gcd=clt1-clt2
         if (gcd .ge. 0.) then
            br=0.
         else
            gcd=-gcd
            br=pi
         end if
      else
     &if (ABS(adl-pi) .eq. 0.) then
c        Coordinates are on opposite longitudes
         gcd=clt1+clt2
         if (gcd .le. pi) then
            if (inb .eq. 0) then
               br=0.
            else
               br=pi
            end if
         else
            gcd=twopi-gcd
            if (inb .eq. 0) then
               br=pi
            else
               br=0.
            end if
         end if
      else
         if (adl .ge. pi) then
            if (dl .ge. 0.) then
               dl=dl-twopi
            else
               dl=dl+twopi
            end if
         end if
         if (inb .eq. 0) then
            cgcd=cclt1*cclt2+sclt1*sclt2*COS(dl)
            gcd=ACOS(REDUCE(cgcd))
            if (gcd .eq. 0.) then
               br=0.
            else
               if (sclt1 .eq. 0.) then
c                 Point 1 is at one of the poles
                  if (ABS(clt1) .lt. .5*pi) then
                     br=pi
                  else
                     br=0.
                  end if
               else
                  br=ACOS(REDUCE((cclt2-cclt1*cgcd)/
     &                                 (sclt1*SIN(gcd))))
               end if
            end if
         else
            if (rho .gt. pi) then
               gcd=twopi-rho
            else
               gcd=rho
            end if
            if (gcd .eq. 0.) then
               br=0.
            else
               if (sclt2 .eq. 0.) then
c                 Point 2 is at one of the poles
                  if (ABS(clt2) .lt. .5*pi) then
                     br=pi
                  else
                     br=0.
                  end if
               else
                  br=pi-ACOS(REDUCE((cclt1-cclt2*COS(gcd))/
     &                                    (sclt2*SIN(gcd))))
               end if
            end if
         end if
         if (dl .lt. 0.) br=twopi-br
      end if
      if (inb .eq. 0) then
         rho=gcd
      else
         if (rho .gt. pi) then
c           Long path calculations
            if (br .lt. pi) then
               br=br+pi
            else
               br=br-pi
            end if
         end if
      end if

      RETURN
      END      ! GCDBR
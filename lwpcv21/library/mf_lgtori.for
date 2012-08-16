      SUBROUTINE MF_LGTORI
     &          (xintg,r,drdc,nderiv)

      implicit complex (a-h,o-z)

      real           omega,wavenr,
     &               rmax,xmgsq,cthrsh,cmgsq,delta(4)

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_lgfl/lgflag(4)
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rmtx/x(4),dxdc(4),dxdh(4),dhdxdc(4)

      dimension      xintg(8),r(4),drdc(4)

      data           delta/1.0,0.0,0.0,1.0/,cthrsh/0.03/,rmax/50.0/


      cmgsq=REAL(c)**2+AIMAG(c)**2
      do i=1,4
         if (lgflag(i) .eq. 0) then
            xmgsq=REAL(xintg(i))**2+AIMAG(xintg(i))**2
            if ((xmgsq .gt.      rmax**2 .and. cmgsq .ge. cthrsh**2)
     &         .or.
     &          (xmgsq .gt. 1.e3*rmax**2 .and. cmgsq .lt. cthrsh**2))
     &         then
               iovflo=1
               RETURN
            end if
            x(i)=xintg(i)
            if (nderiv .eq. 1) dxdc(i)=xintg(i+4)
         else
            if (ABS(REAL(xintg(i))) .gt. 10.) then
               iovflo=1
               RETURN
            end if
            r(i)=EXP(xintg(i))
            x(i)=(r(i)+delta(i))/c
            if (nderiv .eq. 1) then
               dlnrdc=xintg(i+4)
               drdc(i)=r(i)*dlnrdc
               dxdc(i)=(drdc(i)-x(i))/c
            end if
         end if
      end do
      RETURN
      END      ! MF_LGTORI
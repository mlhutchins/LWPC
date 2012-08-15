      SUBROUTINE MF_FINLLG
     &          (xintg,nderiv)

      implicit complex (a-h,o-z)

      real           omega,wavenr,
     &               delta(4)

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_lgfl/lgflag(4)
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rmtx/x(4),dxdc(4),dxdh(4),dhdxdc(4)

      dimension      xintg(8),r(4),drdc(4)

      data           delta/1.0,0.0,0.0,1.0/


      do i=1,4
         if (lgflag(i) .eq. 1) then
            r(i)=EXP(xintg(i))
            x(i)=(r(i)+delta(i))/c
            xintg(i)=x(i)
            if (nderiv .eq. 1) then
               drdc(i)=r(i)*xintg(i+4)
               dxdc(i)=(drdc(i)-x(i))/c
               xintg(i+4)=dxdc(i)
            end if
         end if
      end do
      RETURN
      END      ! MF_FINLLG
      SUBROUTINE MF_TODLGS
     &          (dxdhi,r,drdc,nderiv)

      implicit complex (a-h,o-z)

      real           omega,wavenr

      common/mf_lgfl/lgflag(4)
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rmtx/x(4),dxdc(4),dxdh(4),dhdxdc(4)

      dimension      dxdhi(8),r(4),drdc(4)


      do i=1,4
         if (lgflag(i) .eq. 0) then
            dxdhi(i)=dxdh(i)
            if (nderiv .eq. 1) dxdhi(i+4)=dhdxdc(i)
         else
            drdh=c*dxdh(i)
            dxdhi(i)=drdh/r(i)
            if (nderiv .eq. 1) then
               dhdrdc=c*dhdxdc(i)+dxdh(i)
               dhdldc=(dhdrdc-drdh*drdc(i)/r(i))/r(i)
               dxdhi(i+4)=dhdldc
            end if
         end if
      end do
      RETURN
      END      ! MF_TODLGS
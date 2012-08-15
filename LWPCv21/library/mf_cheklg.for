      SUBROUTINE MF_CHEKLG
     &          (xintg,nderiv)

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      integer        rprnt,xprnt
      real           omega,wavenr,
     &               thresh,rmag,rmgsq,cthrsh,cmgsq,delta(4)

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_lgfl/lgflag(4)
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rmtx/x(4),dxdc(4),dxdh(4),dhdxdc(4)

      dimension      xintg(8),r(4),drdc(4)

      data           delta/1.0,0.0,0.0,1.0/,thresh/0.1/,cthrsh/0.03/


      cmgsq=REAL(c)**2+AIMAG(c)**2
      do i=1,4
         if (lgflag(i) .eq. 0) then
            x(i)=xintg(i)
            r(i)=c*x(i)-delta(i)
            rmgsq=REAL(r(i))**2+AIMAG(r(i))**2
            if (rmgsq .ge. thresh**2 .and. cmgsq .ge. cthrsh**2) then
               xintg(i)=LOG(r(i))
               if (nderiv .eq. 1) then
                  dxdc(i)=xintg(i+4)
                  drdc(i)=c*dxdc(i)+x(i)
                  xintg(i+4)=drdc(i)/r(i)
               end if
               lgflag(i)=1
               if (rprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_CHEKLG: Integration variable '',
     &                  ''changed from (real-imag) to (log-angle) '',
     &                  ''for r('',i1,'')'')') i
            end if
         else
            rmag=EXP(REAL(xintg(i)))
            if (rmag .le. 0.7*thresh) then
               r(i)=EXP(xintg(i))
               x(i)=(r(i)+delta(i))/c
               xintg(i)=x(i)
               if (nderiv .eq. 1) then
                  drdc(i)=r(i)*xintg(i+4)
                  dxdc(i)=(drdc(i)-x(i))/c
                  xintg(i+4)=dxdc(i)
               end if
               lgflag(i)=0
               if (rprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_CHEKLG: Integration variable '',
     &                  ''changed from (log-angle) to (real-imag) '',
     &                  ''for r('',i1,'')'')') i
            end if
         end if
      end do
      RETURN
      END      ! MF_CHEKLG
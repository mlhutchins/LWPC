      SUBROUTINE MF_RMTRX

c***********************************************************************

c     This routine transforms the reflection matrix from
c     magneto-ionic to perpendicular/parallel reflection
c     coefficients.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      integer        rprnt,xprnt
      real           omega,wavenr,amp(2,2),phs(2,2)

      common/mf_emtx/eu(2,2),ed(2,2),deu(2,2),ded(2,2),
     &               eiu(2,2),eid(2,2),deiu(2,2),deid(2,2)
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_oemx/r(2,2),dr(2,2),temp(2,2),dtemp(2,2),nrt
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rntg/x(2,2),dx(2,2),dxdh(8),roe(2,2),droe(2,2),
     &               rnum11,rnum21,rnum12,rnum22,rden


      if (mrprnt .gt. 0 .and. nrt .gt. 0) then

         cxlog=LOG(roe(1,1))
         amp(1,1)= REAL(cxlog)*.434294482
         phs(1,1)=AIMAG(cxlog)*57.296
         cxlog=LOG(roe(2,1))
         amp(2,1)= REAL(cxlog)*.434294482
         phs(2,1)=AIMAG(cxlog)*57.296
         cxlog=LOG(roe(1,2))
         amp(1,2)= REAL(cxlog)*.434294482
         phs(1,2)=AIMAG(cxlog)*57.296
         cxlog=LOG(roe(2,2))
         amp(2,2)= REAL(cxlog)*.434294482
         phs(2,2)=AIMAG(cxlog)*57.296

         write(lwpcLOG_lun,
     &       '(''MF_RMTRX: Convert to perp and parallel '',
     &         ''for theta='',2f7.3)') theta
         write(lwpcLOG_lun,
     &       '(''MF_RMTRX: Starting ROE'')')
         write(lwpcLOG_lun,
     &       '(''MF_RMTRX: Log10 amp'')')
         write(lwpcLOG_lun,
     &       '((14x,2(f10.4)))')
     &        ((amp(l,k),k=1,2),l=1,2)
         write(lwpcLOG_lun,
     &       '(''MF_RMTRX: Phase'')')
         write(lwpcLOG_lun,
     &       '((14x,2(f10.2)))')
     &        ((phs(l,k),k=1,2),l=1,2)
      end if

c     Get matrices of eigen vectors and their inverses.
      call MF_EMTRX (0)

c     The procedure for transforming the magneto-ionic reflection matrix
c     into perpendicular/parallel form is the inverse of that used in
c     the main entry: ROEMTX. The matrix of magneto-ionic reflection
c     coefficients is transformed by multiplying it on the left by the
c     matrix of eigen vectors for downgoing waves and on the right by
c     the inverse of the matrix of eigen vectors for upgoing waves.

      temp(1,1)=ed(1,1)*roe(1,1)+ed(1,2)*roe(2,1)
      temp(2,1)=ed(2,1)*roe(1,1)+ed(2,2)*roe(2,1)
      temp(1,2)=ed(1,1)*roe(1,2)+ed(1,2)*roe(2,2)
      temp(2,2)=ed(2,1)*roe(1,2)+ed(2,2)*roe(2,2)

      r(1,1)=temp(1,1)*eiu(1,1)+temp(1,2)*eiu(2,1)
      r(2,1)=temp(2,1)*eiu(1,1)+temp(2,2)*eiu(2,1)
      r(1,2)=temp(1,1)*eiu(1,2)+temp(1,2)*eiu(2,2)
      r(2,2)=temp(2,1)*eiu(1,2)+temp(2,2)*eiu(2,2)

      x(1,1) = (r(1,1)+(1.,0.))/c
      x(2,1) =  r(2,1)/c
      x(1,2) =  r(1,2)/c
      x(2,2) = (r(2,2)+(1.,0.))/c

      rnum11 = r(1,1)+(1.,0.)
      rnum21 = r(2,1)
      rnum12 = r(1,2)
      rnum22 = r(2,2)+(1.,0.)
      rden = c

      if (ideriv .eq. 1) then

         dtemp(1,1)=ded(1,1)*roe(1,1)+ed(1,1)*droe(1,1)
     &             +ded(1,2)*roe(2,1)+ed(1,2)*droe(2,1)
         dtemp(2,1)=ded(2,1)*roe(1,1)+ed(2,1)*droe(1,1)
     &             +ded(2,2)*roe(2,1)+ed(2,2)*droe(2,1)
         dtemp(1,2)=ded(1,1)*roe(1,2)+ed(1,1)*droe(1,2)
     &             +ded(1,2)*roe(2,2)+ed(1,2)*droe(2,2)
         dtemp(2,2)=ded(2,1)*roe(1,2)+ed(2,1)*droe(1,2)
     &             +ded(2,2)*roe(2,2)+ed(2,2)*droe(2,2)

         dr(1,1)=dtemp(1,1)*eiu(1,1)+temp(1,1)*deiu(1,1)
     &          +dtemp(1,2)*eiu(2,1)+temp(1,2)*deiu(2,1)
         dr(2,1)=dtemp(2,1)*eiu(1,1)+temp(2,1)*deiu(1,1)
     &          +dtemp(2,2)*eiu(2,1)+temp(2,2)*deiu(2,1)
         dr(1,2)=dtemp(1,1)*eiu(1,2)+temp(1,1)*deiu(1,2)
     &          +dtemp(1,2)*eiu(2,2)+temp(1,2)*deiu(2,2)
         dr(2,2)=dtemp(2,1)*eiu(1,2)+temp(2,1)*deiu(1,2)
     &          +dtemp(2,2)*eiu(2,2)+temp(2,2)*deiu(2,2)

         dx(1,1)=(dr(1,1)-x(1,1))/c
         dx(2,1)=(dr(2,1)-x(2,1))/c
         dx(1,2)=(dr(1,2)-x(1,2))/c
         dx(2,2)=(dr(2,2)-x(2,2))/c
      end if

      if (mrprnt .gt. 0 .and. nrt .gt. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_RMTRX: Resulting (R+1)/CS'')')
         write(lwpcLOG_lun,
     &       '((14x,2(5x,2f12.4)))')
     &        ((x(l,k),k=1,2),l=1,2)
      end if
      nrt=nrt-1
      RETURN
      END      ! MF_RMTRX
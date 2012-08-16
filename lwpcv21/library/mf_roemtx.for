      SUBROUTINE MF_ROEMTX

c***********************************************************************

c     This routine transforms the reflection matrix from
c     perpendicular/parallel to magneto-ionic reflection
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


      nrt=2

c     Get matrices of eigen vectors and their inverses.
      call MF_EMTRX (0)

c     The variable denoted by X in this routine is the integration
c     variable in INTEG.
      r(1,1)=c*x(1,1)-(1.,0.)
      r(2,1)=c*x(2,1)
      r(1,2)=c*x(1,2)
      r(2,2)=c*x(2,2)-(1.,0.)

      if (mrprnt .gt. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_ROEMTX: Convert from perp and parallel '',
     &         ''for theta='',2f7.3/)') theta
         write(lwpcLOG_lun,
     &       '(''MF_ROEMTX: Starting (R+1)/C matrix'')')
         write(lwpcLOG_lun,
     &       '((14x,2(5x,2f12.4)))')
     &        ((x(l,k),k=1,2),l=1,2)
         write(lwpcLOG_lun,
     &       '(''MF_ROEMTX: Starting  R      matrix'')')
         write(lwpcLOG_lun,
     &       '((14x,2(5x,2f12.4)))')
     &        ((r(l,k),k=1,2),l=1,2)
      end if

c     The reflection matrix in perpendicular/parallel form is multiplied
c     on the left by the inverse of the matrix of eigen vectors for
c     downgoing waves and on the right by the matrix of eigen vectors
c     for upgoing waves.

      temp(1,1)=eid(1,1)*r(1,1)+eid(1,2)*r(2,1)
      temp(2,1)=eid(2,1)*r(1,1)+eid(2,2)*r(2,1)
      temp(1,2)=eid(1,1)*r(1,2)+eid(1,2)*r(2,2)
      temp(2,2)=eid(2,1)*r(1,2)+eid(2,2)*r(2,2)

      roe(1,1)=temp(1,1)*eu(1,1)+temp(1,2)*eu(2,1)
      roe(2,1)=temp(2,1)*eu(1,1)+temp(2,2)*eu(2,1)
      roe(1,2)=temp(1,1)*eu(1,2)+temp(1,2)*eu(2,2)
      roe(2,2)=temp(2,1)*eu(1,2)+temp(2,2)*eu(2,2)

      if (ideriv .eq. 1) then

         dr(1,1)=x(1,1)+c*dx(1,1)
         dr(2,1)=x(2,1)+c*dx(2,1)
         dr(1,2)=x(1,2)+c*dx(1,2)
         dr(2,2)=x(2,2)+c*dx(2,2)

         dtemp(1,1)=deid(1,1)*r(1,1)+eid(1,1)*dr(1,1)
     &             +deid(1,2)*r(2,1)+eid(1,2)*dr(2,1)
         dtemp(2,1)=deid(2,1)*r(1,1)+eid(2,1)*dr(1,1)
     &             +deid(2,2)*r(2,1)+eid(2,2)*dr(2,1)
         dtemp(1,2)=deid(1,1)*r(1,2)+eid(1,1)*dr(1,2)
     &             +deid(1,2)*r(2,2)+eid(1,2)*dr(2,2)
         dtemp(2,2)=deid(2,1)*r(1,2)+eid(2,1)*dr(1,2)
     &             +deid(2,2)*r(2,2)+eid(2,2)*dr(2,2)

         droe(1,1)=dtemp(1,1)*eu(1,1)+temp(1,1)*deu(1,1)
     &            +dtemp(1,2)*eu(2,1)+temp(1,2)*deu(2,1)
         droe(2,1)=dtemp(2,1)*eu(1,1)+temp(2,1)*deu(1,1)
     &            +dtemp(2,2)*eu(2,1)+temp(2,2)*deu(2,1)
         droe(1,2)=dtemp(1,1)*eu(1,2)+temp(1,1)*deu(1,2)
     &            +dtemp(1,2)*eu(2,2)+temp(1,2)*deu(2,2)
         droe(2,2)=dtemp(2,1)*eu(1,2)+temp(2,1)*deu(1,2)
     &            +dtemp(2,2)*eu(2,2)+temp(2,2)*deu(2,2)
      end if

      if (mrprnt .eq. 0) RETURN

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
     &    '(''MF_ROEMTX: Resulting ROE'')')
      write(lwpcLOG_lun,
     &    '(''MF_ROEMTX: Log10 amp'')')
      write(lwpcLOG_lun,
     &    '((14x,2(f10.4)))')
     &     ((amp(l,k),k=1,2),l=1,2)
      write(lwpcLOG_lun,
     &    '(''MF_ROEMTX: Phase'')')
      write(lwpcLOG_lun,
     &    '((14x,2(f10.2)))')
     &     ((phs(l,k),k=1,2),l=1,2)
      RETURN
      END      ! MF_ROEMTX
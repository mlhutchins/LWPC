      SUBROUTINE MF_INTROE

c***********************************************************************

c     This routine interpolates values of the elements of the magneto-
c     ionic reflection matrix. Entry INIT OE determines the coefficients
c     for the interpolation series if this kind of interpolation can be
c     used. Otherwise, the parameter LENSET is set to be non-zero in the
c     call to E MTRX indicating that the rectangle size is to be reduced
c     or routine LAGRNG is to be used. Additional details are found in
c     section V of NOSC TR 1143.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxboxes=200)

      character*200  error_msg
      real           adjmsh,wdchng,
     &               unitr,boxlft,boxrht,
     &               uniti,boxtop,boxbot,
     &               omega,wavenr,
     &               thresh,rlnmax

      common/mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rntg/r(4),dr(4),drdh(4),drdcdh(4),
     &               roe(4),droe(4),rnd(5)
     &      /mf_roes/rlist(22,mxboxes)
     &      /mf_rseq/r0(4),drdt(4),d2rdt2(4),d3rdt3(4),
     &               zetap(2),gammap(2),theta0,extra

      dimension      rseq(22)

      equivalence   (rseq,r0)

      data           rlnmax/35./,nrlist/22/,thresh/23./


c     Retrieve interpolation parameters.
      call MF_XFER (rlist(1,kb),rseq,nrlist)

c     Initialization for given value of THETA.
      c=COS(theta*0.01745329252)
      s=SIN(theta*0.01745329252)
      csq=c**2
      ssq=s**2

      dcdt=-s*0.01745329252
      dt=1./dcdt

c     An interpolated value of each element of the magneto-ionic
c     reflection matrix is found using a third order series for the
c     log of each element.

      delt=theta-theta0
      do k=1,4
         alnroe=((d3rdt3(k)*delt/6.+d2rdt2(k)*.5)*delt
     &            +drdt(k))*delt+r0(k)
         if (REAL(alnroe) .gt. thresh)
     &      write(lwpcLOG_lun,
     &          '(''MF_INTROE: '',
     &            ''Log of ROE('',i1,'')='',2f8.3)')
     &              k,alnroe
         if (REAL(alnroe) .gt. rlnmax) then
            write(error_msg,
     &          '(''[MF_INTROE]: '',
     &            ''Interpolated ROE out of range'')')
            call LWPC_ERROR ('ERROR',error_msg)
         end if
         roe(k)=EXP(alnroe)
      end do

      if (ideriv .eq. 1) then
         do k=1,4
            dlnrdt=(d3rdt3(k)*delt*.5+d2rdt2(k))*delt+drdt(k)
            droe(k)=dlnrdt*roe(k)*dt
         end do
      end if

c     The magneto-ionic reflection matrix is transformed to
c     perpendicular/parallel form and then referred to the reference
c     height D.

      call MF_RMTRX
      call MF_FSINTG

      RETURN
      END      ! MF_INTROE
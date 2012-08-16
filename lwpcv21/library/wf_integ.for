      SUBROUTINE WF_INTEG
     &          (isotropic,iflag)

c     Performs the integration of the p matrix down through the
c     ionosphere, using the techniques given by Pitteway.
c     Accuracy is maintained by adjusting the step size so that the
c     p matrix is computed with sufficient accuracy.

c     iflag=0  integ for eigen only
c     iflag=1  integ for eigen and eigen-dthta

      implicit real*8 (a-h,o-z)

c     LWPM
      real     *  4 hten,algen,htnu,algnu,
     &              charge,mratio,select_hgts,hgts

      common/lwpc_pr/
     &              hten(51),algen(51,3),nrhten,ihten,
     &              htnu(51),algnu(51,3),nrhtnu,ihtnu,
     &              charge(3),mratio(3),nrspec,lu_prf,
     &              select_hgts(2),hgts(3)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht,
     &              dtheta,lub,thtinc,alpha,h,prec,
     &              p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 eigen2,
     &              m11,m21,m31,m12,m22,m32,m13,m23,m33,
     &              c1,s1,c2,s2,p1,p2,dp1dh,dp2dh,
     &              m31 sav,m32 sav,m33 sav,ortho

      common/wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht
     &      /wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter
     &      /wf_mmtx/
     &              m11,m21,m31,m12,m22,m32,m13,m23,m33
     &      /wf_pmtx/
     &              c1,c2,s1,s2,p1(8),p2(8),dp1dh(8),dp2dh(8)
     &      /wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl

c     Local
      character*200 error_msg
      integer       svflag

      dimension     p1 sav(16),dp1dh sav(16),
     &              p2 sav(16),dp2dh sav(16),
     &              prev p(16),pv dpdh(16),temp p(16)

      equivalence  (p1 sav,p1),(dp1dh sav,dp1dh),
     &             (p2 sav,p2),(dp2dh sav,dp2dh)

c     Minimum step-size allowed (1/1024) and height tolerance (1/2048)
      data          dhmin/0.0009765625d0/,eps ht/0.00048828125d0/


      isteps=0
      emin=prec*.1d0
      emax=prec
      kmax=0
      levl=0
      wfht=top ht
      ht=wfht

      call WF_T_MTRX  (isotropic)
      call WF_INIT    (isotropic,1)
      call WF_P_DERIV (1)

      if (iflag .eq. 1) then
         call WF_TI_MTRX (isotropic)
         call WF_INIT    (isotropic,2)
         call WF_P_DERIV (2)
      end if
      call WF_XFER (p1 sav,p sav,16)
      m31 sav=m31
      m32 sav=m32
      m33 sav=m33
      ortho=0.d0
      a norm=1.d0
      b norm=1.d0
      call WF_STORE

      wfht=wfht-del ht
      delh2=0.125d0*del ht
      svflag=0

c     Determine next stepsize to use.
10    if (svflag .eq. 1) delh2=savdh2
      svflag=0
      ht0=ht
      call WF_XFER (p1 sav,prev p,16)
      call WF_XFER (dp1dh sav,pv dpdh,16)
      htlim=wfht
      if (ihten .lt. nrhten .and. htlim+eps ht .le. hten(ihten+1))
     &   htlim=hten(ihten+1)
      if (ihtnu .lt. nrhtnu .and. htlim+eps ht .le. htnu(ihtnu+1))
     &   htlim=htnu(ihtnu+1)
      if (ht0-delh2 .lt. htlim+eps ht) then
         savdh2=delh2
         svflag=1
         delh2=ht0-htlim
      end if

c     Perform next integration step.
   50 call WF_STEP (isotropic,1,delh2,0)
      call WF_XFER (p1 sav,temp p,16)
      m31 sav=m31
      m32 sav=m32
      m33 sav=m33
      ht=ht0
      call WF_XFER (prev p,p1 sav,16)
      call WF_XFER (pv dpdh,dp1dh sav,16)
      delh=0.5d0*delh2
      call WF_STEP (isotropic,1,delh,1)
      call WF_P_DERIV (1)

      call WF_STEP (isotropic,1,delh,2)

c     Check accuracy of result.
      error=0.d0
      do j=1,16
         pabs=abs(p1 sav(j)-temp p(j))
         if (error .lt. pabs) error=pabs
      end do

c     Adjust step size if necessary.
      if (error .ge. emax) then
         if (delh .gt. dhmin) then
            delh2=0.5d0*delh2
            ht=ht0
            call WF_XFER (prev p,p1 sav,16)
            call WF_XFER (pv dpdh,dp1dh sav,16)
            svflag=0
            go to 50
         end if
         if (kmax .eq. 0) then
            write(error_msg,
     &          '(''[WF_INTEG]: '',
     &            ''Minimum stepsize used at ht='',f9.4)') ht
            call LWPC_ERROR ('Warning',error_msg)
         end if
         kmax=1
      end if
      call WF_SCALE (1)
      call WF_XFER (p1 sav,p sav,16)
      if (ht .lt. wfht+eps ht) call WF_STORE
      call WF_P_DERIV (1)
      if (iflag .eq. 1) then
         ht=ht0
         call WF_STEP    (isotropic,2,delh,3)
         call WF_P_DERIV (2)
         call WF_STEP    (isotropic,2,delh,4)
         call WF_SCALE   (2)
         call WF_P_DERIV (2)
      end if

      isteps=isteps+1

      if (error .lt. emin) delh2=2.d0*delh2

c     Check integration and profile heights.
      if (ht .gt. bot ht) then
         if (ht .le. wfht) wfht=wfht-del ht
         go to 10
      end if
      RETURN
      END      ! WF_INTEG
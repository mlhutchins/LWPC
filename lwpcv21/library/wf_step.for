      SUBROUTINE WF_STEP
     &          (isotropic,index,delh,iflag)

c     Increments the solution of p from ht to ht-delh, using
c     Runge-Kutta integration

c     iflag=0  one    large step using eigen
c           1  first  small step using eigen
c           2  second small step using eigen
c           3  first  small step using eigen-dtheta
c           4  second small step using eigen-dtheta

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht,
     &              p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 eigen2,
     &              c,s,p,dpdh,
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44,
     &              m31 sav,m32 sav,m33 sav,ortho

      common/wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht
     &      /wf_pmtx/
     &              c(2),s(2),p(8,2),dpdh(8,2)
     &      /wf_tmtx/
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44
     &      /wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl

c     Local
      complex  * 16 p0(8),hdelp0(8),delp1(8),delp2(8)

      dimension     t      (18),tm     (18),
     &              t  sav1(18),t  sav2(18),
     &              tm sav1(18),tm sav2(18),
     &              tm sav3(18),tm sav4(18)

      equivalence  (t,t11),(tm,tm11)


      wn=20.958445d-3*freq2
      ht0=ht
      delh k=delh*wn
      do j=1,8
         p0(j)=p(j,index)
         hdelp0(j)=-dpdh(j,index)*0.5d0*delh k
         p(j,index)=p0(j)+hdelp0(j)
      end do

      ht=ht0-0.5d0*delh
      if (iflag .eq. 0) then

         call WF_T_MTRX (isotropic)
         call WF_XFER (t, t sav1,18)
         call WF_XFER (tm,tm sav2,18)
      else
     &if (iflag .eq. 1) then

         call WF_T_MTRX (isotropic)
         call WF_XFER (tm,tm sav1,18)
      else
     &if (iflag .eq. 2) then

         call WF_T_MTRX (isotropic)
         call WF_XFER (tm,tm sav3,18)
      else
     &if (iflag .eq. 3) then

         call WF_XFER (tm sav1,tm,18)
         call WF_TI_MTRX (isotropic)
      else
     &if (iflag .eq. 4) then

         call WF_XFER (tm sav3,tm,18)
         call WF_TI_MTRX (isotropic)
      end if

      call WF_P_DERIV (index)
      do j=1,8
         delp1(j)=-dpdh(j,index)*delh k
         p(j,index)=p0(j)+0.5d0*delp1(j)
      end do

      call WF_P_DERIV (index)
      do j=1,8
         delp2(j)=-dpdh(j,index)*delh k
         p(j,index)=p0(j)+delp2(j)
      end do

      ht=ht0-delh
      if (iflag .eq. 0) then

         call WF_T_MTRX (isotropic)
         call WF_XFER (t, t sav2,18)
         call WF_XFER (tm,tm sav4,18)
      else
     &if (iflag .eq. 1) then

         call WF_XFER (t sav1,t,18)
      else
     &if (iflag .eq. 2) then

         call WF_XFER (t sav2,t,18)
      else
     &if (iflag .eq. 3) then

         call WF_XFER (tm sav2,tm,18)
         call WF_TI_MTRX (isotropic)
      else
     &if (iflag .eq. 4) then

         call WF_XFER (tm sav4,tm,18)
         call WF_TI_MTRX (isotropic)
      end if

      call WF_P_DERIV (index)
      do j=1,8
         p(j,index)=p0(j)+(hdelp0(j)+delp1(j)+delp2(j)
     &                    -dpdh(j,index)*0.5d0*delh k)/3.d0
      end do
      RETURN
      END      ! WF_STEP
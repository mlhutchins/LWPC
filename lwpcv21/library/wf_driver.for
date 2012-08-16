      SUBROUTINE WF_DRIVER
     &          (lu_log,lu_wfd,lu_wfa,print_wf,nsgmnt,neigen,arb_azim)

c***********************************************************************

c     WF_DRIVER calls for the downward integration, and then performs
c     the back substitution of normalizing values (saved by WF_STORE).

c     Field strengths are computed at heights from TOP HT to BOT HT at
c     DEL HT increments and are stored in EX, EY, EZ, HX, HY, HZ.

c***********************************************************************

c     INPUT PARAMETERS:

c     print_wf      Indicates levels of print from WaveFields
c                   =0: minimum print
c                   =1: adds iterations
c                   =2: adds fields vs. height

c***********************************************************************

c     Note that CAPI and NORM are used differently here than in
c     LW_FAST_MC, but it's OK because the parameters are not passed
c     between this routine and LW_FAST_MC.

c********************!**************************************************

      implicit real*8 (a-h,o-z)

      parameter    (nrht=65)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max_ht,del_ht,top_ht,bot_ht,
     &              dtheta,lub,thtinc,alpha,h,prec,
     &              p sav,a norm,b norm,ht,wfht1,wfht2
      complex  * 16 eigen2,
     &              exc,dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld,
     &              m11,m21,m31,m12,m22,m32,m13,m23,m33,
     &              c1,s1,c2,s2,p1,p2,dp1dh,dp2dh,
     &              r11,r22,r12,r21,rbar11,rbar22,
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44

      common/wf_grnd/
     &              exc(3),dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld
     &      /wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max_ht,del_ht,top_ht,bot_ht
     &      /wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter
     &      /wf_mmtx/
     &              m11,m21,m31,m12,m22,m32,m13,m23,m33
     &      /wf_pmtx/
     &              c1,c2,s1,s2,p1(8),p2(8),dp1dh(8),dp2dh(8)
     &      /wf_rmtx/
     &              r11,r22,r12,r21,rbar11,rbar22
     &      /wf_save/
     &              p sav(16),
     &              m31 sav,m32 sav,m33 sav,ortho,
     &              a norm,b norm,ht,wfht1,wfht2,
     &              levl
     &      /wf_tmtx/
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44

c     Local
      logical       adjoint
      integer       print_wf,arb_azim
      complex  * 16 ex,ey,ez,hx,hy,hz,b term(2),w(4),o sum,tp,stp,
     &              p term(4,2)

      equivalence  (p sav,p term)

      common        ex(nrht),ey(nrht),ez(nrht),
     &              hx(nrht),hy(nrht),hz(nrht)

      dimension     eout(12)

      data          dtr/.01745329252d0/


      if (print_wf .gt. 1) then

         if (neigen .eq. 1)
     &      write(lu_log,
     &          '(/''WF_DRIVER: '',
     &             ''Output for segment '',i3)') nsgmnt

         write(lu_log,
     &       '(/''WF_DRIVER: '',
     &          ''azim='',f7.3,'' eigen='',2f8.4,'' at h= 0.'')')
     &            azim2,eigen2
      end if

c     Determine if isotropic case
      if (magfld2 .eq. 0.d0) then
         isotropic=1
      else
     &if (ABS(codip2-90.d0) .ge. 0.15d0) then
         isotropic=0
      else
     &if (ABS(azim2- 90.d0) .lt. 0.15d0) then
         isotropic=1
      else
     &if (ABS(azim2-270.d0) .ge. 0.15d0) then
         isotropic=0
      else
         isotropic=1
      end if

      adjoint=.false.
      azim_save=azim2
      iter_save=iter_flag

      capk=SQRT(1.d0-alpha*h)

      if (h .gt. 0.d0) then

c        The eigen angle must be referenced to h
         stp=SIN(eigen2*dtr)*capk
         eigen2=DCMPLX(0.d0,-1.d0/dtr)
     &         *LOG(SQRT(1.d0-stp**2)+(0.d0,1.d0)*stp)
         if (print_wf .gt. 1) then

c           Print the eigen angle at H
            write(lu_log,
     &          '(''WF_DRIVER:'',14x,
     &            ''eigen='',2f8.4,'' at h='',f3.0)')
     &              eigen2,h
         end if
      end if

c     Iteration to satisfy modal equation
10    call WF_ITRATE (isotropic)

c     Temporary (?) fix for failure to converge to a solution:
c     Drop the offending mode
      if (nriter .gt. mxiter) RETURN

      if (print_wf .gt. 0 .and. .not.adjoint) then

c        Print the iterations and the final eigen angle
         write(lu_log,
     &       '(''WF_DRIVER: '',
     &         ''nriter='',i2,'' eigen='',2f8.4,'' f='',1pe10.2)')
     &           nriter,eigen2,ABS(f)
      end if

c     Combine solutions at bottom so they satisfy boundary condition.
      call WF_BNDY (isotropic,b term)

      if (print_wf .gt. 1 .and. .not.adjoint) then

         write(lu_log,
     &       '(/''WF_DRIVER: '',
     &          ''wfht1, wfht2, levl='',2f11.6,i4)')
     &            wfht1,wfht2,levl

         write(lu_log,
     &       '(''WF_DRIVER: '',
     &         ''azim='',f7.3,'' eigen='',2f8.4,'' at h='',f3.0)')
     &           azim2,eigen2,h

         write(lu_log,
     &       '(''WF_DRIVER: '',
     &         ''Field strengths''/''   ht'',
     &         ''  mag -- ex -- ang  mag -- ey -- ang'',
     &         ''  mag -- ez -- ang'',
     &         ''  mag -- hx -- ang  mag -- hy -- ang'',
     &         ''  mag -- hz -- ang'')')
      end if
      iht=1
      wfht=0.d0

      if (wfht2 .gt. 0.d0) then

c        Height gains below ionosphere
         do while (wfht .lt. wfht2)

            call WF_HTGAIN (wfht,ex(iht),ey(iht),ez(iht),
     &                           hx(iht),hy(iht),hz(iht))
            if (print_wf .gt. 1 .and. .not.adjoint) then
               call WF_MA2 (ex(iht),ey(iht),ez(iht),eout(1))
               call WF_MA2 (hx(iht),hy(iht),hz(iht),eout(7))
               write(lu_log,
     &             '(f5.1,6(1pe11.3,0pf7.3))') wfht,eout
            end if
            iht=iht+1
            wfht=wfht+del_ht
         end do
      end if

c     Perform back substitution of normalizing values.
      o sum=0.d0
      prod a=1.d0
      prod b=1.d0
      call WF_LOAD
      go to 25

21    o sum=o sum*a norm/b norm+ortho
      prod a=prod a*a norm
      if (prod a .lt. 1.d-30) prod a=0.d0
      prod b=prod b*b norm

      call WF_LOAD
      do j=1,4
         p term(j,2)=(p term(j,2)-o sum*p term(j,1))*prod b
         p term(j,1)= p term(j,1)*prod a
      end do

c     Compute field strengths at output heights.
25    do j=1,4
         w(j)=p term(j,1)*b term(1)+p term(j,2)*b term(2)
      end do
      ex(iht)=  w(1)
      ey(iht)=- w(2)
      ez(iht)=-(w(4)*s1+m31 sav*w(1)-m32 sav*w(2))/(1.d0+m33 sav)
      hx(iht)=  w(3)
      hy(iht)=  w(4)
      hz(iht)=- w(2)*s1
      if (print_wf .gt. 1 .and. .not.adjoint) then
         call WF_MA2 (ex(iht),ey(iht),ez(iht),eout(1))
         call WF_MA2 (hx(iht),hy(iht),hz(iht),eout(7))
         write(lu_log,
     &       '(f5.1,6(1pe11.3,0pf7.3))') wfht,eout
      end if

      iht=iht+1
      wfht=wfht+del_ht
      if (wfht .le. wfht1) go to 21
      if (levl .ne. 0) then
         write(lu_log,
     &       '(''ERROR [WF_DRIVER]: levl is not zero: '',i2)') levl
         write(lu_log,
     &       '(''                   wfht, iht ='',f11.6,i3)') wfht,iht
      end if

      if (wfht .le. max_ht) then

c        Compute field strengths at heights above the profile assuming
c        a gradual exponential decay.
         do while (wfht .le. max_ht)
            ex(iht)=ex(iht-1)*.9
            ey(iht)=ey(iht-1)*.9
            ez(iht)=ez(iht-1)*.9
            hx(iht)=hx(iht-1)*.9
            hy(iht)=hy(iht-1)*.9
            hz(iht)=hz(iht-1)*.9
            if (print_wf .gt. 1 .and. .not.adjoint) then
               call WF_MA2 (ex(iht),ey(iht),ez(iht),eout(1))
               call WF_MA2 (hx(iht),hy(iht),hz(iht),eout(7))
               write(lu_log,
     &             '(f5.1,6(1pe11.3,0pf7.3))') wfht,eout
            end if
            iht=iht+1
            wfht=wfht+del_ht
         end do
      end if
      if (print_wf .gt. 1 .and. .not.adjoint) write(lu_log,'(/)')

c     Update the excitation factors
      exc(3)=(1.d0+rbar11)**2
     &      *(1.d0-rbar22*r22)*SQRT(s1)/(dfdt/dtr*rbar11)/fprld**2
      exc(2)=(1.d0+rbar11)
     &      *(1.d0+rbar22)*SQRT(s1)/(dfdt/dtr)*r12/(fprld*fprpd)
      exc(1)=-exc(3)*s1

c     Store wave fields
      if (adjoint) then

         write(lu_wfa,rec=neigen) tp,stp,exc,ey,ez,hy,hz
         iter_flag=iter_save
         azim2=azim_save
      else

c        The final eigen angle must be referenced to the ground
         if (h .eq. 0.d0) then
            stp=SIN(eigen2*dtr)
            tp=eigen2
         else
            stp=SIN(eigen2*dtr)/capk
            tp=DCMPLX(0.d0,-1.d0/dtr)
     &        *LOG(SQRT(1.d0-stp**2)+(0.d0,1.d0)*stp)
         end if
         write(lu_wfd,rec=neigen) tp,stp,exc,ey,ez,hy,hz
         if (arb_azim .eq. 1) then
            iter_flag=0
            azim2=180.d0-azim2
            if (azim2 .lt. 0.d0) azim2=azim2+360.d0
            adjoint=.true.
            go to 10
         end if
      end if
      RETURN
      END      ! WF_DRIVER
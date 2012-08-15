      SUBROUTINE WF_ITRATE
     &          (isotropic)

c     Routine for finding eigen, which satisfies the modal equation.

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht,
     &              dtheta,lub,thtinc,alpha,h,prec
      complex  * 16 eigen2,
     &              exc,dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld

      common/wf_inpt/
     &              eigen2,
     &              freq2,azim2,codip2,magfld2,sigma2,epsr2,
     &              max ht,del ht,top ht,bot ht
     &      /wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter
     &      /wf_grnd/
     &              exc(3),dfdt,f,ey0,hy0,eyg,hyg,fprpd,fprld

c     Local
      logical       loop
      complex  * 16 wf_f_fnctn,
     &              eigen0,f0,zdtheta,del t

      equivalence  (dtheta,zdtheta)


c     Save initial values
      eigen0=eigen2
      bot ht0=bot ht

c     Find the bottom height which gives
c     the best chance of finding a solution
      bot ht=INT(bot ht/del ht)*del ht
      if (bot ht .gt. 0.) then

         bot ht1=bot ht
         fmag1=1000.
         loop=.true.
         do while (loop)

            call WF_INTEG (isotropic,1)
            fmag=ABS(WF_F_FNCTN (1))
            if (fmag .lt. fmag1) then

c              Store the bottom height which gives
c              the smallest F function
               bot ht1=bot ht
               fmag1=fmag
            end if
            if (bot ht .eq. 0. .or. fmag .le. .5) then
               loop=.false.
            else
               bot ht=bot ht-del ht
            end if
         end do
         if (bot ht1 .ne. bot ht) then

c           Use the bottom height which gives
c           the smallest F function
            bot ht=bot ht1
            fmag=fmag1
            if (bot ht .eq. 0. .and. fmag .gt. .5) then

c              We aren't going to be able to find a solution
               f=fmag
               bot ht=bot ht0
               nriter=mxiter+1
cxxcDEBUG
cxx               write(4,
cxx     &             '(''Adjust D:  Failed'')')
               RETURN
            end if
         end if
cxxcDEBUG
cxx         write(4,
cxx     &       '(''Adjust D:'',f7.4,1pe12.3)') bot ht,fmag
      end if

      nriter=0
      call WF_INTEG (isotropic,1)
      f0=  WF_F_FNCTN (2)
      f =  WF_F_FNCTN (1)
      dfdt=(f-f0)/zdtheta
      if (iter_flag .eq. 1) then
         eigen0=eigen2
         loop=.true.
         do while (loop)
            del t=-f/dfdt
            absrl=ABS(DREAL(del t))
            absim=ABS(DIMAG(del t))
            if (absrl .gt. thtinc) del t=del t*(thtinc/absrl)
            if (absim .gt. thtinc) del t=del t*(thtinc/absim)
            eigen2=eigen2+del t
            nriter=nriter+1
            if (absrl .le. lub(1) .and. absim .le. lub(2)) then
               loop=.false.
            else
     &      if (nriter .eq. mxiter .or. DIMAG(eigen2) .ge. 0.) then
               loop=.false.
            end if
            call WF_INTEG (isotropic,1)
            f0=  WF_F_FNCTN (2)
            f =  WF_F_FNCTN (1)
            dfdt=(f-f0)/zdtheta
         end do
      end if

c     Restore the original value of bottom height
      bot ht=bot ht0
      RETURN
      END      ! WF_ITRATE
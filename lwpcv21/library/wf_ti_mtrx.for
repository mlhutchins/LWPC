      SUBROUTINE WF_TI_MTRX
     &          (isotropic)

c     Computes t - the coefficient matrix of the linear system of
c                         dp/dz=-ik*t*p.

      implicit real*8 (a-h,o-z)

c     Wave Fields
      real     *  8 dtheta,lub,thtinc,alpha,h,prec
      complex  * 16 c1,s1,c2,s2,p1,p2,dp1dh,dp2dh,
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44

      common/wf_iter/
     &              dtheta(2),lub(2),thtinc,alpha,h,prec,
     &              iter_flag,mxiter,nriter
     &      /wf_pmtx/
     &              c1,c2,s1,s2,p1(8),p2(8),dp1dh(8),dp2dh(8)
     &      /wf_tmtx/
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44


      t41=tm41
      t32=tm32+c2**2
      t14=1.d0-s2**2*tm14
      t11=-tm11*s2
      t44=-tm44*s2
      if (isotropic .eq. 0) then
         t31=tm31
         t12=tm12*s2
         t42=tm42
         t34=tm34*s2
      end if
      RETURN
      END      ! WF_TI_MTRX
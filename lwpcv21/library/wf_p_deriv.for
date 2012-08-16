      SUBROUTINE WF_P_DERIV
     &          (index)

c     Routine which computes the height derivatives of the field
c     vectors, P, according to Clemmow and Heading (1954). Equation is
c        dP/dz=-i*k*T*P.
c     Multiplication by -i is performed by operating on real and imag
c     parts separately. Multiplication by k is done in WF_STEP.

c     Wave Fields
      complex  * 16 c,s,p,dpdh,
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44

      common/wf_pmtx/
     &              c(2),s(2),p(4,2,2),dpdh(4,2,2)
     &      /wf_tmtx/
     &              t11,t31,t41,t12,t32,t42,t14,t34,t44,
     &              tm11,tm31,tm41,tm12,tm32,tm42,tm14,tm34,tm44

c     Local
      complex  * 16 deriv


      do j=1,2
         deriv=t11*p(1,j,index)+t12*p(2,j,index)+t14*p(4,j,index)
         dpdh(1,j,index)=(0.d0,-1.d0)*deriv
         deriv=p(3,j,index)
         dpdh(2,j,index)=(0.d0,-1.d0)*deriv
         deriv=t31*p(1,j,index)+t32*p(2,j,index)+t34*p(4,j,index)
         dpdh(3,j,index)=(0.d0,-1.d0)*deriv
         deriv=t41*p(1,j,index)+t42*p(2,j,index)+t44*p(4,j,index)
         dpdh(4,j,index)=(0.d0,-1.d0)*deriv
      end do
      RETURN
      END      ! WF_P_DERIV
      SUBROUTINE WF_R_MTRX
     &          (index)

c     Computes reflection coefficient matrix from p matrix and
c     returns it in r.

      implicit real*8 (a-h,o-z)

c     Wave Fields
      complex  * 16 c,s,p,dpdh,
     &              r11,r22,r12,r21,rbar11,rbar22

      common/wf_pmtx/
     &              c(2),s(2),p(4,2,2),dpdh(4,2,2)
     &      /wf_rmtx/
     &              r11,r22,r12,r21,rbar11,rbar22

c     Local
      complex  * 16 g12,g13,g14,g23,g24,g34,
     &              d00,d11,d22,d12,d21


      g12=p(1,1,index)*p(2,2,index)-p(1,2,index)*p(2,1,index)
      g13=p(1,1,index)*p(3,2,index)-p(1,2,index)*p(3,1,index)
      g14=p(1,1,index)*p(4,2,index)-p(1,2,index)*p(4,1,index)
      g23=p(2,1,index)*p(3,2,index)-p(2,2,index)*p(3,1,index)
      g24=p(2,1,index)*p(4,2,index)-p(2,2,index)*p(4,1,index)
      g34=p(3,1,index)*p(4,2,index)-p(3,2,index)*p(4,1,index)

      d00=-g13+c(index)*( g34-g12+c(index)*g24)
      d11= g13+c(index)*( g34+g12+c(index)*g24)
      d22= g13+c(index)*(-g34-g12+c(index)*g24)
      d12=2.d0*c(index)*g14
      d21=2.d0*c(index)*g23

      r11=d11/d00
      r22=d22/d00
      r12=d12/d00
      r21=d21/d00
      RETURN
      END      ! WF_R_MTRX
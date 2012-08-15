      SUBROUTINE WF_INIT
     &          (isotropic,index)

c     Computes the initial p matrix, i.e., the initial conditions
c     for the integration dp/dz=-ik*t*p.

      implicit real*8 (a-h,o-z)

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
      complex  * 16 b3,b2,b1,b0,q temp,det,sqroot,q(4)

      dimension     diff(4)


      if (isotropic .eq. 0) then

         b3=-(t11+t44)
         b2=t11*t44-t14*t41-t32
         b1=-(-t32*(t11+t44)+t12*t31+t34*t42)
         b0=-t11*(t32*t44-t34*t42)
     &      +t12*(t31*t44-t34*t41)
     &      -t14*(t31*t42-t32*t41)

         call WF_QUARTC (b3,b2,b1,b0,q)

         if (DIMAG(q(1)) .le. 0.d0) then
            l=1
         else
            l=0
         end if
         m=l+1
         do while (m .lt. 4)
            n=m+1
            do while (n .le. 4)
               if (DIMAG(q(n)) .le. 0.d0) then
                  l=l+1
                  q temp=q(n)
                  q(n)=q(m)
                  q(m)=q temp
                  m=m+1
               end if
               n=n+1
            end do
            m=m+1
         end do
         if (l .ne. 2) then
            m=1
            do while (m .le. 4)
               angq=ATAN2(DIMAG(q(m)),DREAL(q(m)))*57.295779513d0
               if (DIMAG(q(m)) .lt. 0.d0) angq=angq+360.d0
               if (angq .lt. 315.d0) angq=angq+360.d0
               diff(m)=angq
               m=m+1
            end do
            m=2
            do while (m .le. 4)
               n=m
               do while (n .le. 4)
                  if (diff(n) .le. diff(m-1)) then
                     temp=diff(n)
                     diff(n)=diff(m-1)
                     diff(m-1)=temp
                     q temp=q(n)
                     q(n)=q(m-1)
                     q(m-1)=q temp
                  end if
                  n=n+1
               end do
               m=m+1
            end do
         end if

         j=1
         do while (j .le. 2)
            det=(t11-q(j))*(t44-q(j))-t14*t41
            p(1,j,index)=(t12*q(j)-(t12*t44-t14*t42))/det
            p(2,j,index)=1.d0
            p(3,j,index)=q(j)
            p(4,j,index)=(t42*q(j)+(t12*t41-t11*t42))/det
            j=j+1
         end do
      else

         b1=(t11+t44)*(.5d0,0.d0)
         b0=t11*t44-t14*t41
         sqroot=SQRT(b1**2-b0)
         q(1)=b1+sqroot
         q(4)=b1-sqroot
         sqroot=SQRT(t32)
         q(2)=+sqroot
         q(3)=-sqroot

         q1 test=q(1)+(0.d0,1.d0)*q(1)
         q4 test=q(4)+(0.d0,1.d0)*q(4)
         if (q4 test .gt. q1 test) q(1)=q(4)
         q2 test=q(2)+(0.d0,1.d0)*q(2)
         q3 test=q(3)+(0.d0,1.d0)*q(3)
         if (q3 test .gt. q2 test) q(2)=q(3)

         p(1,1,index)=t14
         p(2,1,index)=0.d0
         p(3,1,index)=0.d0
         p(4,1,index)=q(1)-t11

         p(1,2,index)=0.d0
         p(2,2,index)=0.d0
         p(3,2,index)=q(2)
         p(4,2,index)=0.d0
      end if
      RETURN
      END      ! WF_INIT
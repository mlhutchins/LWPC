      SUBROUTINE MF_INITR

c***********************************************************************

c     Computation of (R+1)/C for reflection from a sharply bounded
c     anisotropic ionosphere of semi-infinite extent where R is the
c     reflection coefficient matrix as defined by Budden. The solution
c     is used as the initial condition for a Runge-Kutta integration.
c     The solution is derived from the one in Radio Science Vol. 3, Aug
c     1968, p.792-795. Derivatives wrt c=COS(THETA) are also computed
c     if IDERIV is set non-zero.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      integer        rprnt,xprnt
      real           omega,wavenr,ht,angq,temp,diff(4)
      complex        m11,m21,m31,m12,m22,m32,m13,m23,m33

      common/mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_mmtx/m11,m21,m31,m12,m22,m32,m13,m23,m33,ht
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rntg/r11,r21,r12,r22,dr11,dr21,dr12,dr22,drdh(8),
     &               roe(8),rnd(5)

      dimension      qtemp(4),q(2),p(2),t(2),dq(2),dp(2),dt(2)

      data           one/(1.,0.)/,two/(2.,0.)/


c     Booker quartic coefficients and its four roots.
      b4=one+m33
      b3=s*(m13+m31)
      b2=-(csq+m33)*(one+m11)+m13*m31-(one+m33)*(csq+m22)+m23*m32
      b1=s*(m12*m23+m21*m32-(csq+m22)*(m13+m31))
      b0=(one+m11)*(csq+m22)*(csq+m33)+m12*m23*m31+m13*m21*m32
     &          -m13*(csq+m22)*m31-(one+m11)*m23*m32-m12*m21*(csq+m33)

      call MF_QUARTC (b4,b3,b2,b1,b0,qtemp)

c     Selection of the two roots corresponding to upward travelling
c     waves as being those of angle nearest to 315 degrees in the
c     complex plane.
      l=0
      do m=2,4
         do n=m,4
            if (AIMAG(qtemp(n)) .le. 0.) then
               l=l+1
               ctemp=qtemp(n)
               qtemp(n)=qtemp(m-1)
               qtemp(m-1)=ctemp
            end if
         end do
      end do
      if (l .ne. 2) then
         do n=1,4
            angq=ATAN2(AIMAG(qtemp(n)),REAL(qtemp(n)))*57.295779513
            if (angq .lt.   0.) angq=angq+360.
            if (angq .lt. 135.) angq=angq+360.
            diff(n)=ABS(angq-315.)
         end do
         do nm=2,4
            do n=nm,4
               if (diff(n) .le. diff(nm-1)) then
                  temp=diff(n)
                  diff(n)=diff(nm-1)
                  diff(nm-1)=temp
                  ctemp=qtemp(n)
                  qtemp(n)=qtemp(nm-1)
                  qtemp(nm-1)=ctemp
               end if
            end do
         end do
      end if
      q(1)=qtemp(1)
      q(2)=qtemp(2)

      if (rprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_INITR: theta='',2f8.3/
     &         ''         q temp='',4(1pe13.4,e12.4)/
     &         ''         q     ='',2(1pe13.4,e12.4))')
     &           theta,(q temp(i),i=1,4),(q(j),j=1,2)

c     Remainder of the solution and its derivatives.
      do k=1,2
         d11=one+m11-q(k)**2
         d12=m12
         d13=m13+q(k)*s
         d31=m31+q(k)*s
         d32=m32
         d33=csq+m33
         den=d11*d33-d13*d31
         p(k)= -(d12*d33-d13*d32)/den
         t(k)=s*(d11*d32-d12*d31)/den+q(k)*p(k)
      end do

      if (ideriv .eq. 1) then
         ds=-c/s
         db3=ds*(m13+m31)
         db2=-two*c*(two+m11+m33)
         db1=(ds/s)*b1-two*s*c*(m13+m31)
         db0=two*c*((one+m11)*(csq+m22+csq+m33)-m13*m31-m12*m21)

         do k=1,2
            dq(k)=-(((db3*q(k)+db2)*q(k)+db1)*q(k)+db0)/
     &             ((((4.,0.)*b4*q(k)+(3.,0.)*b3)*q(k)+two*b2)*q(k)+b1)
            dd11=-two*q(k)*dq(k)
            dd13=q(k)*ds+dq(k)*s
            dd31=q(k)*ds+dq(k)*s
            dd33=two*c
            dden=d11*dd33+dd11*d33-d13*dd31-dd13*d31
            dp(k)=-(dden*p(k)+(d12*dd33-dd13*d32))/den
            dt(k)=q(k)*dp(k)+dq(k)*p(k)
     &           +(d11*d32-d12*d31)*(ds-s*dden/den)/den
     &           +s*(dd11*d32-d12*dd31)/den
         end do
      end if

      den=(t(1)*c+p(1))*(c+q(2))-(t(2)*c+p(2))*(c+q(1))
      factor=two/den
      r11=(t(1)*(c+q(2))-t(2)*(c+q(1)))*factor
      r22=((t(1)*c+p(1))-(t(2)*c+p(2)))*factor
      r12=-(t(1)*p(2)-t(2)*p(1))*factor
      r21=-(q(1)-q(2))*factor

      if (ideriv .eq. 1) then
         dden=(t(1)*c+p(1))*(one+dq(2))
     &       +(t(1)+dt(1)*c+dp(1))*(c+q(2))
     &       -(t(2)*c+p(2))*(one+dq(1))
     &       -(t(2)+dt(2)*c+dp(2))*(c+q(1))
         dr11=-r11*dden/den
     &        +(t(1)*(one+dq(2))+dt(1)*(c+q(2))
     &        -t(2)*(one+dq(1))-dt(2)*(c+q(1)))*factor
         dr22=-r22*dden/den
     &        +((t(1)+dt(1)*c+dp(1))
     &        -(t(2)+dt(2)*c+dp(2)))*factor
         dr12=-r12*dden/den
     &        -(t(1)*dp(2)+dt(1)*p(2)
     &        -t(2)*dp(1)-dt(2)*p(1))*factor
         dr21=-r21*dden/den
     &        -(dq(1)-dq(2))*factor
      end if
      RETURN
      END      ! MF_INITR
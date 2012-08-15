      SUBROUTINE MF_XDERIV

c     Differential equations for (R+1)/C where R is the reflection
c     matrix as defined by Budden.  The equations are derived from the
c     differential equations for R given by Budden in Proc. Roy. Soc.
c     (London), A227, pp.516-537 (1955).  Storage into the array
c     'RMTRX' is made so that the equations may be used with either
c     the main set of integration variables, or with the comparison
c     set, X.  Differential equations for the derivatives of (R+1)/C
c     wrt c=COS(THETA) are also used if IDERIV is set non-zero.

      implicit complex (a-h,o-z)

      real           omega,wavenr
      complex        mikov2

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rmtx/r11,r21,r12,r22,dr11dc,dr21dc,dr12dc,dr22dc,dr11dh,
     &               dr21dh,dr12dh,dr22dh,dr11ch,dr21ch,dr12ch,dr22ch
     &      /mf_rntg/r(8),drdh(8),roe(8),rnd(5)
     &      /mf_smtx/a11,a22,b11,b12,b22,c11,c21,c22,d11,d21,d12,d22,
     &               db11dc,db22dc,dc11dc,dc22dc,dd11dc,dd21dc,dd12dc
     &      /mf_xntg/x(4),dxdh(4)

      dimension      rmtrx(8),deriv(8),rtm(4),drtmdc(4)

      equivalence   (rmtrx(1),r11),(deriv(1),dr11dh)


      call MF_LGTORI (x,rtm,drtmdc,0)

      if (iovflo .ne. 0) RETURN

      mikov2=CMPLX(0.,-.5*wavenr)

      d11r11=d11*r11
      d12r21=d12*r21
      d21r12=d21*r12
      d22r22=d22*r22
      r11r22=r11*r22
      r12r21=r12*r21

      dr11dh=mikov2*(a11+(b11+c11+d11r11+d12r21+d21r12)*r11
     &              +b12*r21+c21*r12+d22*r12r21)
      dr21dh=mikov2*((b22+c11+d11r11+d12r21+d22r22)*r21
     &               +c21*r22+d21*r11r22)
      dr12dh=mikov2*((b11+c22+d11r11+d21r12+d22r22)*r12
     &               +b12*r22+d12*r11r22)
      dr22dh=mikov2*(a22+(b22+c22+d12r21+d21r12+d22r22)*r22
     &              +d11*r12r21)

      call MF_TODLGS (dxdh,rtm,drtmdc,0)

      RETURN
      END      ! MF_XDERIV
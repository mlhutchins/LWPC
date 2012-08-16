      SUBROUTINE MF_EMTRX
     &          (lenset)

c***********************************************************************

c     The magnetoionic eigenvectors are found. They are defined at
c     HTNTRP for the condition that the electron density is negligible.
c     The first element of each vector is Hy and the other is Ey. A
c     separate set of eigenvectors is found for upgoing waves and for
c     downgoing waves. For additional description see Section II of
c     NOSC TR 1143.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxeigen=50)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex,rprnt,xprnt
      real          freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              ranger,rangei,atnmax,lub,h,
     &              rtol,xintrp,htntrp,d,z0,zz,
     &              hofwr,topht,botht,
     &              dcl,dcm,dcn,ec,
     &              omega,wavenr,
     &              dclsq,dcmsq,dcnsq,psq,t1,t2

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_drcs/dcl,dcm,dcn,ec,g,q
     &      /mf_emtx/e(2,2,2),de(2,2,2),ei(2,2,2),dei(2,2,2)
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rseq/r0(4),drdt(4),d2rdt2(4),d3rdt3(4),
     &               zetap(2),gammap(2),theta0,extra


c     Initialization of parameters
      dclsq=dcl**2
      dcmsq=dcm**2
      dcnsq=dcn**2

      c=COS(theta*0.01745329252)
      s=SIN(theta*0.01745329252)
      csq=c**2
      ssq=s**2

      psq=1.+ec
      qsq=csq+ec
      q=SQRT(qsq)
      if (REAL(c) .lt. 0.) q = -q

c     N=1: upgoing waves; N=2: downgoing
      ud=1.
      do n=1,2

         a= dcl*s+ud*dcn*q
         b=-dcn*s+ud*dcl*q
         w=(dcmsq*psq-b**2)*g
         hy=2.*(a+dcm*b*g)*psq
         ey=2.*(a-dcm*b*g)

         zsq=((b**2+dcmsq*psq)*g)**2-4.*a**2*psq
         zeta=SQRT(zsq)

c        LENSET is set to a non-zero value in MF_INTROE for THETA at the
c        center of the search rectangle and set to zero for all other
c        locations in the rectangle. The sign of ZETA is chosen so that
c        W+ZETA has the larger magnitude. The sign of ZETA elsewhere is
c        chosen so that the value of ZETA is closer to that of ZETA at
c        the center.

         if (lenset .ne. 0) zetap(n)=w

         t=zeta-zetap(n)
         t1=REAL(t)**2+AIMAG(t)**2
         t=zeta+zetap(n)
         t2=REAL(t)**2+AIMAG(t)**2
         if (t1 .gt. t2) zeta=-zeta
         if (lenset .ne. 0) zetap(n)=zeta

         if (mrprnt .ne. 0 .and. lenset .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_EMTRX: zeta '',i1,''='',1p2e11.3)')
     &              n,zeta

         wpz=w+zeta

c        GAMMA is the denominator of each element of each eigenvector.
c        The sign of GAMMA is chosen so that the value of GAMMA is
c        closer to that of GAMMA at the center of the rectangle.

         gamsq=-2.*zeta*wpz
         gamma=SQRT(gamsq)

         if (lenset .ne. 0) gammap(n)=gamma

         t=gamma-gammap(n)
         t1=REAL(t)**2+AIMAG(t)**2
         t=gamma+gammap(n)
         t2=REAL(t)**2+AIMAG(t)**2
         if (t1 .gt. t2) gamma=-gamma

         v=1./gamma

c        The 2x2 matrix of eigenvectors
         e(1,1,n)= hy*v
         e(2,1,n)=wpz*v
         e(1,2,n)=wpz*v
         e(2,2,n)= ey*v

         if (ideriv .eq. 1) then

c           Get the derivatives with respect to cosine(THETA)
            ds=-c/s
            dq=c/q

            da= dcl*ds+ud*dcn*dq
            db=-dcn*ds+ud*dcl*dq
            dw=-2.*b*db*g
            dhy=2.*(da+dcm*db*g)*psq
            dey=2.*(da-dcm*db*g)

            dzsq=4.*(b**2+dcmsq*psq)*b*db*g**2-8.*a*da*psq
            dzeta=dzsq/(2.*zeta)

            dwpz=dw+dzeta
            dgamsq=-2.*(dzeta*wpz+zeta*dwpz)
            dgamma=dgamsq/(2.*gamma)

            dv=-dgamma/gamma**2

            de(1,1,n)=dhy*v+hy*dv
            de(2,1,n)=dwpz*v+wpz*dv
            de(1,2,n)=de(2,1,n)
            de(2,2,n)=dey*v+ey*dv
         end if

c        Inverse of the eigenvector matrix is found. The formulation for
c        GAMMA was chosen such that the determinant of the 2x2 matrix of
c        eigenvectors and, hence, its inverse are each equal to one.
         ei(1,1,n)= e(2,2,n)
         ei(2,1,n)=-e(2,1,n)
         ei(1,2,n)=-e(1,2,n)
         ei(2,2,n)= e(1,1,n)

         if (ideriv .eq. 1) then
            dei(1,1,n)= de(2,2,n)
            dei(2,1,n)=-de(2,1,n)
            dei(1,2,n)=-de(1,2,n)
            dei(2,2,n)= de(1,1,n)
         end if

c        Change the sign and index for downgoing waves.
         ud=-ud
      end do

      if (mrprnt .ne. 0 .and. lenset .ne. 0) then
         write(lwpcLOG_lun,
     &       '(''MF_EMTRX: Eigenvectors for   upgoing waves'')')
         write(lwpcLOG_lun,
     &       '((11x,2(1pe14.3,e11.3)))')
     &        ((e(l,k,1),k=1,2),l=1,2)
         write(lwpcLOG_lun,
     &       '(''MF_EMTRX: Eigenvectors for downgoing waves'')')
         write(lwpcLOG_lun,
     &       '((11x,2(1pe14.3,e11.3)))')
     &        ((e(l,k,2),k=1,2),l=1,2)
      end if
      RETURN
      END      ! MF_EMTRX
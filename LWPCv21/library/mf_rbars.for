      SUBROUTINE MF_RBARS

c     This routine computes values of variables which may be used to
c     form the elements of the rbar matrix, where RBAR represents
c     reflection of an elm wave from the earth's surface.  Namely,
c     rbar11=den11/(c*num11-den11) or (1./rbar11+1.)/c=num11/den11 and
c     rbar22=den22/(c*num22-den22) or (1./rbar22+1.)/c=num22/den22.
c     Derivatives wrt c=COS(THETA) are also computed if IDERIV is set
c     non-zero.  Note that the equations are formulated in such a way
c     that a smooth transition is made from the 'curved earth' form to
c     the 'flat earth' form.  See also notes in routine MF_MDHNKL
c     regarding definitions of Hankel function parameters.  The value
c     of earth's radius is such as to make 2./re=3.14e-4.  Computation
c     at THETA=90 is not excluded.

      parameter     (mxeigen=50)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex
      real           lat,lon,lub,kvratt,n0sq,ndsq
      complex        eigen,theta,c,s,csq,ssq,ngsq,sqr,
     &                nm11, nm22, dn11, dn22,
     &               num11,num22,den11,den22,
     &                dnm11, dnm22, ddn11, ddn22,
     &               dnum11,dnum22,dden11,dden22,
     &               k1,k2,k10,k1d,k20,k2d,a0,ad,
     &               f,f0,fd,f0d,dq,df,df0,dfd,df0d,
     &               q0,h10,h20,h10p,h20p,e0,
     &               qd,h1d,h2d,h1dp,h2dp,ed,
     &               f1,f2,hg,exd,z1,z2,rbar11,rbar22

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rbrs/ num11, num22, den11, den22,
     &               dnum11,dnum22,dden11,dden22,f1,f2,hg


c     Computation which is independent of THETA.

      ngsq=CMPLX(epsr,-sigma/(omega*8.85434e-12))

      arg=3.14e-4/wavenr
      avrkot=EXP(LOG(arg)/3.)
      kvratt=1./avrkot**2
      k1=CMPLX(0.,avrkot)
      k2=CMPLX(0.,.5*arg)

      em0=-3.14e-4*h
      n0sq=1.+em0
      k10=k1/n0sq
      k20=k2/n0sq
      emd= 3.14e-4*(d-h)
      ndsq=1.+emd
      k1d=k1/ndsq
      k2d=k2/ndsq

      c=COS(theta*(.01745329252,0.))
      s=SIN(theta*(.01745329252,0.))
      csq=c*c
      ssq=s*s

      dq=(2.*kvratt)*c
      sqr=SQRT(ngsq-ssq)

      num11=(1.,0.)
      num22=(1.,0.)/sqr
      den11=(c-sqr/ngsq)*(.5,0.)
      den22=(c/sqr-(1.,0.))*(.5,0.)
      if (ideriv .eq. 1) then
         dnum11=(0.,0.)
         dnum22=-c/sqr**3
         dden11=((1.,0.)-c/(sqr*ngsq))*(.5,0.)
         dden22=(-csq/sqr**3+(1.,0.)/sqr)*(.5,0.)
      end if

      if (AIMAG(theta) .gt. -10.) then

         a0=c+k20
         ad=c+k2d

         q0=kvratt*(csq+em0)
         qd=kvratt*(csq+emd)

         if (REAL(q0) .le. 0.0 .and. REAL(qd) .le. 0.0 .and.
     &       ABS(q0) .gt. 4.2 .and. ABS(qd) .gt. 4.2) then
            ngboth = 1
         else
            ngboth = 0
         end if

         call MF_MDHNKL (q0,h10,h20,h10p,h20p,e0,ngboth)
         call MF_MDHNKL (qd,h1d,h2d,h1dp,h2dp,ed,ngboth)

         h1d =h1d *EXP(e0-ed)
         h1dp=h1dp*EXP(e0-ed)
         h2d =h2d *EXP(ed-e0)
         h2dp=h2dp*EXP(ed-e0)

         f  =h10 *h2d -h20 *h1d
         f0 =h10p*h2d -h20p*h1d
         fd =h10 *h2dp-h20 *h1dp
         f0d=h10p*h2dp-h20p*h1dp

         df  = (f0+fd)*dq
         df0 = (f0d-q0*f)*dq
         dfd = (f0d-qd*f)*dq
         df0d=-(qd*f0+q0*fd)*dq

         nm11 = num11
         nm22 = num22
         dn11 = den11
         dn22 = den22
         dnm11=dnum11
         dnm22=dnum22
         ddn11=dden11
         ddn22=dden22

         den11=-nm11*(a0*ad*f+k1d*a0*fd+k10*ad*f0+k10*k1d*f0d)
     &         +2.*dn11*(ad*f+k1d*fd)
         den22=-nm22*( c* c*f+k1 *c *fd+k1 *c *f0+k1 *k1 *f0d)
     &         +2.*dn22*( c*f+k1 *fd)
         num11=-2.*nm11*(a0*f+k10*f0)+4.*dn11*f
         num22=-2.*nm22*( c*f+k1 *f0)+4.*dn22*f
         if (ideriv .eq. 1) then
            dden11=-dnm11*(a0*ad*f+k1d*a0*fd+k10*ad*f0-k10*k1d*f0d)
     &             -nm11*((a0+ad)*f+a0*ad*df
     &                    +k1d*(fd+a0*dfd)+k10*(f0+ad*df0)+k10*k1d*df0d)
     &             +2.*ddn11*(ad*f+k1d*fd)+2.*dn11*((f+ad*df)+k1d*dfd)
            dden22=-dnm22*( c* c*f+k1 * c*fd+k1 * c*f0-k1 *k1 *f0d)
     &             -nm22*(( c+ c)*f+ c* c*df
     &                    +k1 *(fd+ c*dfd)+k1 *(f0+ c*df0)+k1 *k1 *df0d)
     &             +2.*ddn22*( c*f+k1 *fd)+2.*dn22*((f+ c*df)+k1 *dfd)
            dnum11=-2.*dnm11*(a0*f+k10*f0)-2.*nm11*((f+a0*df)+k10*df0)
     &             +4.*ddn11*f+4.*dn11*df
            dnum22=-2.*dnm22*( c*f+k1 *f0)-2.*nm22*((f+ c*df)+k1 *df0)
     &             +4.*ddn22*f+4.*dn22*df

c           Following four statements needed only for excitation factors
c           fprl at z=d
            f1= nm11*f0+(nm11*a0-2.*dn11)*f/k10
c           fprp at z=d
            f2=(nm22*f0+(nm22* c-2.*dn22)*f/k1 )*sqr
c           fprl at z=0 divided by fprl at z=d
Change 22 Feb 1994: Test for F1 being zero.
            if (ABS(f1) .eq. 0.) f1=1.e-8
            hg=EXP(-1.57e-4*d)*nm11/f1*(h10p*h20-h10*h20p)
         end if
      else

         z1=(ngsq*c-sqr)/(ngsq*c+sqr)
         z2=(c-sqr)/(c+sqr)
         exd=EXP(CMPLX(0.,-d*wavenr)*c)
         f1=(1.,0.)/exd+z1*exd
         f2=(1.,0.)/exd+z2*exd
         hg=((1.,0.)+z1)/f1
         rbar11=z1*exd**2
         rbar22=z2*exd**2
      end if
      RETURN
      END      ! MF_RBARS
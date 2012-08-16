      SUBROUTINE SW_RBARS

      implicit real*8 (a-h,o-z)

c***********************************************************************

c  Change History:
c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

      parameter    (mxeigen=50)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex

      real     *  4 ranger,rangei,atnmax,lub,h
      complex  *  8 eigen

      common/lwpc_mf/
     &              ranger(2),rangei(2),atnmax,lub,h,
     &              eigen(mxeigen),nreigen

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      integer       adjflg
      complex  * 16 c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22

      common/sw_mode/
     &              omega,wn,thetar,thetai,c,s,csq,ssq,f,dfdtht,
     &              hg,norm11,norm22,norm12,rbar11,rbar22,
     &              nriter,adjflg,isotrp

      real     *  8 kvraot,ndsq,n0sq
      complex  * 16 theta,
     &              ngsq,sqroot,ikc,exd,exdsq,
     &              p0,h10,h20,h10p,h20p,
     &              pd,h1d,h2d,h1dp,h2dp,
     &              a1st,a2nd,a3rd,a4th,
     &              c1,c2,c3,c4,c5,z1,z2

      equivalence  (thetar,theta)


      ngsq=DCMPLX(DBLE(epsr),-DBLE(sigma)/(omega*8.85434d-12))
      sqroot=SQRT(ngsq-ssq)

      if (DIMAG(theta) .lt. -10.d0 .or. alpha .eq. 0.) then

c        Flat earth
         ikc=DCMPLX(0.d0,-wn)*c
         exd=EXP(ikc*botht)
         exdsq=exd*exd
         z1=(ngsq*c-sqroot)/(ngsq*c+sqroot)
         z2=(c-sqroot)/(c+sqroot)
         rbar11=z1*exdsq
         rbar22=z2*exdsq
         hg=exd*((1.d0,0.d0)+z1)/((1.d0,0.d0)+rbar11)
         norm11=((1.d0,0.d0)+rbar11)*((1.d0,0.d0)+rbar11)/exdsq
         norm22=((1.d0,0.d0)+rbar22)*((1.d0,0.d0)+rbar22)/exdsq
         norm12=((1.d0,0.d0)+rbar11)*((1.d0,0.d0)+rbar22)/exdsq
      else

         if (botht .eq. 0.d0) then

            rbar11=(ngsq*c-sqroot)/(ngsq*c+sqroot)
            rbar22=(c-sqroot)/(c+sqroot)
            hg=1.
            norm11=-2.124292958d0
            norm22=norm11
            norm12=norm11
         else

            kvraot=EXP(LOG(wn/alpha)/3.d0)
            avrkot=1.d0/kvraot

            n0sq=1.d0-alpha*h
            p0=kvraot**2*(n0sq-ssq)
            call XC_MDHNKL (p0,h10 ,h10e ,h20 ,h20e ,
     &                         h10p,h10pe,h20p,h20pe,theta,'rb 1')
            ndsq=1.d0-alpha*(h-botht)
            pd=kvraot**2*(ndsq-ssq)
            call XC_MDHNKL (pd,h1d ,h1de ,h2d ,h2de ,
     &                         h1dp,h1dpe,h2dp,h2dpe,theta,'rb 2')

cxx         a1st=h10 *h2d *EXP(h10e +h2de )-h1d *h20 *EXP(h1de +h20e )
cxx         a2nd=h10p*h2d *EXP(h10pe+h2de )-h1d *h20p*EXP(h1de +h20pe)
cxx         a3rd=h10 *h2dp*EXP(h10e +h2dpe)-h1dp*h20 *EXP(h1dpe+h20e )
cxx         a4th=h10p*h2dp*EXP(h10pe+h2dpe)-h1dp*h20p*EXP(h1dpe+h20pe)

            call XC_ADD (a1st,a1ste,h10 *h2d ,h10e +h2de ,
     &                             -h1d *h20 ,h1de +h20e )
            call XC_ADD (a2nd,a2nde,h10p*h2d ,h10pe+h2de ,
     &                             -h1d *h20p,h1de +h20pe)
            call XC_ADD (a3rd,a3rde,h10 *h2dp,h10e +h2dpe,
     &                             -h1dp*h20 ,h1dpe+h20e )
            call XC_ADD (a4th,a4the,h10p*h2dp,h10pe+h2dpe,
     &                             -h1dp*h20p,h1dpe+h20pe)

            if (ABS(a1st) .le. 1.d-30) then

               rbar11=1.
               rbar22=1.
               norm11=1.
               norm22=1.
               norm12=1.
               hg=1.
            else

               c1=DCMPLX(0.d0,-kvraot)
               c2=DCMPLX(0.d0,-avrkot)
               c3=c*ndsq
               c4=DCMPLX(0.d0,.5d0*alpha/wn)
               c5=n0sq/ngsq*sqroot

cxx            z1=c1*(c3+c4)*(c5+c4)*a1st
cxx  &           +   (c3+c4)*        a2nd
cxx  &           +           (c5+c4)*a3rd
cxx  &           -c2*                a4th

cxx            z2=c1*(c3-c4)*(c5+c4)*a1st
cxx  &           +   (c3-c4)*        a2nd
cxx  &           -           (c5+c4)*a3rd
cxx  &           +c2*                a4th

               call XC_ADD (z1,z1e,c1*(c3+c4)*(c5+c4)*a1st,a1ste,
     &                                (c3+c4)*        a2nd,a2nde)
               call XC_ADD (z1,z1e,                     z1,z1e,
     &                                        (c5+c4)*a3rd,a3rde)
               call XC_ADD (z1,z1e,                     z1,z1e,
     &                            -c2*                a4th,a4the)

               call XC_ADD (z2,z2e,c1*(c3-c4)*(c5+c4)*a1st,a1ste,
     &                                (c3-c4)*        a2nd,a2nde)
               call XC_ADD (z2,z2e,                     z2,z2e,
     &                            -           (c5+c4)*a3rd,a3rde)
               call XC_ADD (z2,z2e,                     z2,z2e,
     &                             c2*                a4th,a4the)

               rbar11=z1/z2*EXP(z1e-z2e)

cxx            z1=c1*c*sqroot*a1st
cxx  &           +   c*       a2nd
cxx  &           +     sqroot*a3rd
cxx  &           -c2*         a4th

cxx            z2=c1*c*sqroot*a1st
cxx  &           +   c*       a2nd
cxx  &           -     sqroot*a3rd
cxx  &           +c2*         a4th

               call XC_ADD (z1,z1e,c1*c*sqroot*a1st,a1ste,
     &                                c*       a2nd,a2nde)
               call XC_ADD (z1,z1e,              z1,z1e,
     &                                  sqroot*a3rd,a3rde)
               call XC_ADD (z1,z1e,              z1,z1e,
     &                            -c2*         a4th,a4the)

               call XC_ADD (z2,z2e,c1*c*sqroot*a1st,a1ste,
     &                                c*       a2nd,a2nde)
               call XC_ADD (z2,z2e,              z2,z2e,
     &                            -     sqroot*a3rd,a3rde)
               call XC_ADD (z2,z2e,              z2,z2e,
     &                             c2*         a4th,a4the)

               rbar22=z1/z2*EXP(z1e-z2e)

cxx            z1=c1*(c5+c4)*a1st+a2nd
cxx            z2=c1*sqroot *a1st+a2nd

               call XC_ADD (z1,z1e,c1*(c5+c4)*a1st,a1ste,
     &                                        a2nd,a2nde)

               call XC_ADD (z2,z2e,c1*sqroot *a1st,a1ste,
     &                                        a2nd,a2nde)

               norm11=z1*z1*EXP(z1e+z1e)
               norm22=z2*z2*EXP(z2e+z2e)
               norm12=z1*z2*EXP(z1e+z2e)

               hg=EXP(-.5*alpha*botht-z1e)*(0.d0,1.457495441040461)/z1
            end if
         end if
      end if
      RETURN
      END      ! SW_RBARS
      SUBROUTINE LW_HTGAIN
     &          (iopt,freq,sigma,epsr,alpha,
     &           mxeigen,nreigen,tp,z,hg)

c***********************************************************************

c     Calculates height gain functions for waveguide modes;
c     all three components are computed

c     iopt          output flag;
c                   =0 height gains
c                   =1 height gains normalized to z=0
c     freq          frequency in kHz;
c     sigma         ground conductivity in Siemens;
c     epsr          dielectric constant of the ground;
c     alpha         earth curvature in inverse km;
c                   (2/re) for curved earth or zero for flat earth
c     mxeigen       maximum number of modes;
c     nreigen       number of modes in the arrays;
c     tp            array of mode solutions referenced to the
c                   ground;
c     z             height at which functions are to be computed
c                   in km;
c     hg            array of height gain functions;
c                   first subscript is 1,2,3 for z,y,x components

c*******************!***************************************************

      real     *  4 ka13,ka23
      real     *  8 a1e,a2e,a3e,a4e,hg1e,hg2e,hg3e,
     &              h10e,h20e,h1p0e,h2p0e,
     &              h1ze,h2ze,h1pze,h2pze
      complex  *  8 tp(mxeigen),hg(3,mxeigen),
     &              c,ssq,ngsq,sqroot,ratio,expz,
     &              hg0/(0.,1.45749544)/
      complex  * 16 thetap,a1,a2,a3,a4,hg1,hg2,hg3,t1,t2,t3,t4,
     &              p0,h10,h20,h1p0,h2p0,
     &              p1,h1z,h2z,h1pz,h2pz


      wn=2.0958426e-02*freq
      ak=alpha/wn
      ak13=EXP(LOG(ak)/3.)
      ak23=ak13**2
      ka13=1./ak13
      ka23=ka13**2
      ngsq=CMPLX(epsr,-sigma/(5.5633459e-8*freq))

      p1=CMPLX(ka23*alpha*z,0.)
      expz=EXP(.5*alpha*z)

      do neigen=1,nreigen
         thetap=tp(neigen)
         ssq=CSIN(tp(neigen)*(.01745329252,0.))**2
         sqroot=CSQRT(ngsq-ssq)
         if(AIMAG(tp(neigen)) .le. -10. .or. alpha .eq. 0.) then

            c=CSQRT((1.,0.)-ssq)
            expz=CEXP(CMPLX(0.,wn*z)*c)
            a1=(ngsq*c-sqroot)/(ngsq*c+sqroot)
            a2=(c-sqroot)/(c+sqroot)
            hg(1,neigen)= expz+a1/expz
            hg(2,neigen)= expz+a2/expz
            hg(3,neigen)=(expz-a1/expz)*c
            if(iopt .eq. 2) then
               hg(1,neigen)=hg(1,neigen)/( (1.,0.)+a1)
               hg(2,neigen)=hg(2,neigen)/( (1.,0.)+a2)
               hg(3,neigen)=hg(3,neigen)/(((1.,0.)-a1)*c)
            end if
         else

            p0=CMPLX(ka23,0.)*((1.,0.)-ssq)
            call XC_MDHNKL (p0   ,h10 ,h10e ,h20 ,h20e ,
     &                            h1p0,h1p0e,h2p0,h2p0e,thetap,'HG 1')
            call XC_MDHNKL (p0+p1,h1z ,h1ze ,h2z ,h2ze ,
     &                            h1pz,h1pze,h2pz,h2pze,thetap,'HG 2')

c           a1=h10 *h2z -h1z *h20
c           a2=h1p0*h2z -h1z *h2p0
c           a3=h10 *h2pz-h1pz*h20
c           a4=h1p0*h2pz-h1pz*h2p0

            call XC_ADD (a1,a1e,h10 *h2z ,h10e +h2ze ,
     &                         -h1z *h20 ,h1ze +h20e )
            call XC_ADD (a2,a2e,h1p0*h2z ,h1p0e+h2ze ,
     &                         -h1z *h2p0,h1ze +h2p0e)
            call XC_ADD (a3,a3e,h10 *h2pz,h10e +h2pze,
     &                         -h1pz*h20 ,h1pze+h20e )
            call XC_ADD (a4,a4e,h1p0*h2pz,h1p0e+h2pze,
     &                         -h1pz*h2p0,h1pze+h2p0e)

            ratio=sqroot/ngsq

c           hg1=                    expz*((.5*ak23-i*ka13*ratio)*a1+a2)
c           hg3=-.5*i*ak*hg1-i*ak13*expz*((.5*ak23-i*ka13*ratio)*a3+a4)
c           hg2=-i*ka13*sqroot*a1+a2

            t1=.5*ak23-ka13*ratio*(0.d0,1.d0)
            t2=-.5*ak*(0.d0,1.d0)
            t3=-ak13*(0.d0,1.d0)
            t4=-ka13*sqroot*(0.d0,1.d0)

            call XC_ADD (hg1,hg1e,expz*t1*a1,a1e,expz*a2,a2e)

            call XC_ADD (hg3,hg3e,expz*t1*a3,a3e,expz*a4,a4e)
            call XC_ADD (hg3,hg3e,t2*hg1,hg1e,t3*hg3,hg3e)

            call XC_ADD (hg2,hg2e,t4*a1,a1e,a2,a2e)

            hg(1,neigen)=hg1*DEXP(hg1e)
            hg(2,neigen)=hg2*DEXP(hg2e)
            hg(3,neigen)=hg3*DEXP(hg3e)
            if(iopt .eq. 2) then
               hg(1,neigen)=hg(1,neigen)/hg0
               hg(2,neigen)=hg(2,neigen)/hg0
               hg(3,neigen)=hg(3,neigen)/(ratio*hg0)
            end if
         end if
      end do

      RETURN
      END      ! LW_HTGAIN
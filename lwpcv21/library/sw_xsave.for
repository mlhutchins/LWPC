      SUBROUTINE SW_XSAVE

c***********************************************************************

c     This routine is called after execution of SW_WVGD.
c     It updates the data sets used to do the quadratic extrapolation.

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     26 Apr 96     Changed to generic functions.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

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

      complex  *  8 tb,eigens
      real     *  4 xs

      common/sw_extr/
     &              tb,eigens(3,mxeigen),xs(3),ls

      integer       print_swg
      real     *  4 drmin,drmax

      common/sw_path/
     &              drmin,drmax,mdir,lostm,lx,print_swg

      integer       rpoly
      real     *  4 dtheta,tol,deigen,thtinc,ftol,alpha,prec

      common/sw_wgin/
     &              dtheta(2),tol(2),deigen(2),
     &              thtinc,ftol,maxitr,alpha,prec,rpoly,nrtlst

      complex  *  8 tp,capt,fofr

      common/sw_wgou/
     &              tp(mxeigen),capt(4,mxeigen),fofr(mxeigen),lu_mds

      complex  *  8 stb,ngsq,zdtr

      data          zdtr/(.01745329252,0.)/


      capk=SQRT(1.-alpha*h)
      coeff=-182.0428*freq
      x=rho
      if (lx .lt. 3) then
         lx=lx+1
         if (lx .eq. 1) then
c           Get Brewster mode
            if (sigma .lt. 1.e-3) then
               ngsq=CMPLX(epsr,-1.7975e7*sigma/freq)
               stb=SQRT(ngsq/(1.+ngsq))
               atten=coeff*AIMAG(stb)
               if (atten .le. atnmax) then
c                 The Brewster mode is contained within the normal set.
                  tb=(0.,0.)
               else
c                 The Brewster mode is outside the normal set.
                  if (atten .le. 2.*atnmax) then
c                    The attenuation rate is not excessive.
c                    Reference Brewster mode to H
Change: 09 Oct 92
c                    stb=stb/capk
                     stb=stb*capk
Change: END
                     tb=(0.,-1.)/zdtr
     &                 *LOG(SQRT((1.,0.)-stb**2)+(0.,1.)*stb)
                  else
c                    The attenuation rate is excessive.
                     tb=(0.,0.)
                  end if
               end if
            else
               atten=0.
               tb=(0.,0.)
            end if
            if (print_swg .gt. 2)
     &         write(lwpcLOG_lun,
     &             '(''[SW_XSAVE]: Brewster '',2f8.3,f7.1)') tb,atten
         end if
      else
         do l=1,2
            xs(l)=xs(l+1)
            do k=1,nreigen
               eigens(l,k)=eigens(l+1,k)
            end do
         end do
      end if
      ls=nreigen
      xs(lx)=x
      do k=1,nreigen
         eigens(lx,k)=eigen(k)
      end do

      k=1
      do while (k .le. nreigen)
c        If this mode is not near the Brewster,
c        then check its attenuation rate
         if (ABS( REAL(eigen(k)-tb)) .gt. deigen(1) .or.
     &       ABS(AIMAG(eigen(k)-tb)) .gt. deigen(2)) then
            atten=coeff*AIMAG(SIN(eigen(k)*zdtr))
            if (atten .gt. atnmax) then
c              Delete k-th mode
               if (print_swg .gt. 2)
     &            write(lwpcLOG_lun,
     &                '(''[SW_XSAVE]: Deleted'',i3,'':'',2f8.3)')
     &                  k,eigen(k)
               nreigen=nreigen-1
               if (nreigen .eq. 0) then
c                 All modes have been deleted
                  if (print_swg .gt. 2)
     &               write(lwpcLOG_lun,
     &                   '(''[SW_XSAVE]: All modes deleted'')')
                  lostm=2
                  eigen(1)=(0.,0.)
                  RETURN
               else
c                 Check the remaining modes
                  do l=k,nreigen
                     eigens(1,l)=eigens(1,l+1)
                     eigens(2,l)=eigens(2,l+1)
                     eigens(3,l)=eigens(3,l+1)
                     eigen (  l)=eigen (  l+1)
                     tp    (  l)=tp    (  l+1)
                     capt  (1,l)=capt  (1,l+1)
                     capt  (2,l)=capt  (2,l+1)
                     capt  (3,l)=capt  (3,l+1)
                     capt  (4,l)=capt  (4,l+1)
                     fofr  (  l)=fofr  (  l+1)
                  end do
                  eigen(nreigen+1)=(0.,0.)
                  k=k-1
               end if
            end if
         end if
         k=k+1
      end do
      ls=nreigen
      RETURN
      END      ! SW_XSAVE
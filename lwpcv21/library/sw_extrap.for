      SUBROUTINE SW_EXTRAP

c***********************************************************************

c     This routine sets up and maintains the data sets for the quadratic
c     extrapolation of EIGENs down the propagation path.

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     26 Apr 96     Changed to generic functions.

c     14 Feb 98     Test for too large a change in extrapolated modes.

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

      complex  *  8 s

      data          dtr/.01745329252/


      if (lx .eq. 0) RETURN

      capk=SQRT(1.-alpha*h)
      coeff=-182.0428*freq
      x=rho

      nreigen=ls
      do k=1,nreigen
         s=(0.,0.)
         do l1=1,lx
            p=1.
            do l2=1,lx
               if (l1 .ne. l2) p=p*(x-xs(l2))/(xs(l1)-xs(l2))
            end do
            s=s+p*eigens(l1,k)
         end do
         eigen(k)=s
      end do
      if (print_swg .gt. 2) then
         write(lwpcLOG_lun,
     &       '(''[SW_EXTRAP]: EIGENs for lx/x='',i1,''/'',f6.0)') lx,x
         do k=1,nreigen
            s=eigen(k)
            atten=coeff/capk*AIMAG(SIN(s*dtr))
            if (ABS( REAL(s-tb)) .gt. deigen(1) .or.
     &          ABS(AIMAG(s-tb)) .gt. deigen(2)) then
               write(lwpcLOG_lun,
     &             '(i3,5x,2f8.3,f7.1)') k,s,atten
            else
               write(lwpcLOG_lun,
     &             '(i3,5x,2f8.3,f7.1,'' Brewster'')') k,s,atten
            end if
         end do
      end if

c     Scan the extrapolated EIGENs for invalid values.
      do k=1,nreigen
         if (REAL(eigen(k)) .le. 0. .or. REAL(eigen(k)) .ge. 90. .or.
     &      AIMAG(eigen(k)) .ge. 0.) then
            if (print_swg .gt. 2)
     &      write(lwpcLOG_lun,
     &          '(''[SW_EXTRAP] ERROR: '',
     &            ''Invalid extrapolated mode'',i3)') k
            lostm=2
            RETURN
         end if
      end do

c     Look for modes significantly different from the previous set
      if (lx .gt. 1) then
         do l1=1,nreigen
c           Calculate average value of previous solutions
            s=(0.,0.)
            do k=1,lx
               s=s+eigens(k,l1)
            end do
            s=s/lx
            if (ABS( REAL(s)- REAL(eigen(l1))) .ge. 1. .and.
     &          ABS(AIMAG(s)-AIMAG(eigen(l1))) .ge. .5) then
               if (print_swg .gt. 2)
     &         write(lwpcLOG_lun,
     &             '(''[SW_EXTRAP] ERROR: '',
     &               ''Change too large in mode'',i3)') l1
               lostm=5
               RETURN
            end if
         end do
      end if

c     Look for duplicate modes
      do l1=1,nreigen-1
         thtr= REAL(eigen(l1))
         thti=AIMAG(eigen(l1))
         do l2=l1+1,nreigen
            if (ABS(thtr- REAL(eigen(l2))) .le. deigen(1) .and.
     &          ABS(thti-AIMAG(eigen(l2))) .le. deigen(2)) then
               if (print_swg .gt. 2)
     &         write(lwpcLOG_lun,
     &             '(''[SW_EXTRAP] ERROR: '',
     &               ''Duplicate extrapolated modes'',2i3)') l1,l2
               lostm=2
               RETURN
            end if
         end do
      end do
      if (nreigen .lt. mxeigen) eigen(nreigen+1)=(0.,0.)
      RETURN
      END      ! SW_EXTRAP
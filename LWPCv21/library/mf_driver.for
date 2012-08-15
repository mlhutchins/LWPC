      SUBROUTINE MF_DRIVER

c***********************************************************************

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex,autos,rprnt,xprnt
      real           lat,lon,lub
      complex        eigen,theta,c,s,csq,ssq

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_area/gmax,bxrtio,tmesh,autos
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      omega=6.283185308e3*freq
      wavenr=omega/2.997928e5
      if (freq .le. 29.9) then
         gmax=3.
      else
         if (freq .le. 99.9) then
            gmax=2.
         else
            gmax=1.
         end if
      end if
      tmesh=SQRT(1./freq)
      lub=SQRT(15./freq)/100.

c      if (lprnt .ne. 0 .or. mrprnt .ne. 0) then
c         write(lwpcLOG_lun,
c     &       '(''MF_DRIVER: gmax= '',f6.3,4x,''bxrtio='',f6.4)')
c     &         gmax,bxrtio
c         write(lwpcLOG_lun,
c     &       '(''MF_DRIVER: tmesh='',f6.3,4x,''lub=   '',f6.4)')
c     &         tmesh,lub
c      end if
c   MLH commented out for Matlab processing of log file

      if (ranger(1) .gt. ranger(2)) then
         temp=ranger(1)
         ranger(1)=ranger(2)
         ranger(2)=temp
      end if
      if (rangei(1) .lt. rangei(2)) then
         temp=rangei(1)
         rangei(1)=rangei(2)
         rangei(2)=temp
      end if

      call MF_WVGD
 			
      RETURN
      END      ! MF_DRIVER
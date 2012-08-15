      SUBROUTINE MF_INITEV

c***********************************************************************

c     HTNTRP for the condition that the electron density is negligible
c     is found.

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
      character*200 error_msg
      integer       pflag,pindex,rprnt,xprnt
      real          freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              ranger,rangei,atnmax,lub,h,
     &              rtol,xintrp,htntrp,d,z0,zz,
     &              hofwr,topht,botht,
     &              dcl,dcm,dcn,ec,omega,wavenr,
     &              ht,en,nu,cx,x,cy,y,coefen,
     &              gthrsh,re

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_drcs/dcl,dcm,dcn,ec,g,q
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rseq/r0(4),drdt(4),d2rdt2(4),d3rdt3(4),
     &               zetap(2),gammap(2),theta0,extra

      dimension     en(3),nu(3)

      data          re/6369.427/,cx/3.182357e9/,cy/1.758796e7/,
     &              gthrsh/0.1/


c     The height HTNTRP at which cap X (as defined by Budden) is equal
c     to the input or default value of X = Xinterp. It is used as a
c     default value of the reference height, D, in LAGRNG in those cases
c     in which the usual procedure of that routine for finding an
c     appropriate reference height cannot be used and REFHT is not used.
c     It is always the reference height used in MF_INTROE.

      coefen=cx/omega**2
      ht=botht
      x=0.
      do while (x .lt. xintrp)

         call PRFL_ENNU (ht,en,nu)

         x=coefen*en(1)
c         if (mrprnt .ne. 0)
c     &      write(lwpcLOG_lun,
c     &          '(''MF_INITEV: '',
c     &            ''ht, en, x='',f7.2,1p2e10.2)')
c     &              ht,en(1),x
c         if (ht .eq. topht) then
c         write(error_msg,
c     &          '(''[MF_INITEV]: '',
c     &            ''HTNTRP cannot be set'')')
c            call LWPC_ERROR ('ERROR',error_msg)
c         end if
c   MLH commented out for Matlab processing of log file

         ht=ht+1.
      end do
      htntrp=ht-2.
c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_INITEV: '',
c     &         ''htntrp='',f7.2)')
c     &           htntrp
c   MLH commented out for Matlab processing of log file

      call PRFL_ENNU (htntrp,en,nu)
      z=nu(1)/omega

      y=-cy*bfield/omega
      g=y/(z+(0.,1.))
c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_INITEV: '',
c     &         ''g='',1p2e10.2)')
c     &           g
c   MLH commented out for Matlab processing of log file

      ec=2.*(htntrp-h)/re

c     If the magnitude of G is very small, that is, if the collision
c     frequency at HTNTRP is large, MF_INTROE is not used, that is,
c     magnetoionic reflection coefficients are not used. In this case
c     interpolation is done entirely in MF_LAGRNG.

      if (ABS(g) .lt. gthrsh) then
         lowg=1
      else
         lowg=0
         call MF_BRNCPT
      end if
      RETURN
      END      ! MF_INITEV
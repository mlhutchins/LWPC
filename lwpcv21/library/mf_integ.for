      SUBROUTINE MF_INTEG

c***********************************************************************

c     This routine performs an integration of the differential equations
c     for the ionospheric reflection matrix using Runge-Kutta
c     integration formulas.  The integration variables are the elements
c     of the matrix (R+1)/C where R is the reflection matrix described
c     by Budden.  If IDERIV is set non-zero, the derivatives of (R+1)/C
c     elements wrt c=COS(THETA) are also integration variables.  The
c     set of heights at which pairs of integration steps begin and end
c     is stored in the array 'RKHTS'.  Sizes of the steps are determined
c     by comparing the values of the elements of (R+1)/C after a pair
c     of steps and values of the same variables after a single double-
c     size comparison step.  The (R+1)/C variables used in the
c     comparison step are denoted as 'X'.  If the difference is too
c     large, the step size pairs are cut in half by adding a new ht to
c     the list in 'RKHTS'.  Up if two successive differences are very
c     small the intervening height is deleted from the list of heights.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxkhts=401)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      integer        pflag,pindex,rprnt,xprnt
      real           freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,
     &               rtol,xintrp,htntrp,d,z0,zz,
     &               omega,wavenr,ht,rkhts,r parts,x parts,
     &               delh,rmag,err,e,ep
      complex        mmatrix

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_lgfl/lgflag(4)
     &      /mf_mmtx/mmatrix(9),ht
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_rkht/rkhts(mxkhts),nrkhts
     &      /mf_rntg/r(8),drdh(8),roe(8),rnd(5)
     &      /mf_xntg/x(4),dxdh(4)

      dimension      r0(8),hdelr0(8),delr1(8),delr2(8),r parts(8),
     &               x0(4),hdelx0(4),delx1(4),delx2(4),x parts(8),
     &               rsave(8)

      equivalence   (r,r parts),(x,x parts)

      data           third/(.3333333333,0.)/


c     Initialization
      nointg=0
      if (ideriv .ne. 0) then
         n=8
      else
         n=4
      end if
      ep=99.9
      iovflo=0
      if (rprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_INTEG:  ht'',10x,''11r11'',16x,''11r1'',16x,
     &         ''1r11'',17x,''1r1'')')

c     Initialization for given value of THETA.
      c=COS(theta*0.01745329252)
      s=SIN(theta*0.01745329252)
      csq=c**2
      ssq=s**2

      jrk=1
      ht=topht
      call MF_SMTRX
      call MF_INITR
      if (rprnt .ne. 0) then
         do i=1,8
            if (ABS(r parts(i)) .ge. 100.) r parts(i)=99.99999
            if (ABS(x parts(i)) .ge. 100.) x parts(i)=99.99999
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,x parts
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,r parts
      end if

      lgflag(1)=0
      lgflag(2)=0
      lgflag(3)=0
      lgflag(4)=0
      call MF_CHEKLG (r,ideriv)

      call MF_XFER (r,x,4)
      if (rprnt .ne. 0) then
         do i=1,8
            if (ABS(r parts(i)) .ge. 100.) r parts(i)=99.99999
            if (ABS(x parts(i)) .ge. 100.) x parts(i)=99.99999
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,x parts
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,r parts
      end if

c     Initialization after any successful pair of steps
20    call MF_XFER (r,rsave,n)
      do i=1,4
         if (lgflag(i) .ne. 0 .and. REAL(r(i))+0.5 .gt. 10.) then
c            write(lwpcLOG_lun,
c     &          '(''MF_INTEG: At theta='',2f7.2,'' magnitude of '',
c     &            ''an element of R greater than EXP(10)'')')
c     &              theta
c MLH Commented out above error for consistant log file
            nointg=1
            RETURN
         end if
      end do

      ht=rkhts(jrk)
      call MF_XFER (r,x,4)

c     Initialization after any reduction of step size
25    delh=(rkhts(jrk+1)-rkhts(jrk))*0.5
      dh=delh
      hdh=dh*(.5,0.)
      tdh=dh*(2.,0.)

c     Begin first Runge-Kutta step of the pair and the comparison step.
      call MF_RDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         r0(i)=r(i)
         hdelr0(i)=drdh(i)*hdh
         r(i)=r0(i)+hdelr0(i)
      end do

      do i=1,4
         x0(i)=x(i)
         hdelx0(i)=hdelr0(i)*2.
         x(i)=x0(i)+hdelx0(i)
      end do

      ht=ht+0.5*delh
      call MF_SMTRX
      call MF_RDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         delr1(i)=drdh(i)*dh
         r(i)=r0(i)+0.5*delr1(i)
      end do

      call MF_RDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         delr2(i)=drdh(i)*dh
         r(i)=r0(i)+delr2(i)
      end do

      ht=ht+0.5*delh
      call MF_SMTRX
      call MF_RDERIV
      call MF_XDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         hdelr3=drdh(i)*hdh
         delr4=(hdelr0(i)+delr1(i)+delr2(i)+hdelr3)*third
         r(i)=r0(i)+delr4
      end do

c     Begin second Runge-Kutta step of the pair and continuation of
c     the comparison step.
      do i=1,4
         delx1(i)=dxdh(i)*tdh
         x(i)=x0(i)+0.5*delx1(i)
      end do

      call MF_RDERIV
      call MF_XDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         r0(i)=r(i)
         hdelr0(i)=drdh(i)*hdh
         r(i)=r0(i)+hdelr0(i)
      end do

      do i=1,4
         delx2(i)=dxdh(i)*tdh
         x(i)=x0(i)+delx2(i)
      end do

      ht=ht+0.5*delh
      call MF_SMTRX
      call MF_RDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         delr1(i)=drdh(i)*dh
         r(i)=r0(i)+0.5*delr1(i)
      end do

      call MF_RDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         delr2(i)=drdh(i)*dh
         r(i)=r0(i)+delr2(i)
      end do

      ht=ht+0.5*delh
      call MF_SMTRX
      call MF_RDERIV
      call MF_XDERIV
      if (iovflo .ne. 0) go to 60

      do i=1,n
         hdelr3=drdh(i)*hdh
         delr4=(hdelr0(i)+delr1(i)+delr2(i)+hdelr3)*third
         r(i)=r0(i)+delr4
      end do

      do i=1,4
         hdelx3=dxdh(i)*dh
         delx4=(hdelx0(i)+delx1(i)+delx2(i)+hdelx3)*third
         x(i)=x0(i)+delx4
      end do

c     Estimate truncation error
      if (rprnt .ne. 0) then
         do i=1,8
            if (ABS(r parts(i)) .ge. 100.) r parts(i)=99.99999
            if (ABS(x parts(i)) .ge. 100.) x parts(i)=99.99999
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,x parts
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,r parts
      end if
      e=0.
      do i=1,4
         err=ABS(r(i)-x(i))
         rmag=ABS(r(i))
         if (rmag .gt. 1.) err=err/rmag
         if (err .gt. rtol) go to 61
         if (e .lt. err) e=err
      end do
      go to 70

c     If truncation error is too large, reduce step size by adding a
c     height to the list.
60    if (rprnt .ne. 0) then
         do i=1,8
            if (ABS(r parts(i)) .ge. 100.) r parts(i)=99.99999
            if (ABS(x parts(i)) .ge. 100.) x parts(i)=99.99999
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INTEG:  '',f7.2,4(2x,2f9.5))')
     &           ht,x parts
         write(lwpcLOG_lun,
     &       '(''MF_INTEG:  '',f7.2,4(2x,2f9.5))')
     &           ht,r parts
      end if
61    if (rprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_INTEG: Overflow'')')
      if (nrkhts .ge. mxkhts) then
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: Too many steps'')')
         nointg=1
         rkhts(1)=topht
         rkhts(2)=botht
         nrkhts=2
         RETURN
      end if
      iovflo=0
      call MF_XFER (rsave,r,n)
      call MF_XFER (rsave,x,4)
      ht=rkhts(jrk)
      call MF_SMTRX

      do jj=nrkhts,jrk+1,-1
         rkhts(jj+1)=rkhts(jj)
      end do
      rkhts(jrk+1)=(rkhts(jrk)+rkhts(jrk+2))*0.5
      nrkhts=nrkhts+1
      go to 25

70    call MF_CHEKLG (r,ideriv)
c     If truncation error is very small, increase step size by deleting
c     a height from the list.
      if (e .le. 0.01*rtol .and. ep .le. 0.03*rtol) then
         do jj=jrk,nrkhts-1
            rkhts(jj)=rkhts(jj+1)
         end do
         jrk=jrk-1
         nrkhts=nrkhts-1
         e=99.9
      end if
      ep=e
      jrk=jrk+1
      if (jrk .lt. nrkhts) go to 20

      call MF_FINLLG (r,ideriv)
      call MF_FINLLG (x,0)
      if (rprnt .ne. 0) then
         do i=1,8
            if (ABS(r parts(i)) .ge. 100.) r parts(i)=99.99999
            if (ABS(x parts(i)) .ge. 100.) x parts(i)=99.99999
         end do
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,x parts
         write(lwpcLOG_lun,
     &       '(''MF_INTEG: '',f7.2,4(2x,2f9.5))')
     &           ht,r parts
      end if
      RETURN
      END      ! MF_INTEG
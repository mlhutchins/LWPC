      SUBROUTINE MF_BRNCPT

c***********************************************************************

c     Branch points of ZETA as defined by Eq. 3 of NOSC TR 1143 are
c     found. At these points in the THETA plane, the ordinary wave
c     cannot be distinguished from the extraordinary wave. Therefore,
c     magnetoionic reflection coefficients are not defined.

c     Closed solution version.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      integer        rprnt,xprnt
      real           dcl,dcm,dcn,ec,
     &               dclsq,dcmsq,dcnsq,psq,p,
     &               argln,qmag,zsqmgp,zsqmgm,zsqmag

      common/mf_bpts/bpt(6),nobpts
     &      /mf_drcs/dcl,dcm,dcn,ec,g,q
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      dimension      t(4),qq(4)


c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_BRNCPT: Closed solution for branch points'')')
c   MLH commented out for Matlab processing of log file

      nobpts=0

      dclsq=dcl**2
      dcmsq=dcm**2
      dcnsq=dcn**2
      psq=1.+ec
      p=SQRT(psq)

c     Two solutions to the equation
c        (p**2-t(i)**2)*G (+/-) 2*p*t(i) = 0
c     If t(i)=l*s(i)+n*q(i), this is the condition that ZETA in TR 1143
c     is zero. The other two solutions are the negative of these two.

      t(1)=p*((1.,0.)+SQRT((1.,0.)+g**2))/g
      t(2)=p*((1.,0.)-SQRT((1.,0.)+g**2))/g

c     Solutions to the equation
c        l*S(i)+n*q(i) = t(i)
c     where
c        S(i)**2+q(i)**2 = p**2.
c     Only solutions for which the real part of S(i) is positive are
c     included. The solution replaces the iteration described in Section
c     IV of TR 1143.

      i=1
      do ii=1,2

         rtterm=dcl*SQRT((dclsq+dcnsq)*psq-t(ii)**2)
         qp=(dcn*t(ii)+rtterm)/(dclsq+dcnsq)

         do nrt=1,2

            qsq=qp**2
            s=SQRT(psq-qsq)
            if (REAL(s) .lt. 0.) s=-s

c           A sign ambiguity in q(i) is resolved by substituting back
c           into the equation for ZETA squared (See Eq. 3 of TR 1143).
            a= dcl*s+dcn*qp
            b=-dcn*s+dcl*qp
            zsq=((b**2+dcmsq*psq)*g)**2-4.*a**2*psq
            zsqmgp=ABS(zsq)

            qm=-qp
            a= dcl*s+dcn*qm
            b=-dcn*s+dcl*qm
            zsq=((b**2+dcmsq*psq)*g)**2-4.*a**2*psq
            zsqmgm=ABS(zsq)

            if (zsqmgp .lt. zsqmgm) then
               qq(i)=qp
               zsqmag=zsqmgp
            else
               qq(i)=qm
               zsqmag=zsqmgm
            end if

c           The branch points are found. The condition that the real
c           part of C must have the same sign as the real part of q
c           is included.

            csq=qq(i)**2-ec
            c=SQRT(csq)
            if (REAL(qq(i)) .lt. 0.) c=-c

            tcompl=(0.,-1.)*LOG(s+(0.,1.)*c)
            bpt(i)=(90.,0.)-tcompl*(57.296,0.)

c            if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &         write(lwpcLOG_lun,
c     &             '(''MF_BRNCPT: bpt='',2f10.3,'' q='',1p2e10.3,1x,
c     &               ''zsqmag='',e10.3)')
c     &                 bpt(i),qq(i),zsqmag
c   MLH commented out for Matlab processing of log file

            i=i+1
            qp=(dcn*t(ii)-rtterm)/(dclsq+dcnsq)
         end do
      end do

c     THETA = 90 is added to the list of points near which magnetoionic
c     reflection coefficients are no to be used.

      bpt(5)=90.

c     The point nearest the search area at which q = 0 is added to the
c     list of points near which the magnetoionic reflection coefficients
c     are not to be used.

      if (ec .le. 0.) then
         qpt=CMPLX(ACOS(SQRT(-ec))*57.296,0.)
      else
         argln=SQRT(ec)+SQRT(1.+ec)
         qpt=(90.,0.)+(0.,-1.)*LOG(argln)*(57.296,0.)
      end if
      bpt(6)=qpt
      qtest=SQRT(COS(qpt*.01745329252)**2+ec)
      qmag=ABS(qtest)

c      if (lprnt .ne. 0 .or. mrprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_BRNCPT: qmag='',1pe9.2,'' at theta='',0p2f7.3)')
c&           qmag,qpt

c   MLH commented out for Matlab processing of log file
      RETURN
      END      ! MF_BRNCPT
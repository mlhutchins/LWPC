      SUBROUTINE MF_FZEROS

c***********************************************************************

c     This routine finds the zeros of the analytic function, F, within
c     a specified search rectangle using a mesh scheme. It is assummed
c     that there are no poles in F within or near the rectangle and no
c     more that two zeros near the same location. First derivatives of
c     F are used but only for refinement. Triangular mesh units are
c     used in contrast to the square units used in an earlier scheme.
c     The zeros are found along lines of constant pahsse: real(F)=0.

c     ERRATUM: Section IV of NOSC TR 1143 erroneously stated that lines
c     of constant phase are for imag(F)=0.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     25 Nov 96     Changed name of local UNITI to prevent changing the
c                   value of a variable in common.
c
c 3/17/10 MLH Commented out some error reporting to give a consistant log file
c
c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50,mxeigenb=25,mxboxes=200)

      integer        rprnt,xprnt
      real           lub
      complex        eigen,eigenb,theta,c,s,csq,ssq,
     &               f,dfdt,delt,thetaa,thetab,thetac,fa,fb,fc,
     &               thentr,thexit,thbsav,thzero(4),
     &               exit1(mxeigenb),exit2(mxeigenb),udfdt(mxeigenb)

      common/lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_eigb/eigenb(mxeigenb),nreigenb
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt
     &      /mf_side/tlft,trht,ttop,tbot

      data           maxtrn/500/,maxntn/10/,maxnwm/3/,eps/1.e-20/,
     &               hroot3/.866025404/


c     ADJMSR is the geometrical height of each mesh triangle and is the
c     mesh interval in the real direction. ADJMSI is the length of a
c     side of each triangle and is the mesh interval along the left and
c     right sides of the mesh triangle.

      adjmsr=adjmsh
      adjmsi=adjmsh/hroot3
      newmsh=0
      nomesh=0

10    iside=1
      nrxits=0
      linenr=1
      nreigenb=0
      nzl=0

c     The parameter INTHAF is non-zero if, as in the Fig. 3 of NOSC
c     TR 1143, the lower left corner of the mesh pattern is not a sharp
c     corner and is zero otherwise. It is used also in determining
c     whether the upper right corner of the mesh pattern is a sharp
c     corner.

Change:  25 Nov 96
cxx      Changed name of local UNITI to prevent changing the
cxx      value of a variable in common.
cxx      uniti=adjmsi*.5
cxx      inthaf=MOD(INT((ttop-tbot)/uniti+.5),2)

      height=adjmsi*.5
      inthaf=MOD(INT((ttop-tbot)/height+.5),2)

c     The upper left corner of the mesh pattern is determined and the
c     value of F is evaluated. The entire perimeter of the mesh pattern
c     is set to be somewhat outside the perimeter of the search
c     rectangle in order to ensure that all zeros in the rectangle are
c     found.

      thetaa=CMPLX(tlft-adjmsr*.5,ttop+adjmsi*.25)
      call MF_FCTVAL (thetaa,fa)
      fra= REAL(fa)
      if (fra .eq. 0.) fra=eps
      fia=AIMAG(fa)
      if (fia .eq. 0.) fia=eps

      if (mzprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_FZEROS: '',
     &         ''thetaa='',2f8.3,'' f='',1p2e10.2,i3)')
     &           thetaa,fra,fia,iside
      go to 21

c     THETAB is the lead point around the perimeter of the mesh pattern.
c     THETAA is the trailing one.

20    thetaa=thetab
      fra=frb
      fia=fib

c     ISIDE=1 at the left side of the mesh pattern

      if (iside .ne. 1) go to 22
21    thetab=thetaa+CMPLX(0.,-adjmsi)
      if (AIMAG(thetaa) .lt. tbot) iside=2
      if (iside .eq. 1) go to 27

c     ISIDE=2 at the bottom of the mesh pattern. The parameter INTHAF
c     specifies the slope of the leftmost segment of the bottom edge of
c     the mesh pattern. If the slope of a segment is downward and to the
c     right, SIGN is set to -1 and if upward and to the right, it is set
c     to 1.

      sign=1.
      if (inthaf .ne. 0) sign=-1.
22    if (iside .ne. 2) go to 23
      thetab=thetaa+CMPLX( adjmsr,sign*adjmsi*.5)
      sign=-sign
      if (REAL(thetaa) .gt. trht) iside=3
      if (iside .eq. 2) go to 27

c     ISIDE=3 at the right side of the mesh pattern.

23    if (iside .ne. 3) go to 24
      thetab=thetaa+CMPLX(0.,adjmsi)
      if (AIMAG(thetaa) .gt. ttop) iside=4
      if (iside .eq. 3) go to 27

c     ISIDE=4 at the top of the mesh pattern. If the slope of a segment
c     is downward and to the left, SIGN is set to -1 and if upward and
c     to the left, it is set to 1. The last setting is for the following
c     segment. The leftmost segment (ofthe top of the mesh pattern) is
c     always downward and to the left.

      if (inthaf .eq. 0) sign=-sign
24    thetab=thetaa+CMPLX(-adjmsr,sign*adjmsi*.5)
      sign=-sign
      if (REAL(thetaa) .lt. tlft) go to 70

c     The F function is evaluated at THETAB

27    call MF_FCTVAL (thetab,fb)
      frb= REAL(fb)
      if (frb .eq. 0.) frb=eps
      fib=AIMAG(fb)
      if (fib .eq. 0.) fib=eps

      if (mzprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_FZEROS: '',
     &         ''thetab='',2f8.3,'' f='',1p2e10.2,i3)')
     &           thetab,frb,fib,iside

c     The F function at THETAA and THETAB is tested for a change in the
c     sign of the real part. Such a change indicates a line of constant
c     phase, in particular, Re(F)=0. However, if it is the exit of a
c     line already followed, the search continues along the perimeter of
c     the mesh pattern.

      if (fra .gt. 0. .and. frb .gt. 0.) go to 20
      if (fra .lt. 0. .and. frb .lt. 0.) go to 20
      if (nrxits .gt. 0) then
         do i=1,nrxits
            if (ABS(thetaa-exit1(i)) .lt. .1*adjmsh .and.
     &          ABS(thetab-exit2(i)) .lt. .1*adjmsh) go to 20
         end do
      end if

c     Values associated with THETAB are saved for continuing along the
c     perimeter after a line of constant phase has been followed.

      thbsav=thetab
      frbsav=frb
      fibsav=fib

c     The approximate location of the line of constant pahse, Re(F)=0,
c     at the entry side of the first triangle is found by linear
c     interpolation. An approximate value of Im(F) at that point is
c     also found by linear interpolation.

      thentr=(thetaa*frb-thetab*fra)/(frb-fra)
      fientr=((thetab-thentr)*fia+(thentr-thetaa)*fib)/(thetab-thetaa)

      if (mzprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_FZEROS: '',
     &         ''line '',i2,2f8.3,3x,1p2e10.2)')
     &           linenr,thentr,fientr

      last=0
      nrtrng=0
      do while (last .eq. 0 .and. nrtrng .le. maxtrn)

c        THETAC is at the third corner of the mesh triangle. It is
c        determined from the other two corners, THETAA and THETAB. The
c        corner THETAC is always counterclockwise from THETAB and
c        THETAB, in turn, is always counterclockwise from THETAA.

         thetac=.5*(thetaa+thetab)+CMPLX(0.,-hroot3)*(thetaa-thetab)

         call MF_FCTVAL (thetac,fc)
         frc= REAL(fc)
         if (frc .eq. 0.) frc=eps
         fic=AIMAG(fc)
         if (fic .eq. 0.) fic=eps

         if (mzprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_FZEROS: '',
     &            ''thetac='',2f8.3,'' f='',1p2e10.2,i3)')
     &              thetac,frc,fic,iside

         if ((frb .gt. 0. .and. frc .gt. 0.) .or.
     &       (frb .lt. 0. .and. frc .lt. 0.)) then

            if ((frc .gt. 0. .and. fra .gt. 0.) .or.
     &          (frc .lt. 0. .and. fra .lt. 0.)) then

               if (mzprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_FZEROS: '',
     &                  ''No exit from mesh unit'')')
               nomesh=1
               go to 99
            else

               left=1
               thexit=(thetac*fra-thetaa*frc)/(fra-frc)
               fiexit=((thetaa-thexit)*fic+(thexit-thetac)*fia)
     &                /(thetaa-thetac)
            end if
         else

            left=0
            thexit=(thetab*frc-thetac*frb)/(frc-frb)
            fiexit=((thetac-thexit)*fib+(thexit-thetab)*fic)
     &             /(thetac-thetab)
         end if

         if (mzprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_FZEROS: '',
     &            ''left '',i2,2f8.3,3x,1p2e10.2)')
     &              left,thexit,fiexit

          if ((fientr .gt. 0. .and. fiexit .gt. 0.) .or.
     &        (fientr .lt. 0. .and. fiexit .lt. 0.)) go to 47
          nzl=nzl+1
          thzero(nzl)=(thentr*fiexit-thexit*fientr)/(fiexit-fientr)

          if (mzprnt .ne. 0)
     &       write(lwpcLOG_lun,
     &           '(''MF_FZEROS: '',
     &             ''thzero='',2f8.3)')
     &               thzero(nzl)
          if (nzl .gt. 3) go to 80

47        trc= REAL(thetac)
          tic=AIMAG(thetac)

          if (left .eq. 0) then
             trb= REAL(thetab)
             tib=AIMAG(thetab)
             if ((trb .lt. tlft .and. trc .lt. tlft) .or.
     &           (trb .gt. trht .and. trc .gt. trht) .or.
     &           (tib .lt. tbot .and. tic .lt. tbot) .or.
     &           (tib .gt. ttop .and. tic .gt. ttop)) then
                last=1
            else
               thentr=thexit
               fientr=fiexit
               thetaa=thetac
               fra=frc
               fia=fic
            end if
         else
            tra= REAL(thetaa)
            tia=AIMAG(thetaa)
            if ((tra .lt. tlft .and. trc .lt. tlft) .or.
     &          (tra .gt. trht .and. trc .gt. trht) .or.
     &          (tia .lt. tbot .and. tic .lt. tbot) .or.
     &          (tia .gt. ttop .and. tic .gt. ttop)) then
               last=1
            else
               thentr=thexit
               fientr=fiexit
               thetab=thetac
               frb=frc
               fib=fic
            end if
         end if
         nrtrng=nrtrng+1
      end do

      if (last .eq. 0) then

c        A loop in following a line of constant phase might occur about
c        once every ten thousand times this routine is called due to
c        round off errors in determining THETAA, THETAB and THETAC.

         write(lwpcLOG_lun,
     &       '(''MF_FZEROS: '',
     &         ''Loop in mesh unit search'')')
         go to 80
      end if

      nrxits=nrxits+1
      if (left .eq. 0) then
         exit1(nrxits)=thetab
         exit2(nrxits)=thetac
      else
         exit1(nrxits)=thetac
         exit2(nrxits)=thetaa
      end if

      if (mzprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_FZEROS: '',i4,1x,
     &         ''zeros on line number'',i3)')
     &           nzl,linenr
      if (nzl .gt. 0) then
         if (nreigenb .ge. mxeigenb) then
            if (mzprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_FZEROS: '',
     &               ''Too many zeros in the rectangle'')')
            nomesh=1
            go to 99
         end if

         if (nzl .gt. 1) then
c            if (lprnt .ne. 0 .or. mzprnt .ne. 0)
c     &        write(lwpcLOG_lun,
c     &            '(''MF_FZEROS: '',
c     &              ''On same phase line:'',4(2x,2f7.3),
c     &              '' - mesh size reduced'')')
c     &             (thzero(i),i=1,nzl)
c Edited out for consistant log file - MLH
            go to 80
         end if

         nreigenb=nreigenb+1
         eigenb(nreigenb)=thzero(1)
      end if

      linenr=linenr+1
      nzl=0

      thetab=thbsav
      frb=frbsav
      fib=fibsav
      go to 20

70    if (nreigenb .gt. 0) then

         delmax=adjmsr/float(maxntn)

         do j=1,nreigenb
            if (mzprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_FZEROS: '',i3)')
     &                 j

            last=0
            nrntn=0
            do while (last .eq. 0 .and. nrntn .lt. maxntn)
               call MF_FDFDT (eigenb(j),f,dfdt)
               if (nointg .ne. 0) go to 99

               if (mzprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_FZEROS: '',2f7.3,'' f='',1p2e10.2)')
     &                    eigenb(j),f
               delt=-f/dfdt
               absdt=ABS(delt)
               if (absdt .gt. delmax) delt=delt*delmax/ABS(delt)
               eigenb(j)=eigenb(j)+delt

               if (absdt .le.    lub) last=1
               if (absdt .le. .3*lub) last=2

               nrntn=nrntn+1
            end do

            if (last .eq. 0) then
               if (absdt .le.  3.*lub) then
                  if (newmsh .lt. 1) go to 80
               else
                  if (absdt .le. 10.*lub) then
                     if (newmsh .lt. 2) go to 80
                  else
                     if (newmsh .lt. 3) go to 80
                     nomesh=1
                     go to 99
                  end if
               end if
            end if

            if (last .eq. 1) then
               call MF_FDFDT (eigenb(j),f,dfdt)
               if (nointg .ne. 0) go to 99
               if (mzprnt .ne. 0)
     &            write(lwpcLOG_lun,
     &                '(''MF_FZEROS: '',2f7.3,'' f='',1p2e10.2)')
     &                    eigenb(j),f
               delt=-f/dfdt
               absdt=ABS(delt)
               if (absdt .gt. delmax) delt=delt*delmax/ABS(delt)
               eigenb(j)=eigenb(j)+delt
            end if

            udfdt(j)=dfdt/ABS(dfdt)
         end do

         if (nreigenb .gt. 1) then
            do j=1,nreigenb-1
               do jj=j+1,nreigenb
                  if (ABS(eigenb(j)-eigenb(jj)) .lt. 10.*lub .and.
     &                ABS(udfdt(j)-udfdt(jj)) .lt. 0.6) go to 80
               end do
            end do
         end if
      end if
      go to 99

80    newmsh=newmsh+1
      if (newmsh .gt. maxnwm) then
         nomesh=1
         go to 99
      end if
      adjmsr=adjmsr*.5
      adjmsi=adjmsi*.5
c      if (lprnt .ne. 0 .or. mzprnt .ne. 0)
c     &   write(lwpcLOG_lun,
c     &       '(''MF_FZEROS: '',
c     &         ''Mesh reduced to'',f7.4)')
c     &           adjmsr
c     Commented out to give uniform log file - 3/17/10 MLH
      go to 10

99    RETURN
      END      ! MF_FZEROS

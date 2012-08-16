      SUBROUTINE MF_WGSORT

c***********************************************************************

c     This routine checks the list of eigen angles for duplicates, i.e.,
c     solutions found in different search rectangles. This situation
c     occurs near a common boundary or corner. Duplicates are removed
c     from the list.

c     The array listke must be set before this subroutine is called ! added

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50,mxboxes=200)

      character*  8  archive,prgm_id
      character* 20  xmtr_id,path_id
      character* 40  prfl_id
      character* 80  case_id
      character*120  file_id
      logical        iterate,match
      integer        pflag,pindex,rprnt,xprnt
      real           lub
      complex        eigen,theta,c,s,csq,ssq,temp,f,dfdt,delt,
     &               eigen1,f1,df1dt,
     &               eigen2,f2,df2dt,
     &               eigdif    ! added

      common/lwpc_in/archive,file_id(3),prgm_id,
     &               case_id,prfl_id,xmtr_id,path_id,
     &               freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &               lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &               hofwr,topht,botht,pindex
     &      /lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_hgts/rtol,xintrp,htntrp,d,z0,zz
     &      /mf_list/listno(mxboxes),listbx(mxeigen),listke(mxeigen)
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      dimension      btolsq(mxboxes)


      if (msprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_WGSORT: Begin'')')

      if (iexact .ne. 0) go to 70    ! added

c     The average height of the ionosphere is used for the reference
c     height D, regardless of whether routine INTROE is used for
c     interpolation of full-wave solutions are used.
      d=(topht+botht)*.5

c     Set up heights for FSINTG
      z0=botht
      zz=d

      btsmin=4.*adjmsh**2
      dtmin=.2*adjmsh

      if (msprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_WGSORT: lub, btsmin, dtmin ='',3f8.4)')
     &           lub,btsmin,dtmin

c     The list of eigen angles is ordered from largest to smallest
c     real part.

      if (nreigen .ge. 2) then
         do j1=1,nreigen-1
            do j2=j1+1,nreigen
               if (REAL(eigen(j2)) .ge. REAL(eigen(j1))) then
                  temp      =eigen(j1)
                  eigen(j1) =eigen(j2)
                  eigen(j2) =temp
                  itemp     =listbx(j1)
                  listbx(j1)=listbx(j2)
                  listbx(j2)=itemp
                  itemp     =listke(j1)    ! added 3 statements
                  listke(j1)=listke(j2)
                  listke(j2)=itemp
               end if
            end do
         end do
      end if

      if (msprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_WGSORT:'',3('' ('',i3,'')'',2f7.3)/
     &        (''          '',3('' ('',i3,'')'',2f7.3)))')
     &        (listbx(jj),eigen(jj),jj=1,nreigen)

      do kb=1,nrboxs        ! statement no. omitted
         btolsq(kb)=((boxrht(kb)-boxlft(kb))**2+
     &               (boxtop(kb)-boxbot(kb))**2)/16.
         if (btolsq(kb) .lt. btsmin) btolsq(kb)=btsmin
      end do

 20   iterate=.false.       ! moved to this location

      j1=1
      do while (j1 .le. nreigen-1)
         kbx1=listbx(j1)
         kex1=listke(j1)    ! changed
         eigen1=eigen(j1)

         j2=j1+1
         do while (j2 .le. nreigen)
            kbx2=listbx(j2)
            kex2=listke(j2)    ! changed
            eigen2=eigen(j2)

c           The search rectangle of each eigen angle is listed in the
c           array LISTBX. If the search area of both eigen angles is
c           the same, no further testing is performed.

cxx            iterate=.false.
            if (kbx1 .ne. kbx2) then

               diffsq= REAL(eigen1-eigen2)**2
     &               +AIMAG(eigen1-eigen2)**2

               if (diffsq .le. btolsq(kbx1) .and.
     &             diffsq .le. btolsq(kbx2)) then

                  if (msprnt .ne. 0) then
                     write(lwpcLOG_lun,
     &                   '(''MF_WGSORT: Refine eigens:'')')
                     write(lwpcLOG_lun,
     &                   '(''MF_WGSORT: '',
     &                     ''eigen('',i3,'')='',2f8.4,1x,
     &                     ''exact='',i1,1x,
     &                     ''btol('',i3,'')='',f6.3)')
     &                       j1,eigen1,kex1,kbx1,SQRT(btolsq(kbx1))
                     write(lwpcLOG_lun,
     &                   '(''MF_WGSORT: '',
     &                      ''eigen('',i3,'')='',2f8.4,1x,
     &                      ''exact='',i1,1x,
     &                      ''btol('',i3,'')='',f6.3)')
     &                        j2,eigen2,kex2,kbx2,SQRT(btolsq(kbx2))
                     write(lwpcLOG_lun,
     &                   '(''MF_WGSORT: '',
     &                     ''eigen diff='',2f8.4)')
     &                       eigen1-eigen2
                  end if

                  theta=eigen1
                  iter=0
                  if (listke(j1) .eq. 0) then    ! changed

c                    The modes in question haven't been computed
c                    using exact calculations.
                     kexact=1

                     delt=(99.,99.)
                     do while (ABS(delt) .gt. lub .and. iter .lt. 11)
                        call MF_FDFDT (theta,f,dfdt)
                        delt=-f/dfdt
                        if (msprnt .gt. 0) then
                           write(lwpcLOG_lun,
     &                         '(''MF_WGSORT:'',i4,2f8.4,1x,
     &                           ''|F| '',f7.4,1x,
     &                           ''dTHETA '',f7.4,f8.4)')
     &                             iter,theta,ABS(f),delt
                        end if
                        theta=theta+delt
                        iter=iter+1
                     end do

c                    Store updated eigen angle
                     eigen(j1)=theta
                     eigen1=theta       ! added
                     iterate=.true.
                     listke(j1)=1       ! added
                  end if

                  kexact=1    ! added
                  call MF_FDFDT (theta,f1,df1dt)

                  phase1=ATAN2(AIMAG(df1dt),REAL(df1dt))*57.296
                  if (msprnt .gt. 0) then
                     write(lwpcLOG_lun,
     &                   '(''MF_WGSORT:'',i4,2f8.4,1x,
     &                     ''|F| '',f7.4,1x,
     &                     ''dF/dT  '',f7.4,f8.4,1x,
     &                     ''phs '',f7.1)')
     &                       iter,theta,ABS(f1),df1dt,phase1
                  end if

                  theta=eigen2
                  iter=0
                  if (listke(j2) .eq. 0) then    ! changed

c                    The modes in question haven't been computed
c                    using exact calculations.
                     kexact=1

                     delt=(99.,99.)
                     do while (ABS(delt) .gt. lub .and. iter .lt. 11)
                        call MF_FDFDT (theta,f,dfdt)
                        delt=-f/dfdt
                        if (msprnt .gt. 0) then
                           write(lwpcLOG_lun,
     &                         '(''MF_WGSORT:'',i4,2f8.4,1x,
     &                           ''|F| '',f7.4,1x,
     &                           ''dTHETA '',f7.4,f8.4)')
     &                             iter,theta,ABS(f),delt
                        end if
                        theta=theta+delt
                        iter=iter+1
                     end do

c                    Store updated eigen angle
                     eigen(j2)=theta
                     eigen2=theta       ! added
                     iterate=.true.
                     listke(j2)=1       ! added
                  end if

                  kexact=1    ! added
                  call MF_FDFDT (theta,f2,df2dt)

                  phase2=ATAN2(AIMAG(df2dt),REAL(df2dt))*57.296
                  if (msprnt .gt. 0) then
                     write(lwpcLOG_lun,
     &                   '(''MF_WGSORT:'',i4,2f8.4,1x,
     &                     ''|F| '',f7.4,1x,
     &                     ''dF/dT  '',f7.4,f8.4,1x,
     &                     ''phs '',f7.1)')
     &                       iter,theta,ABS(f2),df2dt,phase2
                  end if

                  match=.false.
Change 19 Nov 1990: Increased the tolerance on the real part
c                   from 1e-2 to 5e-2.
Change 11 Feb 1991: Increased the tolerance on the imag part
c                   from 1e-2 to 5e-2.
                  if (ABS( REAL(eigen1-eigen2)) .le. 5.e-2 .and.
     &                ABS(AIMAG(eigen1-eigen2)) .le. 5.e-2) then

c                    Check the derivatives:
c                    If the phases are within 45 degrees of each other,
c                    then call the modes the same and eliminate one;
c                    otherwise, call them degenerate and keep both.

                     if (phase1 .ge. 0. .and. phase2 .ge. 0.) then

c                       Both phases are in the upper hemisphere
                        if (ABS(phase1-phase2) .le. 45.) match=.true.
                     else
     &               if (phase1 .le. 0. .and. phase2 .le. 0.) then

c                       Both phases are in the lower hemisphere
                        if (ABS(phase1-phase2) .le. 45.) match=.true.
                     else

c                       The phases are in opposite hemispheres
                        if (ABS(phase1-phase2) .le.  45. .or.
     &                      ABS(phase1-phase2) .ge. 315.) match=.true.
                     end if
                  end if

                  if (match) then

c                    The modes are identical, eliminate one.

                     if (msprnt .gt. 0)
     &                  write(lwpcLOG_lun,
     &                      '(''MF_WGSORT: '',
     &                        ''eigen('',i3,'') removed'')')
     &                          MAX0(j1,j2)

                     if (nreigen .gt. 2 .and.
     &                   nreigen .gt. MAX0(j1,j2)) then

                        do j=MAX0(j1,j2),nreigen-1
                           eigen (j)=eigen (j+1)
                           listbx(j)=listbx(j+1)
                           listke(j)=listke(j+1)    ! added
                        end do
                     end if

c                    Reduce the number of modes by one
                     nreigen=nreigen-1

c                    Set the counter so that the new mode J2
c                    is compard to mode J1.
                     j2=j2-1
                  end if
               end if
            end if
            j2=j2+1
         end do
         j1=j1+1
      end do

      if (iterate) then

c        The list of eigen angles must be re-ordered from
c        largest to smallest real part because the iteration
c        may have caused one mode to approach one that has
c        already been checked.

         if (nreigen .ge. 2) then
            do j1=1,nreigen-1
               do j2=j1+1,nreigen
                  if (REAL(eigen(j2)) .ge.
     &                REAL(eigen(j1))) then
                     temp      =eigen(j1)
                     eigen(j1) =eigen(j2)
                     eigen(j2) =temp
                     itemp     =listbx(j1)
                     listbx(j1)=listbx(j2)
                     listbx(j2)=itemp
                     itemp     =listke(j1)    ! added 3 statements
                     listke(j1)=listke(j2)
                     listke(j2)=itemp
                  end if
               end do
            end do
         end if
         if (msprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_WGSORT: '',
     &          3(''('',i3,'')'',2f7.3)/
     &           (''           '',
     &          3(''('',i3,'')'',2f7.3)))')
     &            (listbx(jj),eigen(jj),jj=1,nreigen)
         go to 20     ! added
      end if
      RETURN

c     Test for redundant solutions when iexact=1
 70   j1=1
      do while (j1 .le. nreigen-1)
         j2=j1+1
         do while (j2 .le. nreigen)
            if (listbx(j1) .ne. listbx(j2)) then
               eigdif=eigen(j1)-eigen(j2)
               if (ABS( REAL(eigdif)) .lt. 5.e-2 .and.
     &             ABS(AIMAG(eigdif)) .lt. 5.e-2) then
                  if (j2 .ne. nreigen) then
                     do j=j2,nreigen-1
                        eigen(j)=eigen(j+1)
                        listbx(j)=listbx(j+1)
                     end do
                  end if
                  nreigen=nreigen-1
                  j2=j2-1
               end if
            end if
            j2=j2+1
         end do
         j1=j1+1
      end do

      if (nreigen .ge. 2) then
         do j1=1,nreigen-1
            do j2=j1+1,nreigen
               if (REAL(eigen(j2)) .ge.
     &             REAL(eigen(j1))) then
                  temp      =eigen(j1)
                  eigen(j1) =eigen(j2)
                  eigen(j2) =temp
                  itemp     =listbx(j1)
                  listbx(j1)=listbx(j2)
                  listbx(j2)=itemp
               end if
            end do
         end do
      end if
      if (msprnt .ne. 0)
     &   write(lwpcLOG_lun,
     &       '(''MF_WGSORT: '',
     &       3(''('',i3,'')'',2f7.3)/
     &        (''           '',
     &       3(''('',i3,'')'',2f7.3)))')
     &         (listbx(jj),eigen(jj),jj=1,nreigen)
      RETURN
      END      ! MF_WGSORT
      SUBROUTINE PLT_JOINT_CNTR
     &          (xlbl,ylbl,
     &           cntr_level,ntlist,ntl,plvl,rcomp,
     &           grd_lat,grd_lon,
     &           mxlat,nrlat,mxlon,nrlon,
     &           amp,sig,ampl)

c Sets up contours of signal levels for fixed availibility

c Change History

c     16 Apr 1995   Changed to use new geophysics routines and to use
c                   colors from the GRAPHICS.INI file.

c     07 Jun 1995   Moved generation of plot labels and maps to the
c                   calling routine.

c     26 Aug 1995   Changed PLT_CNTR, PLT_EXCD and PLT_JOINT_CNTR to use
c                   fills from GRAPHICS.INI; the variable USE_FILL_LIST
c                   is removed.

c     24 Feb 1996   Moved cells,mxcells,nrpts,mxholes,xwork,ywork,mxwork
c                   from argument list to new common block grf$cntr.

c     12 Mar 1997   Modified to use new string routines in GRF.

c     13 Jan 1998   Dropped test for output device re setting the color
c                   and fill accordingly.

c     Graphics parameters
      include      'graphics.cmn'

c     Map parameters
      include      'lwpc_geo.cmn'

      integer       rcomp
      integer       ntlist(5)
      real          cntr_level(5),
     &              grd_lat(2),grd_lon(2)

      character*  8 color,fill
      character* 80 plblx,pfrmt

      dimension     amp(mxlon,mxlat,2,5),sig(mxlon,mxlat,2,5),
     &              ampl(mxlon,mxlat),
     &              xl(2),yl(2),zcntr(2)


c     Set up number of standard deviations corresponding to %TA
      ss=PROB_TO_SIGMA (plvl)

c     Label for the time availability
      write(plblx,'(i3,''% availability'')')
     &      INT(plvl)
      call GRF_STRING (xlbl,ylbl,.1,plblx,0.,'LB',retX)
      ylbl=ylbl-.15

c     Double space
      ylbl=ylbl-.15

c     Label for the contours
      plblx='Tx'
      call GRF_STRING (xlbl,ylbl,.1,plblx,0.,'LB',retX)
      ylbl=ylbl-.15

c     Define a format to be used for numerical labels of the contours
      write(pfrmt,'(''(i1,4('''','''',i1))'')')
      xl(1)=xlbl+1.
      xl(2)=xlbl+1.6

c     Single coverage
      nt=0
      irtn=1
1     if (nt .eq. ntl) then
         if (ntl .eq. 1) go to 9
         kk=6
         n1=1
         n2=1
         irtn=2
      else
         nt=nt+1
         kk=nt
         do l=1,nrlat
            do k=1,nrlon
               ampl(k,l)=amp(k,l,rcomp,ntlist(nt))
     &                  -sig(k,l,rcomp,ntlist(nt))*ss
     &                     -cntr_level(ntlist(nt))
            end do
         end do
         write(plblx,pfrmt) ntlist(nt)
         go to 6
      end if

c     Dual coverage
2     if (n2 .eq. ntl) then
         if (n1 .eq. ntl-1) then
            if (ntl .eq. 2) go to 9
            kk=7
            n1=1
            n2=2
            n3=2
            irtn=3
            go to 3
         else
            n1=n1+1
            n2=n1
         end if
      end if
      n2=n2+1
      do l=1,nrlat
         do k=1,nrlon
            ampl(k,l)=MIN(amp(k,l,rcomp,ntlist(n1))
     &                   -sig(k,l,rcomp,ntlist(n1))*ss
     &                      -cntr_level(ntlist(n1)),
     &                    amp(k,l,rcomp,ntlist(n2))
     &                   -sig(k,l,rcomp,ntlist(n2))*ss
     &                      -cntr_level(ntlist(n2)))
         end do
      end do
      write(plblx,pfrmt) ntlist(n1),ntlist(n2)
      go to 6

c     Triple coverage
3     if (n3 .eq. ntl) then
         if (n2 .eq. ntl-1) then
            if (n1 .eq. ntl-2) then
               if (ntl .eq. 3) go to 9
               kk=8
               n1=1
               n2=2
               n3=3
               n4=3
               irtn=4
               go to 4
            else
               n1=n1+1
               n2=n1+1
               n3=n2
            end if
         else
            n2=n2+1
            n3=n2
         end if
      end if
      n3=n3+1
      do l=1,nrlat
         do k=1,nrlon
            ampl(k,l)=MIN(amp(k,l,rcomp,ntlist(n1))
     &                   -sig(k,l,rcomp,ntlist(n1))*ss
     &                      -cntr_level(ntlist(n1)),
     &                    amp(k,l,rcomp,ntlist(n2))
     &                   -sig(k,l,rcomp,ntlist(n2))*ss
     &                      -cntr_level(ntlist(n2)),
     &                    amp(k,l,rcomp,ntlist(n3))
     &                   -sig(k,l,rcomp,ntlist(n3))*ss
     &                      -cntr_level(ntlist(n3)))
         end do
      end do
      write(plblx,pfrmt) ntlist(n1),ntlist(n2),ntlist(n3)
      go to 6

c     Quadruple coverage
4     if (n4 .eq. ntl) then
         if (n3 .eq. ntl-1) then
            if (n2 .eq. ntl-2) then
               if (n1 .eq. ntl-3) then
                  if (ntl .eq. 4) go to 9
                  go to 5
               else
                  n1=n1+1
                  n2=n1+1
                  n3=n2+1
                  n4=n3
               end if
            else
               n2=n2+1
               n3=n2+1
               n4=n3
            end if
         else
            n3=n3+1
            n4=n3
         end if
      end if
      n4=n4+1
      do l=1,nrlat
         do k=1,nrlon
            ampl(k,l)=MIN(amp(k,l,rcomp,ntlist(n1))
     &                   -sig(k,l,rcomp,ntlist(n1))*ss
     &                      -cntr_level(ntlist(n1)),
     &                    amp(k,l,rcomp,ntlist(n2))
     &                   -sig(k,l,rcomp,ntlist(n2))*ss
     &                      -cntr_level(ntlist(n2)),
     &                    amp(k,l,rcomp,ntlist(n3))
     &                   -sig(k,l,rcomp,ntlist(n3))*ss
     &                      -cntr_level(ntlist(n3)),
     &                    amp(k,l,rcomp,ntlist(n4))
     &                   -sig(k,l,rcomp,ntlist(n4))*ss
     &                      -cntr_level(ntlist(n4)))
         end do
      end do
      write(plblx,pfrmt) ntlist(n1),ntlist(n2),ntlist(n3),ntlist(n4)
      go to 6

c     Quintuple coverage
5     do l=1,nrlat
         do k=1,nrlon
            ampl(k,l)=MIN(amp(k,l,rcomp,1)
     &                   -sig(k,l,rcomp,1)*ss
     &                      -cntr_level(1),
     &                    amp(k,l,rcomp,2)
     &                   -sig(k,l,rcomp,2)*ss
     &                      -cntr_level(2),
     &                    amp(k,l,rcomp,3)
     &                   -sig(k,l,rcomp,3)*ss
     &                      -cntr_level(3),
     &                    amp(k,l,rcomp,4)
     &                   -sig(k,l,rcomp,4)*ss
     &                      -cntr_level(4),
     &                    amp(k,l,rcomp,5)
     &                   -sig(k,l,rcomp,5)*ss
     &                      -cntr_level(5))
         end do
      end do
      write(plblx,pfrmt) ntlist
      kk=9
      irtn=9

c     Do contour plots
6     zcntr(1)=0.
      zcntr(2)=999.

c     Label for this contour
      call GRF_COLOR ('labels')
      call GRF_STRING (xlbl,ylbl,.1,plblx,0.,'LB',retX)

      write(color,'(''color'',i1)') kk
      fill='solid'

      call GRF_COLOR (color)

      yl(1)=ylbl
      yl(2)=ylbl+0.15
      call GRF_FILL_RECT
     &    (xl(1),yl(1),xl(2),yl(2),fill)

      ylbl=ylbl-.15

      call GEO_CNTR
     &    (ampl,mxlon,mxlat,nrlon,nrlat,
     &     grd_lat,grd_lon,
     &     zcntr,0,fill,color)

      go to (1,2,3,4,9),irtn

9     RETURN
      END      ! PLT_JOINT_CNTR
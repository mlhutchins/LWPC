      SUBROUTINE PLT_CNTR
     &          (plt_device,plt_orientation,
     &           brief,xlbl,ylbl,
     &           ntlist,ntl,nplt,
     &           plot_lbl,graf_lbl,
     &           xmtr_lbl,data_lbl,
     &           prfl_lbl,file_lbl,
     &           rcvr_lbl,cntr_lbl,
     &           plevel,nplvl,
     &           cntr_level,nclvl,cntr_range,
     &           cntr_lvl,mxcntr,
     &           opa_lat,opa_lon,
     &           grd_lat,grd_lon,
     &           map_type,map_size,map_tics,map_grid,
     &           terminator,chi_list,
     &           chi_max,chi_min,chi_inc,
     &           chi_label,chi_line,
     &           amp1,std1,ampl,mxlat,nrlat,mxlon,nrlon)

c Sets up contours of signal levels for fixed availibility

c Change History

c     26 Sep 1994   Made compatible with new contour routines; only does
c                   fill contours now; removed display of %Sea %Map on
c                   plot.

c     03 Oct 1994   Moved world map call from PLT_CNTR_LABEL so that it
c                   overlays the filled contours.

c     16 Apr 1995   Changed to use new geophysics routines and to use
c                   colors from the GRAPHICS.INI file.

c     22 May 1995   Added solid fills for LJC color device.

c     07 Jun 1995   Moved generation of plot labels and maps to the
c                   calling routine.

c     26 Aug 1995   Changed PLT_CNTR, PLT_EXCD and PLT_JOINT_CNTR to use
c                   fills from GRAPHICS.INI; the variable USE_FILL_LIST
c                   is removed.

c     01 Feb 1996   Changed to only use contour levels from an input
c                   array; calculate the contour data array in the
c                   calling routine.

c     06 Feb 1996   Changed graph label, i.e., S/N, to a single element
c                   to allow better alignment of the contour labels.

c     24 Feb 1996   Moved cells,mxcells,nrpts,mxholes,xwork,ywork,mxwork
c                   from argument list to new common block grf$cntr.

c     12 Mar 1997   Modified to use new string routines in GRF.

c     23 Jun 1997   Moved common blocks of code from grdPlot to this
c                   routine to reduce the size of grdPlot.

c     13 Jan 1998   Dropped test for output device re setting the color
c                   and fill accordingly.

c     14 Jan 98     Removed PLT_UNIT.

c     Graphics parameters
      include      'lwpc_lun.cmn'
      include      'graphics.cmn'

      character*(*) plt_device,plt_orientation,
     &              plot_lbl,graf_lbl,
     &              xmtr_lbl(-1:6),data_lbl(-1:6),
     &              prfl_lbl(-1:6),file_lbl(-1:6),
     &              rcvr_lbl(-1:1),cntr_lbl(mxcntr),
     &              map_type(2)
      logical       map_grid,terminator,chi_label
      integer       mnth_num,day,year,UT,chi_line
      real          plevel(7),
     &              cntr_level(7),
     &              cntr_range(3),
     &              cntr_lvl(mxcntr),
     &              opa_lat(2),opa_lon(2),
     &              grd_lat(2),grd_lon(2),
     &              map_size(2),map_tics(2),
     &              chi_list(2),chi_max,chi_min,chi_inc,
     &              amp1(mxlon,mxlat),
     &              std1(mxlon,mxlat),
     &              ampl(mxlon,mxlat)

      character*  3 mnth_nam
      character*  8 color,fill
      character* 80 plblx
      real          xl(2),yl(2),zcntr(2)

cxxcDEBUG
cxx      line=1

      do np=1,nplvl

c        Initiate graphics
         call GRF_BEGIN (plt_device,plt_orientation)

c        Set up labels for this specific plot
         xlbl=map_size(1)
         ylbl=map_size(2)

         call PLT_CNTR_LABEL
     &       (brief,xlbl,ylbl,
     &        ntlist,ntl,
     &        plot_lbl,graf_lbl,
     &        xmtr_lbl,data_lbl,
     &        prfl_lbl,file_lbl,
     &        rcvr_lbl)

c        Set up signal, SNR, etc. contours
c        parametric in time availibility
         plvl=plevel(np)

c        Get amplitude level corresponding to
c        specified time availability
         ss=PROB_TO_SIGMA (plvl)
         do l=1,nrlat
            do k=1,nrlon
               ampl(k,l)=amp1(k,l)-ss*std1(k,l)
            end do
         end do
cxxcDEBUG
cxx         do n=1,nrlon,29
cxx            write(plblx,'(''grd.'',i3.3)') n
cxx            open (lwpcTMP_lun,file=plblx,status='unknown')
cxx            write(lwpcTMP_lun,'(a)') 'Contour data'
cxx            write(lwpcTMP_lun,'(a)') graf_lbl
cxx            write(lwpcTMP_lun,'(f4.1)') plvl
cxx            write(plblx,
cxx     &          '(''(i3,'',i2,''f5.1,i3)'')')
cxx     &            MIN(nrlon-n+1,29)
cxx            write(lwpcTMP_lun,
cxx     &           '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx            do j=nrlat,1,-1
cxx               write(lwpcTMP_lun,plblx)
cxx     &               j,(amp1(i,j),i=n,MIN(nrlon,n+28)),j
cxx            end do
cxx            write(lwpcTMP_lun,
cxx     &          '(3x,29i5)') (i,i=n,MIN(nrlon,n+28))
cxx            close(lwpcTMP_lun)
cxx         end do

         if (nclvl .eq. 0) then

            ampinc=cntr_range(3)
            call PLT_CNTR_RANGE
     &          (ampl,mxlat,nrlat,mxlon,nrlon,
     &           ampinc,ampmin,ampmax)
            if (cntr_range(1) .ne. -99.)
     &         ampmin=MAX(ampmin,cntr_range(1))
            if (cntr_range(2) .ne. -99.)
     &         ampmax=MIN(ampmax,cntr_range(2))
            nl=0
            do amp=ampmin,ampmax,ampinc
               nl=nl+1
               cntr_lvl(nl)=amp
            end do
            nrlvl=nl
         else

            do nl=1,nclvl
               cntr_lvl(nl)=cntr_level(nl)
            end do
            nrlvl=nclvl
         end if
         do nl=1,nrlvl-1

            call PLT_AREA_SIZE
     &          (mxlat,nrlat,mxlon,nrlon,
     &           ampl,opa_lat,opa_lon,
     &           cntr_lvl(nl+1),area_a,area_o)

            write(cntr_lbl(nl),
     &          '(f6.1,'' to '',f6.1,1x,i3)')
     &            cntr_lvl(nl),cntr_lvl(nl+1),
     &            INT(area_o)
         end do

c        Label for the time availability
         write(plblx,'(i3,''% availability'')')
     &         INT(plvl)
         call GRF_STRING (xlbl,ylbl,.1,plblx,0.,'LB',adjX)
         ylbl=ylbl-.15

c        Double space
         ylbl=ylbl-.15

c        Add %ocean tag to graph label
c        NOTE:  graf_lbl MUST be at least 20 characters long
         write(graf_lbl(19:),'(''%O'')')

c        Label for what is being plotted, i.e., SIGNAL (dB/uV/m)
         call GRF_STRING (xlbl,ylbl,.1,graf_lbl,0.,'LB',retX)
         adjX=MAX(adjX,retX)
         ylbl=ylbl-.15

c        Define a format for numerical labels of the contours
         xl(1)=xlbl+adjX
         xl(2)=xlbl+adjX+.6

c        Do contour plots
         do nc=1,nrlvl-1

            kk=MOD(nc,8)
            if (kk .eq. 0) kk=8

            write(color,'(''color'',i1)') kk
            fill='solid'

            zcntr(1)=cntr_lvl(nc  )
            zcntr(2)=cntr_lvl(nc+1)

c           Label for this contour
            call GRF_COLOR ('labels')
            call GRF_STRING (xlbl,ylbl,.1,cntr_lbl(nc),0.,'LB',retX)

            yl(1)=ylbl
            yl(2)=ylbl+0.15

            call GRF_COLOR (color)
            call GRF_FILL_RECT (xl(1),yl(1),xl(2),yl(2),fill)

            ylbl=ylbl-.15

            call GEO_CNTR
     &          (ampl,mxlon,mxlat,nrlon,nrlat,
     &           grd_lat,grd_lon,
     &           zcntr,0,fill,color)
         end do

c        Generate map
         call WORLD_MAP
     &       (map_type,map_grid,map_tics)

         if (terminator) then

c           Show the terminator boundaries on the map
c           ONLY IF the signal data was run for a specific
c           date and time
            if (prfl_lbl(nplt)(:10) .eq. 'LWPM Date:') then

c              Extract the date and time
               read (prfl_lbl(nplt),'(11x,3(i2,1x),i4)')
     &               mnth_num,day,year,UT
               call MONTH_NAME (mnth_num,mnth_nam)
               UThrs=(UT-40.*INT(UT/100.))/60.
               call GEO_CHI_MAP
     &             (mnth_nam,day,year,UThrs,
     &              2,2,chi_list,
     &              chi_max,chi_min,chi_inc,
     &              chi_label,chi_line)
            end if
         end if
         call GRF_DONE
      end do
      RETURN
      END      ! PLT_CNTR
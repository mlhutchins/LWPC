      SUBROUTINE GEO_CNTR
     &          (v,mxx,mxy,nrx,nry,
     &           grd_lat,grd_lon,
     &           vcntr,line,fill,color)

c***********************************************************************
c                   subroutine geo_cntr
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     Draws coordinates of lines of constant value in a grid of values
c     stored in a two dimensional array V. The lower left corner of the
c     grid is at V(1,1).

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c  Parameters passed:
c     v             [r]           two dimensional array of data
c     mxx           [i]           dimension of the first  index of V
c     mxy           [i]           dimension of the second index of V
c     nrx           [i]           limit of first  index of values in V
c     nry           [i]           limit of second index of values in V

c     grd_lat       [r,2]         latitude  limits of grid data
c     grd_lon       [r,2]         longitude limits of grid data

c     vcntr         [r,2]         definition of the contour band
c     line          [i]           =0: filled contours
c                                 >0: line type to use; see GRF_CURVE
c     fill          [c]           name of fill or percentage to use
c     color         [c]           name or RGB sequence of color to use

c  Parameters returned:

c  Common blocks referenced:
c     geo$data
c     grf$cntr
c     lwpc_lun

c  Functions and subroutines referenced:
c     abs
c     float
c     int
c     max
c     min
c     mod

c     geo_coord

c     grf_cntr
c     grf_cntr_fill
c     grf_cntr_join
c     grf_cntr_line
c     grf_color
c     grf_limits

c     recvr2

c     shift_array
c     shift_array2

c  Entry:

c  References:

c  Change History:

c     24 Feb 1996   Moved cells,mxcells,nrpts,mxholes,xwork,ywork,mxwork
c                   from argument list to new common block grf$cntr.

c     18 Dec 1997   Added stereographic projection.

c     19 Jan 1998   Corrected problem encountered when the left
c                   longitude of the defined grid data was to the right
c                   of the mapped area.

c     10 Oct 1999   Better test for the antipode inserted.

c*******************!***************************************************

      IMPLICIT NONE

c     LWPC parameters
      include      'lwpc_geo.cmn'
      include      'lwpc_lun.cmn'
      include      'grf_cntr.cmn'

      character*(*) color
      character*(*) fill
      integer       line
      integer       mxx
      integer       mxy
      integer       nrx
      integer       nry
      real          grd_lat
      real          grd_lon
      real          v
      real          vcntr

      dimension     grd_lat(2)
      dimension     grd_lon(2)
      dimension     v(mxx,mxy)
      dimension     vcntr(2)

      character*  8 status
      logical       search
      logical       split_lat
      integer       n
      integer       ndx_lat1
      integer       ndx_lat2
      integer       ndx_lon1
      integer       ndx_lon2
      integer       n1
      integer       nh
      integer       nmax
      integer       np
      integer       nrp
      integer       nr_shift_lon
      integer       nx
      integer       nx1
      integer       nxm
      integer       nxtr
      integer       ny
      integer       ny1
      integer       nym
      integer       shift_lat
      integer       shift_lon
      real          br
      real          br_inc
      real          crclt
      real          dlat
      real          dlon
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x
      real          geo_y
      real          grd_lon1
      real          grd_lon2
      real          lat
      real          lat0
      real          lon
      real          lon0
      real          map_lat(2)
      real          map_lon(2)
      real          pi
      real          plt_lat
      real          plt_lon
      real          plt_x
      real          plt_y
      real          prv_br
      real          prv_rng
      real          rclt
      real          rlng
      real          rng
      real          rng_inc
      real          rsq
      real          srclt
      real          tol
      real          xnp
      real          ynp
cxxcDEBUG2
cxx      real          retX

      data          dtr/.01745329252/
      data          pi/3.14159265359/


c     Set some working values
      grd_lon1=grd_lon(1)
      grd_lon2=grd_lon(2)
      if (grd_lon2 .ge. grd_lon1) grd_lon2=grd_lon2-360.

      dlat=(grd_lat(2)-grd_lat(1))/(nry-1)
      dlon=(grd_lon1-grd_lon2)/(nrx-1)

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal Equidistant, Gnomonic, Orthographic or Stereographic

c        The most effective way to map a rectangular array onto
c        these projections is to split the input data so that
c        only the visible portions are processed. However, since
c        we have to allow for something on the back side to become
c        visible and to allow for the possibility that a contour
c        line may pass through the antipode of the map's projection
c        center, it is it is necessary to process the grid in an
c        upper and lower portion separately.  The solution, as
c        complicated as it seems, is to split the input array into
c        and upper and lower portion and to process the longitude
c        in four separate passes.
cxxcDEBUG
cxx         line=1
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'cntr   ',vcntr
cxx         write(lwpcLOG_lun,*)'grd-lat',nry,grd_lat,dlat
cxx         write(lwpcLOG_lun,*)'grd-lon',nrx,grd_lon,dlon

         shift_lat=0

         lat0=geo_center_lat
         lon0=geo_center_lon

c        Latitude bounds of the mapped area
         if (line .eq. 0) then

c           Filled contours

c           Latitude bounds of the upper, visible part
c           of the mapped area
            lat=INT((lat0-geo_rng_max*6.366*9.)/dlat+.99)*dlat
            map_lat(1)=MAX(-lat0,grd_lat(1),lat)

            lat=INT((lat0+geo_rng_max*6.366*9.)/dlat+.99)*dlat
            map_lat(2)=MIN(grd_lat(2),lat)

            if (map_lat(1) .ge. map_lat(2)) then

c              We don't need to shift the data array
               lat=INT((lat0-geo_rng_max*6.366*9.)/dlat+.99)*dlat
               map_lat(1)=MAX(grd_lat(1),lat)

               split_lat=.false.
            else

c              We need to shift the data array to expose the upper,
c              visible part of the mapped area
               split_lat=.true.
            end if
         else

c           Line contours; we don't need to shift the data array
            lat=INT((lat0-geo_rng_max*6.366*9.)/dlat+.99)*dlat
            map_lat(1)=MAX(grd_lat(1),lat)

            lat=INT((lat0+geo_rng_max*6.366*9.)/dlat+.99)*dlat
            map_lat(2)=MIN(grd_lat(2),lat)

            split_lat=.false.
         end if

c        Longitude bounds of the mapped area
11       if (line .eq. 0) then

c           Do filled contours in NR_SHIFT_LON sections
            nr_shift_lon=4

            map_lon(1)=grd_lon1
            map_lon(2)=map_lon(1)-360./nr_shift_lon
         else

c           Do lined contours
            nr_shift_lon=2

            map_lon(1)=grd_lon1
            if (map_lon(1) .gt.  180.) map_lon(1)=map_lon(1)-360.
            map_lon(2)=map_lon(1)-360.
         end if
         shift_lon=1
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'map-lat',map_lat
cxx         write(lwpcLOG_lun,*)'map-lon',map_lon

c        Set up counters in latitude
         if (grd_lat(1) .gt. map_lat(1)) then

c           The bottom of the data is above the mapped area;
c           the 1st index and the plot reference is the
c           data coordinate.
            ndx_lat1=1
            plt_lat=grd_lat(1)
         else

c           The bottom of the data is below the mapped area;
c           the 1st index and the plot reference is the
c           map coordinate.
            ndx_lat1=INT((map_lat(1)-grd_lat(1))/dlat)+1
            plt_lat=map_lat(1)
         end if
         if (grd_lat(2) .le. map_lat(2)) then

c           The top of the data is below the mapped area;
c           the 2nd index is referenced to the
c           data coordinate.
            ndx_lat2=nry
         else

c           The top of the data is above the mapped area;
c           the 2nd index is referenced to the
c           map coordinate.
            ndx_lat2=INT((map_lat(2)-grd_lat(1))/dlat)+1
         end if
         ny1=1
         nym=ndx_lat2-ndx_lat1+1
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'ndx-lat',ndx_lat1,ndx_lat2,map_lat

         if (ndx_lat1 .gt. 1) then

c           Shift the grid data in latitude
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'original-lat'
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(1,n),n=1,nry)

            call SHIFT_ARRAY2
     &          (v,mxx,mxy,nrx,1,ndx_lat1,nry)

c           Keep track of the original first row
            ny1=2-ndx_lat1
            if (ny1 .lt. 1) ny1=ny1+nry
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'shift-lat',shift_lat,ny1,nym
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(1,n),n=1,nry)

         end if

c        Set up counters in longitude
21       search=.false.
         ndx_lon1=0
         plt_lon=99.
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'map-lat',shift_lat,map_lat
cxx         write(lwpcLOG_lun,*)'map-lon',shift_lon,map_lon

         if (grd_lon1 .lt. map_lon(1)) then

            if (grd_lon1 .le. map_lon(2)) then

c              The left edge of the data is to the right of the
c              right edge of the mapped area; no plot is possible.
               search=.false.
            else

c              The left edge of the data is to the right of the
c              left edge of the mapped area; the 1st index and
c              the plot reference is the data coordinate.
               search=.true.
               ndx_lon1=1
               plt_lon=grd_lon1
            end if
         else

            if (grd_lon2 .ge. map_lon(1)) then

c              The right edge of the data is to the left of the
c              left edge of the mapped area; no plot is possible.
               search=.false.
            else

c              The right edge of the data is to the left of the
c              left edge of mapped area; the 1st index and the
c              plot reference is the map coordinate.
               search=.true.
               ndx_lon1=INT((grd_lon1-map_lon(1))/dlon)+1
               plt_lon=map_lon(1)
            end if
         end if
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'ndx-lon',search,ndx_lon1,plt_lon

         if (search .and. ndx_lon1 .ge. 1 .and. ndx_lon1 .lt. nrx) then

            nx1=1
            nxm=MIN(nrx,INT((map_lon(1)-grd_lon2)/dlon+.9)+1)
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'ndx-lon',ndx_lon1,nxm,map_lon

c           Shift the grid data in longitude
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'original-lon'
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(n,nym/2),n=1,nrx)

            if (MOD(ABS(grd_lon1-grd_lon2),360.) .eq. 0.) then

c              Global array
               do n=1,nym
                  call SHIFT_ARRAY
     &                (v(1,n),mxx,1,ndx_lon1,nrx-1)

c                 Copy the new 1st column to the end
                  v(nrx,n)=v(1,n)
               end do

c              Keep track of the original first column
               nx1=2-ndx_lon1
               if (nx1 .lt. 1) nx1=nx1+nrx-1
            else

c              Not a global array
               if (ndx_lon1 .lt. nrx) then
                  do n=1,nym

                     call SHIFT_ARRAY
     &                   (v(1,n),mxx,1,ndx_lon1,nrx)
                  end do

c                 Keep track of the original first column
                  nx1=2-ndx_lon1
                  if (nx1 .lt. 1) nx1=nx1+nrx
               end if
            end if
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'shift-lon',shift_lon,nx1
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(n,nym/2),n=1,nrx)
cxx            n=shift_lat*nr_shift_lon+shift_lon+1
cxx            write(color,'(''color'',i1)') n
cxx            write(lwpcLOG_lun,*) color

c           Search for contours
            status='begin'
            do while (status .ne. 'complete')

               call GRF_CNTR
     &             (v,mxx,mxy,nxm,nym,
     &              vcntr,status)
cxxcDEBUG
cxx               write(lwpcLOG_lun,*)'cntr',shift_lat,shift_lon,nrpts(0)

               if (nrpts(0) .gt. 0) then

                  if (status .eq. 'complete' .and. nrpts(0) .eq. 5) then

c                    Check if the whole array is filled
                     if (cntrpx(1) .eq.         1. .and.
     &                   cntrpy(1) .eq.         1. .and.
     &                   cntrpx(2) .eq. FLOAT(nxm) .and.
     &                   cntrpy(2) .eq.         1. .and.
     &                   cntrpx(3) .eq. FLOAT(nxm) .and.
     &                   cntrpy(3) .eq. FLOAT(nym) .and.
     &                   cntrpx(4) .eq.         1. .and.
     &                   cntrpy(4) .eq. FLOAT(nym) .and.
     &                   cntrpx(5) .eq.         1. .and.
     &                   cntrpy(5) .eq.         1.) then

                        np=0
                        ny=1
                        do nx=1,nxm
                           np=np+1
                           cntrpx(np)=nx
                           cntrpy(np)=ny
                        end do
                        nx=nxm
                        do ny=1,nym
                           np=np+1
                           cntrpx(np)=nx
                           cntrpy(np)=ny
                        end do
                        ny=nym
                        do nx=nxm,1,-1
                           np=np+1
                           cntrpx(np)=nx
                           cntrpy(np)=ny
                        end do
                        nx=1
                        do ny=nym,1,-1
                           np=np+1
                           cntrpx(np)=nx
                           cntrpy(np)=ny
                        end do
                        np=np+1
                        cntrpx(np)=cntrpx(1)
                        cntrpy(np)=cntrpy(1)
                        nrpts(0)=np
                     end if
                  end if

c                 Convert to longitude, latitude
                  do np=1,nrpts(nrholes)

                     lat=plt_lat+(cntrpy(np)-1.)*dlat
                     lon=plt_lon-(cntrpx(np)-1.)*dlon
                     if (lon .lt. -180.) lon=lon+360.
cxxcDEBUG
cxx                     write(lwpcLOG_lun,'(i4,2f6.1,2f7.1)')
cxx     &                     np,cntrpx(np),cntrpy(np),lat,lon

                     cntrpy(np)=lat
                     cntrpx(np)=lon
                  end do

                  if (save_cntr) then

c                    Prepare to write contour data to lwpcTMP_lun
                     call GRF_CNTR_JOIN

                     write(lwpcTMP_lun) nrholes,nrpts

                     if (nrpts(0) .gt. 0) then

c                       Save the coordinates to the data file
                        write(lwpcTMP_lun) shift_lat,
     &                       (cntrpx(n),cntrpy(n),n=1,nrpts(0))
                     end if
                  end if
cxxcDEBUG2
cxxc                 Plot the raw contour
cxx                  if (color .eq. 'color7') then
cxx                  call GRF_COLOR
cxx     &                (color)
cxx
cxx                  nmax=nrpts(0)
cxx                  do np=1,nmax
cxx
cxx                     lat=cntrpy(np)
cxx                     lon=cntrpx(np)
cxx
cxx                     call GEO_COORD
cxx     &                   (lat,lon,
cxx     &                    geo_rng,geo_brng,geo_x,geo_y,
cxx     &                    plt_x,plt_y)
cxx
cxx                     cntrpx(np)=plt_x
cxx                     cntrpy(np)=plt_y
cxx                  end do
cxx
cxx                  call GRF_CURVE
cxx     &                (cntrpx,cntrpy,nrpts(0),0.,0.,1.,1.,4,20)
cxx
cxx                  do n=1,nrpts(0),10
cxx                     np=n
cxx                     call GRF_NUMBER
cxx     &                   (cntrpx(np),cntrpy(np),.1,FLOAT(np),0.,-1,
cxx     &                   'LB',retX)
cxx                  end do
cxx                  if (np .ne. nrpts(0)) then
cxx                     np=nrpts(0)
cxx                     call GRF_NUMBER
cxx     &                   (cntrpx(np),cntrpy(np),.1,FLOAT(np),0.,-1,
cxx     &                   'LB',retX)
cxx                  end if
cxx                  end if
cxxcDEBUG2
c                 Count number of points beyond the maximum range or
c                 outside the bounds of the plot.
                  nrp=0

                  nmax=nrpts(0)
                  do np=1,nmax

                     lat=cntrpy(np)
                     lon=cntrpx(np)

                     call GEO_COORD
     &                   (lat,lon,
     &                    geo_rng,geo_brng,geo_x,geo_y,
     &                    plt_x,plt_y)

                     if (plt_x   .lt.         0.  .or.
     &                   plt_x   .gt. geo_size_x  .or.
     &                   plt_y   .lt.         0.  .or.
     &                   plt_y   .gt. geo_size_y  .or.
     &                   geo_rng .gt. geo_rng_max) then

c                       This point is out of bounds
                        nrp=nrp+1
                     end if
                  end do
cxxcDEBUG
cxx                  write(lwpcLOG_lun,'(i4,'' out of bounds'')')
cxx     &                  nrp

c                 Check if all points are out of bounds
                  if (nrp .lt. nmax) then

                     np=1
                     n1=1
                     do nh=0,nrholes

                        nmax=nrpts(nh)
                        do while (np .le. nmax)

c                          Extract position
                           lat=cntrpy(np)
                           lon=cntrpx(np)

c                          Convert to range and bearing
                           call GEO_COORD
     &                         (lat,lon,
     &                          geo_rng,geo_brng,geo_x,geo_y,
     &                          plt_x,plt_y)
cxxcDEBUG
cxx                           write(lwpcLOG_lun,
cxx     &                         '(i4,3f7.1,f8.1,2f6.2)')
cxx     &                           np,lat,lon,
cxx     &                           geo_brng/dtr,geo_rng*6366.,
cxx     &                           plt_x,plt_y

                           if (geo_rng .gt. geo_rng_max) then
Change: 10/10/99
c       Better test for the antipode
c                             if (ABS(lat0+lat) .le. 0.1 .and.
c    &                            MOD(ABS(lon0-lon),360.) .gt. 179.9)
c    &                           then
                              if (ABS(lat0+lat) .le. 0.1
     &                           .and.
     &                           ((lon0 .gt. 0. .and. lon .lt. 0.)
     &                           .or.
     &                            (lon0 .lt. 0. .and. lon .gt. 0.))
     &                           .and.
     &                           (ABS(lon0-lon) .gt. 179.9
     &                           .and.
     &                            ABS(lon0-lon) .lt. 181.1)) then

c                                This is the antipode which should give
c                                a bearing of zero
                                 if (shift_lat .eq. 0) then

c                                   Force bearing to 0 if we are
c                                   looking at the upper half of the map
                                    geo_brng=0.
                                 else

c                                   Force bearing to 180 if we are
c                                   looking at the lower half of the map
                                    geo_brng=pi
                                 end if
                              end if

c                             Move the point to the edge of the map
                              geo_rng=geo_rng_max
                              call RECVR2
     &                            (geo_lng0,
     &                             geo_clt0,geo_cclt0,geo_sclt0,
     &                             geo_brng,geo_rng,
     &                             rlng,rclt,crclt,srclt)

                              lat=90.-rclt/dtr
                              lon=rlng/dtr

                              call GEO_COORD
     &                            (lat,lon,
     &                             geo_rng,geo_brng,geo_x,geo_y,
     &                             plt_x,plt_y)
cxxcDEBUG
cxx                              write(lwpcLOG_lun,
cxx     &                            '(i4,3f7.1,f8.1,2f6.2,''*'')')
cxx     &                              np,lat,lon,
cxx     &                              geo_brng/dtr,geo_rng*6366.,
cxx     &                              plt_x,plt_y
                           end if

c                          Save current position
                           xnp=plt_x
                           ynp=plt_y

                           if (np .gt. n1) then

                              rsq=(xnp-cntrpx(np-1))**2
     &                           +(ynp-cntrpy(np-1))**2
                              tol=500.
                              tol=.25
                              if (rsq .gt. tol) then

c                                The line segment is too long;
c                                we must insert additional points for a
c                                smooth curve
                                 nxtr=MIN(21,INT(rsq/tol+0.5))

c                                Shift arrays to make room for the new
c                                points
                                 do n=nrpts(nrholes),np,-1

                                    cntrpx(n+nxtr)=cntrpx(n)
                                    cntrpy(n+nxtr)=cntrpy(n)
                                 end do

c                                Add extra points by linear interpolation
                                 if (geo_brng-prv_br .gt. pi) then

                                    br_inc=(geo_brng-prv_br-2.*pi)
     &                                    /(nxtr+1)
                                 else
     &                           if (geo_brng-prv_br .lt. -pi) then

                                    br_inc=(geo_brng-prv_br+2.*pi)
     &                                    /(nxtr+1)
                                 else

                                    br_inc=(geo_brng-prv_br)
     &                                    /(nxtr+1)
                                 end if
                                 rng_inc=(geo_rng-prv_rng)/(nxtr+1)

                                 br =prv_br
                                 rng=prv_rng
                                 do n=1,nxtr

c                                   Interpolate bearing
                                    br =br +br _inc
                                    rng=rng+rng_inc

                                    call RECVR2
     &                                  (geo_lng0,geo_clt0,
     &                                   geo_cclt0,geo_sclt0,
     &                                   br,rng,
     &                                   rlng,rclt,crclt,srclt)

                                    lat=90.-rclt/dtr
                                    lon=rlng/dtr

                                    call GEO_COORD
     &                                  (lat,lon,
     &                                   geo_rng,geo_brng,geo_x,geo_y,
     &                                   plt_x,plt_y)

                                    cntrpx(np)=plt_x
                                    cntrpy(np)=plt_y
cxxcDEBUG
cxx                                    write(lwpcLOG_lun,
cxx     &                                  '(i4,3f7.1,f8.1,2f6.2,''**'')')
cxx     &                                    np,lat,lon,
cxx     &                                    geo_brng/dtr,geo_rng*6366.,
cxx     &                                    plt_x,plt_y
                                    np=np+1
                                 end do

c                                Back to the original bearing and range
                                 geo_brng=br +br _inc
                                 geo_rng =rng+rng_inc

c                                Adjust counters
                                 do n=nh,nrholes

                                    nrpts(n)=nrpts(n)+nxtr
                                 end do
                                 nmax=nmax+nxtr
                              end if
                           end if

c                          Store current position
                           cntrpx(np)=xnp
                           cntrpy(np)=ynp

c                          Store current bearing
                           prv_br =geo_brng
                           prv_rng=geo_rng

                           np=np+1
                        end do
                        n1=nmax+1
                     end do
cxxcDEBUG2
cxx                     if (color .eq. 'color7') then
cxx                     call GRF_COLOR
cxx     &                   (color)
cxx
cxx                     call GRF_CURVE
cxx     &                   (cntrpx,cntrpy,nrpts(0),0.,0.,1.,1.,4,20)
cxx
cxx                     do n=1,nrpts(0),10
cxx                        np=n
cxx                        call GRF_NUMBER
cxx     &                      (cntrpx(np),cntrpy(np),.1,FLOAT(np),0.,-1,
cxx     &                      'LB',retX)
cxx                     end do
cxx                     if (np .ne. nrpts(0)) then
cxx                        np=nrpts(0)
cxx                        call GRF_NUMBER
cxx     &                      (cntrpx(np),cntrpy(np),.1,FLOAT(np),0.,-1,
cxx     &                      'LB',retX)
cxx                     end if
cxx                     end if
cxxcDEBUG2
c                    Ensure that all points are inside the boundary
                     call GRF_LIMITS
     &                   (mxholes,nrholes,nrpts,mxcntrp,cntrpx,cntrpy,
     &                    geo_size_x,geo_size_y)

                     if (line .eq. 0) then

                        call GRF_CNTR_FILL
     &                      (0.,1.,0.,1.,fill,color)
                     else

                        call GRF_CNTR_LINE
     &                      (0.,1.,0.,1.,line,color)
                     end if
                  end if
               end if
            end do

            if (ndx_lon1 .ge. 1) then

c              Put array back to original order
               if (MOD(ABS(grd_lon1-grd_lon2),360.) .eq. 0.) then

c                 Global array
                  do n=1,nym
                     call SHIFT_ARRAY
     &                   (v(1,n),mxx,1,nx1,nrx-1)

c                    Copy the new 1st column to the end
                     v(nrx,n)=v(1,n)
                  end do
               else

c                 Not a global array
                  do n=1,nym
                     call SHIFT_ARRAY
     &                   (v(1,n),mxx,1,nx1,nrx)
                  end do
               end if
cxxcDEBUG
cxx               write(lwpcLOG_lun,*)'exit-lon'
cxx               write(lwpcLOG_lun,'((10f6.1))')(v(n,nym/2),n=1,nrx)
            end if
         end if

         if (shift_lon .lt. nr_shift_lon) then

c           Shift the map reference longitude
            shift_lon=shift_lon+1
            if (line .eq. 0) then

               map_lon(1)=map_lon(2)
               map_lon(2)=map_lon(2)-360./nr_shift_lon
               if (map_lon(1) .le. -180. .and.
     &             map_lon(2) .le. -180.) then

                  map_lon(1)=map_lon(1)+360.
                  map_lon(2)=map_lon(2)+360.
               else
     &         if (map_lon(1) .ge.  180. .and.
     &             map_lon(2) .ge.  180.) then

                  map_lon(1)=map_lon(1)-360.
                  map_lon(2)=map_lon(2)-360.
               end if
               go to 21
            else

               if (map_lon(2) .lt. grd_lon2) then

                  map_lon(1)=map_lon(1)+360.
                  map_lon(2)=map_lon(2)+360.
                  if (map_lon(2) .lt. grd_lon1) go to 21
               else
     &         if (map_lon(2) .gt. grd_lon2) then

                  map_lon(1)=map_lon(1)-360.
                  map_lon(2)=map_lon(2)-360.
                  if (map_lon(1) .gt. grd_lon2) go to 21
               end if
            end if
         end if

c        Restore original longitude bounds of the mapped area
         if (line .eq. 0) then

            map_lon(1)=INT(lon0/dlon+.99)*dlon
            map_lon(2)=map_lon(1)-360./nr_shift_lon
         else

            map_lon(1)=INT((lon0+180.)/dlon+.99)*dlon
            if (map_lon(1) .gt.  180.) map_lon(1)=map_lon(1)-360.
            map_lon(2)=map_lon(1)-360.
         end if

         if (ndx_lat1 .gt. 1) then

c           Put array back to original order
            call SHIFT_ARRAY2
     &          (v,mxx,mxy,nrx,1,ny1,nry)
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'exit-lat'
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(1,n),n=1,nry)
         end if

         if (split_lat) then

c           Now shift the data array to expose the lower,
c           visible part of the mapped area
            lat=INT((lat0-geo_rng_max*6.366*9.)
     &              /dlat-.99)*dlat
            map_lat(1)=MAX(grd_lat(1),lat)

            lat=INT((lat0+geo_rng_max*6.366*9.)
     &              /dlat-.99)*dlat
            map_lat(2)=MIN(-lat0,grd_lat(2),lat)

            if (map_lat(1) .lt. map_lat(2)) then

               shift_lat=shift_lat+1
               split_lat=.false.
               go to 11
            end if
         end if

      CASE( 'M','R' )

c        Mercator or Rectangular
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'cntr   ',vcntr
cxx         write(lwpcLOG_lun,*)'grd-lat',nry,grd_lat,dlat
cxx         write(lwpcLOG_lun,*)'grd-lon',nrx,grd_lon,dlon

c        Latitude bounds of the mapped area
         map_lat(1)=geo_corner1_lat
         map_lat(2)=geo_corner2_lat

c        Longitude bounds of the mapped area
         map_lon(1)=geo_corner1_lon
         map_lon(2)=geo_corner2_lon
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'map-lat',map_lat
cxx         write(lwpcLOG_lun,*)'map-lon',map_lon

         if (grd_lat(2) .le. map_lat(1) .or.
     &       grd_lat(1) .ge. map_lat(2)) then

c           The data is either above or below the mapped area;
c           no plot is possible.
            RETURN
         end if

c        Set up counters in latitude
         if (grd_lat(1) .gt. map_lat(1)) then

c           The bottom of the data is above the mapped area;
c           the 1st index and the plot reference is the
c           data coordinate.
            ndx_lat1=1
            plt_lat=grd_lat(1)
         else

c           The bottom of the data is below the mapped area;
c           the 1st index and the plot reference is the
c           map coordinate.
            ndx_lat1=INT((map_lat(1)-grd_lat(1))/dlat)+1
            plt_lat=map_lat(1)
         end if
         if (grd_lat(2) .le. map_lat(2)) then

c           The top of the data is below the mapped area;
c           the 2nd index is referenced to the
c           data coordinate.
            ndx_lat2=nry
         else

c           The top of the data is above the mapped area;
c           the 2nd index is referenced to the
c           map coordinate.
            ndx_lat2=INT((map_lat(2)-grd_lat(1))/dlat)+1
         end if
         ny1=1
         nym=ndx_lat2-ndx_lat1+1
cxxcDEBUG
cxx         write(lwpcLOG_lun,*)'ndx-lat',ndx_lat1,ndx_lat2,map_lat

         if (ndx_lat1 .gt. 1) then

c           Shift the grid data in latitude
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'original-lat'
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(1,n),n=1,nry)

            call SHIFT_ARRAY2
     &          (v,mxx,mxy,nrx,1,ndx_lat1,nry)

c           Keep track of the original first row
            ny1=2-ndx_lat1
            if (ny1 .lt. 1) ny1=ny1+nry
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'shift-lat',ny1,nym
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(1,n),n=1,nry)
         end if

c        Ensure that the data grid and the map grid are properly
c        aligned for the possible longitude shifts to be used.
         if (grd_lon1 .gt. map_lon(1) .and.
     &       grd_lon2 .gt. map_lon(1)) then

            grd_lon1=grd_lon1-360.
            grd_lon2=grd_lon2-360.
         else
     &   if (grd_lon1 .lt. map_lon(2) .and.
     &       grd_lon2 .lt. map_lon(2)) then

            grd_lon1=grd_lon1+360.
            grd_lon2=grd_lon2+360.
         end if

c        There are only two possible shifts in longitude
         do shift_lon=1,2

c           Set up counters in longitude
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'map-lon',shift_lon,map_lon

            search=.false.
            ndx_lon1=0
            plt_lon=99.
            if (grd_lon1 .le. map_lon(1) .and.
     &          grd_lon1 .gt. map_lon(2)) then

c              Left edge of the data is bounded by the mapped area.
               search=.true.
               ndx_lon1=1
               plt_lon=grd_lon1
            else
     &      if (grd_lon1 .gt. map_lon(1)) then

c              Left edge of the data is to the left of the
c              left edge of the mapped area.
               search=.true.
               ndx_lon1=INT((grd_lon1-map_lon(1))/dlon)+1
               plt_lon=map_lon(1)
            end if
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'ndx-lon',search,ndx_lon1,plt_lon

            if (search .and.
     &          ndx_lon1 .ge. 1 .and. ndx_lon1 .lt. nrx) then

               if (grd_lon2 .ge. map_lon(2)) then

c                 The right edge of the data is to the left of the
c                 right edge of mapped area; the 2nd index is the
c                 data coordinate.
                  ndx_lon2=nrx
               else

c                 The right edge of the data is to the right of the
c                 right edge of mapped area; the 2nd index is the
c                 map coordinate.
                  ndx_lon2=MIN(nrx,INT((grd_lon1-map_lon(2))/dlon+.9)+1)
               end if

               nx1=1
               nxm=ndx_lon2-ndx_lon1+1
cxxcDEBUG
cxx               write(lwpcLOG_lun,*)'ndx-lon',ndx_lon1,ndx_lon2,map_lon

               if (ndx_lon1 .gt. 1) then

c                 Shift the grid data in longitude
cxxcDEBUG
cxx                  write(lwpcLOG_lun,*)'original-lon'
cxx                  write(lwpcLOG_lun,'((10f6.1))')(v(n,nym/2),n=1,nrx)

                  if (MOD(ABS(grd_lon1-grd_lon2),360.) .eq. 0.) then

c                    Global array
                     do n=1,nym
                        call SHIFT_ARRAY
     &                      (v(1,n),mxx,1,ndx_lon1,nrx-1)

c                       Copy the new 1st column to the end
                        v(nrx,n)=v(1,n)
                     end do

c                    Keep track of the original first column
                     nx1=2-ndx_lon1
                     if (nx1 .lt. 1) nx1=nx1+nrx-1
                  else

c                    Not a global array
                     do n=1,nym
                        call SHIFT_ARRAY
     &                      (v(1,n),mxx,1,ndx_lon1,nrx)
                     end do

c                    Keep track of the original first column
                     nx1=2-ndx_lon1
                     if (nx1 .lt. 1) nx1=nx1+nrx
                  end if
cxxcDEBUG
cxx                  write(lwpcLOG_lun,*)'shift-lon',shift_lon,nx1
cxx                  write(lwpcLOG_lun,'((10f6.1))')(v(n,nym/2),n=1,nrx)
               end if

c              Search for contours
               status='begin'
               do while (status .ne. 'complete')

                  call GRF_CNTR
     &                (v,mxx,mxy,nxm,nym,
     &                 vcntr,status)

                  if (nrpts(0) .gt. 0) then

c                    Convert to longitude, latitude
                     do np=1,nrpts(nrholes)

                        lat=plt_lat+(cntrpy(np)-1.)*dlat
                        lon=plt_lon-(cntrpx(np)-1.)*dlon

                        cntrpy(np)=lat
                        cntrpx(np)=lon
                     end do
                  end if

                  if (save_cntr) then

c                    Prepare to write contour data to lwpcTMP_lun
                     call GRF_CNTR_JOIN

                     write(lwpcTMP_lun) nrholes,nrpts

                     if (nrpts(0) .gt. 0) then

c                       Save the coordinates to the data file
                        write(lwpcTMP_lun) shift_lat,
     &                       (cntrpx(n),cntrpy(n),n=1,nrpts(0))
                     end if
                  end if

                  if (nrpts(0) .gt. 0) then

c                    Convert to inches
                     do np=1,nrpts(nrholes)

                        lat=cntrpy(np)
                        lon=cntrpx(np)

                        call GEO_COORD
     &                      (lat,lon,
     &                       geo_rng,geo_brng,geo_x,geo_y,
     &                       plt_x,plt_y)

                        cntrpx(np)=plt_x
                        cntrpy(np)=plt_y
                     end do

                     if (line .eq. 0) then

                        call GRF_CNTR_FILL
     &                      (0.,1.,0.,1.,
     &                       fill,color)
                     else

                        call GRF_CNTR_LINE
     &                      (0.,1.,0.,1.,
     &                       line,color)
                     end if
                  end if
               end do

               if (line .eq. 0 .and. ndx_lon1 .gt. 1) then

c                 Put array back to the original order
                  if (MOD(ABS(grd_lon1-grd_lon2),360.) .eq. 0.) then

c                    Global array
                     do n=1,nym
                        call SHIFT_ARRAY
     &                      (v(1,n),mxx,1,nx1,nrx-1)

                        v(nrx,n)=v(1,n)
                     end do
                  else

c                    Not a global array
                     do n=1,nym
                        call SHIFT_ARRAY
     &                      (v(1,n),mxx,1,nx1,nrx)
                     end do
                  end if
cxxcDEBUG
cxx                  write(lwpcLOG_lun,*)'exit-lon'
cxx                  write(lwpcLOG_lun,'((10f6.1))')(v(n,nym/2),n=1,nrx)
               end if
            end if

c           Shift longitude
            grd_lon1=grd_lon1+360.
            grd_lon2=grd_lon2+360.
            if (grd_lon2 .ge. map_lon(1)) then
               grd_lon1=grd_lon1-720.
               grd_lon2=grd_lon2-720.
            end if
         end do

         if (ndx_lat1 .gt. 1) then

c           Put array back to original order
            call SHIFT_ARRAY2
     &          (v,mxx,mxy,nrx,1,ny1,nry)
cxxcDEBUG
cxx            write(lwpcLOG_lun,*)'exit-lat'
cxx            write(lwpcLOG_lun,'((10f6.1))')(v(1,n),n=1,nry)
         end if

      END SELECT

      RETURN
      END      ! GEO_CNTR
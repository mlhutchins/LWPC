      SUBROUTINE GEO_COAST_MAP
     &          (map_option,map_index)

c***********************************************************************
c                   subroutine geo_coast_map
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     06 Apr 1996

c  Function:
c     This program provides graphical display of coastal outlines using
c     the WDB data set. The original data base was prepared and put in
c     the public domain by Fred Pospechil and Antonio Riveria. The data
c     bases have been concatenated into a single file. The data files
c     start with headers that start with different values:

c        coastlines 1000
c        islands    5000
c        lakes      6000
c        rivers     7000

c     The data base was modified by Jerry Ferguson to concatenate line
c     segments to enable polyfills of the land masses (it also seems to
c     speed up the display of the coastal outlines); certain islands
c     were stored under the lakes data; the appropriate line segments
c     were moved; a number of small islands were in the coastline data
c     base; these were moved to the islands data base.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     map_option    [s] map type: coast, land
c     map_index     [i] range of line segments to plot

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     close

c     geo_coord
c     grf_cntr_fill
c     grf_color
c     grf_curve
c     grf_limits
c     open_dat
c     recvr2

c     Requires COAST$D.DAT

c  References:

c  Change History:

c     18 Dec 1997   Added stereographic projection.

c     10 Oct 1999   Better test for the antipode inserted.

c 2/10/10 MLH   Changed STRUCTURE command to TYPE command

c*******************!***************************************************

      IMPLICIT NONE

      character*(*) map_option
      integer       map_index(2)

c     LWPC parameters
      include      'lwpc_geo.cmn'
      include      'lwpc_lun.cmn'
      include      'grf_cntr.cmn'

c     Coast map data
      integer       mxcoord
      integer       nrcoord

      parameter    (mxcoord=10650)

      TYPE segment
         integer*2  ndx
         integer*2  nrcoord
         integer*2  nrcoast
         integer*2  lat(mxcoord)
         integer*2  lon(mxcoord)
      END TYPE
      TYPE(segment) line

      character*  1 option
      character*  8 color
      character* 40 file_name
      integer  *  2 index(2)
      integer       k
      integer       n
      integer       m
      integer       mxpass
      integer       nrpass
      real          coast_lat(mxcoord)
      real          coast_lon(mxcoord)
      real          crclt
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x
      real          geo_y
      real          lat
      real          latx
      real          lon
      real          lonx
      real          pi
      real          prv_brng
      real          plt_x
      real          prv_x
      real          plt_y
      real          prv_y
      real          rclt
      real          rlng
      real          tst_scale
      real          srclt

      data          file_name/'coast$d.dat'/

      data          dtr/.01745329252/
      data          pi/3.14159265359/


c     Open the data file:
      call OPEN_DAT (lwpcCOAST_lun,file_name,'unformatted')

c     Get map options
      option=map_option
      call STR_LOWER (option,0,0)

      index(1)=MIN(map_index(1),map_index(2))
      index(2)=MAX(map_index(1),map_index(2))

c     Store current color
      call GRF_COLOR ('store')

      nrholes=0

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal, Gnomonic, Orthographic or Stereographic

31       read (lwpcCOAST_lun)
     &         line%ndx,
     &         line%nrcoord,
     &         line%nrcoast,
     &        (line%lat(n),
     &         line%lon(n),n=1,line%nrcoord)

c        Check for data to be plotted
         if (line%ndx .lt. index(1)) go to 31

         if (line%ndx .le. index(2)) then

            if (option(1:1) .eq. 'c') then

c              Set number of points for line plot
               nrcoord=line%nrcoast
            else

c              Set number of points for fill plot
               nrcoord=line%nrcoord
            end if

c           Check if this area is inside of mapped area
            k=0
            n=0
            latx=-999.
            lonx=-999.
            do m=1,nrcoord

               coast_lat(m)= line%lat(m)/60.
               coast_lon(m)=-line%lon(m)/60.

               lat=coast_lat(m)
               lon=coast_lon(m)

               if (ABS(lat-latx) .gt. .5 .or.
     &             ABS(lon-lonx) .gt. .5) then

                  call GEO_COORD
     &                (lat,lon,
     &                 geo_rng,geo_brng,geo_x,geo_y,
     &                 plt_x,plt_y)

                  k=k+1
                  if (geo_rng .lt. geo_rng_max) n=n+1

                  latx=lat
                  lonx=lon
               end if
            end do
            if (n .eq. 0) go to 31

c           Set up plot resolution
            tst_scale=.0001

            nrpass=0
            mxpass=1
            do while (nrpass .lt. mxpass)

               m=1
               prv_x=99.
               prv_y=99.
               nrcntrp=0
               do while (m .le. nrcoord)

                  lat=coast_lat(m)
                  lon=coast_lon(m)

                  call GEO_COORD
     &                (lat,lon,
     &                 geo_rng,geo_brng,geo_x,geo_y,
     &                 plt_x,plt_y)

                  if (geo_rng .gt. geo_rng_max) then

                     if (ABS(geo_center_lat+lat) .le. 0.1
     &                  .and.
     &                  ((geo_center_lon .gt. 0. .and. lon .lt. 0.)
     &                  .or.
     &                   (geo_center_lon .lt. 0. .and. lon .gt. 0.))
     &                  .and.
     &                  (ABS(geo_center_lon-lon) .gt. 179.9
     &                  .and.
     &                   ABS(geo_center_lon-lon) .lt. 181.1)) then

c                       This is the antipode which should give a
c                       bearing of zero
                        if (prv_brng .lt. 1.57 .or.
     &                      prv_brng .gt. 4.71) then

c                          Force bearing to 0
                           geo_brng=0.
                        else

c                          Force bearing to 180
                           geo_brng=pi
                        end if
                     end if

c                    Move the point to the edge of the map
                     geo_rng=geo_rng_max
                     call RECVR2
     &                   (geo_lng0,
     &                    geo_clt0,geo_cclt0,geo_sclt0,
     &                    geo_brng,geo_rng,
     &                    rlng,rclt,crclt,srclt)

                     lat=90.-rclt/dtr
                     lon=rlng/dtr

                     call GEO_COORD
     &                   (lat,lon,
     &                    geo_rng,geo_brng,geo_x,geo_y,
     &                    plt_x,plt_y)
                  end if
                  prv_brng=geo_brng

                  if ((plt_x-prv_x)**2
     &               +(plt_y-prv_y)**2 .ge. tst_scale) then

                     if (nrcntrp .eq. mxcntrp) then

c                       Current plot resolution is too fine;
c                       restart with coarser value
                        tst_scale=tst_scale*5.
                        m=nrcoord
                        nrcntrp=-1
                        mxpass=mxpass+1
                     else

c                       Store this point
                        nrcntrp=nrcntrp+1
                        cntrpx (nrcntrp)=plt_x
                        cntrpy (nrcntrp)=plt_y

                        prv_x=plt_x
                        prv_y=plt_y
                     end if
                  end if
                  m=m+1
               end do
               nrpass=nrpass+1
            end do

            if (nrcntrp .gt. 0) then

               nrpts(0)=nrcntrp

c              Ensure that all points are inside the boundary
               call GRF_LIMITS
     &             (mxholes,nrholes,nrpts,mxcntrp,cntrpx,cntrpy,
     &              geo_size_x,geo_size_y)

               if (option(1:1) .eq. 'c') then

c                 Set coastal outline color
                  color='coast'
                  call GRF_COLOR
     &                (color)
                  call GRF_CURVE
     &                (cntrpx,cntrpy,nrcntrp,0.,0.,1.,1.,1,20)
               else

                  if (line%ndx .ge. 6000) then

c                    Set lake fill color
                     color='lakes'
                  else

c                    Set land mass fill color
                     color='land'
                  end if

                  call GRF_CNTR_FILL
     &                (0.,1.,0.,1.,'100%',color)
               end if
            end if
            go to 31
         end if

      CASE( 'M','R' )

c        Mercator or Rectangular

51       read (lwpcCOAST_lun)
     &         line%ndx,
     &         line%nrcoord,
     &         line%nrcoast,
     &        (line%lat(n),
     &         line%lon(n),n=1,line%nrcoord)

c        Check for data to be plotted
         if (line%ndx .lt. index(1)) go to 51

         if (line%ndx .le. index(2)) then

            if (option(1:1) .eq. 'c') then

c              Set number of points for line plot
               nrcoord=line%nrcoast
            else

c              Set number of points for fill plot
               nrcoord=line%nrcoord
            end if

c           Extract coordinates
            do m=1,nrcoord

               coast_lat(m)= line%lat(m)/60.
               coast_lon(m)=-line%lon(m)/60.
            end do

            if (line%ndx .eq. 1024) then

c              Antarctica;
c              shift the coordinates to put the 180 meridian at the
c              beginning of the arrays
               k=3991
               call SHIFT_ARRAY
     &             (coast_lat,mxcoord,1,k,nrcoord)
               call SHIFT_ARRAY
     &             (coast_lon,mxcoord,1,k,nrcoord)

c              Now, add points to close the filled land mass
               nrcoord=nrcoord+1
               coast_lon(nrcoord)=coast_lon(1)-360.
               coast_lat(nrcoord)=coast_lat(1)
               nrcoord=nrcoord+1
               coast_lon(nrcoord)=coast_lon(1)-360.
               coast_lat(nrcoord)=-90.
               nrcoord=nrcoord+1
               coast_lon(nrcoord)=coast_lon(1)
               coast_lat(nrcoord)=-90.
               nrcoord=nrcoord+1
               coast_lon(nrcoord)=coast_lon(1)
               coast_lat(nrcoord)=coast_lat(1)
            end if

c           Set up plot resolution
            tst_scale=.0001

            nrpass=0
            mxpass=1
            do while (nrpass .lt. mxpass)

               m=1
               prv_x=99.
               prv_y=99.
               nrcntrp=0
               do while (m .lt. nrcoord)

                  lat=coast_lat(m)
                  lon=coast_lon(m)

                  call GEO_COORD
     &                (lat,lon,
     &                 geo_rng,geo_brng,geo_x,geo_y,
     &                 plt_x,plt_y)

                  if (nrpass .eq. 0) then

                     if (lon .gt. geo_corner1_lon) then

c                       This line extends to the left of the map;
c                       set up a second pass
                        mxpass=2
                        plt_x=0.
                     else
     &               if (lon .lt. geo_corner2_lon) then

c                       This line extends to the right of the map;
c                       set up a second pass
                        mxpass=2
                        plt_x=geo_size_x
                     end if
                  else

c                    This is the second pass;
c                    exclude what has already been drawn
                     if (lon .ge. geo_corner1_lon) then

                        plt_x=0.
                     else
     &               if (lon .le. geo_corner2_lon) then

                        plt_x=geo_size_x
                     end if
                  end if

                  if (plt_y .lt.         0.) then

                     plt_y=0.
                  else
     &            if (plt_y .gt. geo_size_y) then

                     plt_y=geo_size_y
                  end if

                  if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &                plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

                     if (nrcntrp .eq. 0 .or.
     &                  ((plt_x-prv_x)**2
     &                  +(plt_y-prv_y)**2 .ge. tst_scale)) then

                        if (nrcntrp .eq. mxcntrp-3) then

c                          Current plot resolution is too fine;
c                          restart with coarser value
                           tst_scale=tst_scale*5.
                           m=nrcoord
                           nrcntrp=-1
                        else

c                          Store this point
                           nrcntrp=nrcntrp+1
                           cntrpx (nrcntrp)=plt_x
                           cntrpy (nrcntrp)=plt_y

                           prv_x=plt_x
                           prv_y=plt_y
                        end if
                     end if
                  end if
                  m=m+1
               end do

               if (nrcntrp .gt. 0) then

                  if (option(1:1) .eq. 'c') then

                     if (line%ndx .eq. 1024) then

c                       Antarctica;
c                       don't close the line
                        nrcntrp=nrcntrp-1
                     end if

c                    Set coastal outline color
                     color='coast'
                     call GRF_COLOR
     &                   (color)
                     call GRF_CURVE
     &                   (cntrpx,cntrpy,nrcntrp,0.,0.,1.,1.,1,20)
                  else

                     nrpts(0)=nrcntrp

                     if (line%ndx .ge. 6000) then

c                       Set lake fill color
                        color='lakes'
                     else

c                       Set land mass fill color
                        color='land'
                     end if

                     call GRF_CNTR_FILL
     &                   (0.,1.,0.,1.,'100%',color)
                  end if
               end if

               if (nrcntrp .ge. 0) then

                  nrpass=nrpass+1
                  if (nrpass .lt. mxpass) then

                     if (geo_corner1_lon .eq. 180.) then

c                       Shift this line segment to the left and
c                       do another pass
                        do m=1,nrcoord
                           coast_lon(m)=coast_lon(m)+360.
                        end do
                     else

c                       Shift this line segment to the right and
c                       do another pass
                        do m=1,nrcoord
                           coast_lon(m)=coast_lon(m)-360.
                        end do
                     end if
                  end if
               end if
            end do
            go to 51
         end if

      END SELECT

      CLOSE(lwpcCOAST_lun)

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_COAST_MAP
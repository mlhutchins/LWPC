      SUBROUTINE GEO_DEFS
     &          (map_projection,map_lat,map_lon,map_size)

c***********************************************************************
c                   subroutine geo_defs
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     This subroutine sets up parameters for geographic displays.

c     Projection    LAT(1) LON(1) LAT(2) LON(2) SIZE(1) SIZE(2)
c     rectangular   lat1   lon1   lat2   lon2   size x  size y
c     mercator      lat1   lon1   lat2   lon2   size x  size y
c     gnomonic      lat0   lon0   range  N/A    size x  size y
c     azimuthal     lat0   lon0   range  N/A    size x  size y
c     orthographic  lat0   lon0   range  N/A    size x  size y
c                   stereographic

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     map_projection[s  ] projection indicator;
c                         ='azim':  Azimuthal equidistant
c                         ='gnom':  Gnomonic
c                         ='merc':  Mercator
c                         ='orth':  Orthographic
c                         ='rect':  linear in longitude and latitude

c     map_lat       [r,2] latitude  bounds; deg North;

c     map_lon       [r,2] longitude bounds; deg West;

c     map_size      [r,2] lengths of map axes; inches;

c                         See table above for special handling for
c                         MAP_PROJECTION='azim', 'gnom' and 'ortho'.

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     atan
c     cos
c     max
c     min
c     sin
c     sqrt
c     tan

c     geo_coord
c     lwpc_error
c     str_upper

c  References:

c  Change History:

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      character*(*) map_projection
      real          map_lat     (2)
      real          map_lon     (2)
      real          map_size    (2)

      character*200 error_msg
      integer       inside
      real          br
      real          crclt
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x1
      real          geo_x2
      real          geo_y1
      real          geo_y2
      real          lat
      real          lon
      real          plt_x1
      real          plt_x2
      real          plt_y1
      real          plt_y2
      real          rclt
      real          rlng
      real          srclt
      real          tmp

      data          dtr/0.01745329252/


c     Set values in LWPC map common
      geo_projection=map_projection

      geo_size_x=map_size(1)
      geo_size_y=map_size(2)

c     Store parameter for map selection
      geo_prjctn=geo_projection
      call STR_UPPER (geo_prjctn,0,0)

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal Equidistant, Gnomonic, Orthographic or Stereographic
         geo_center_lat=map_lat(1)
         geo_center_lon=map_lon(1)
         geo_range _max=map_lat(2)

c        Ensure that the longitude is +/- 180
         if (geo_center_lon .lt. -180.) then
            geo_center_lon=MOD(geo_center_lon,360.)+360.
         else
     &   if (geo_center_lon .gt. 180.) then
            geo_center_lon=MOD(geo_center_lon,360.)-360.
         end if

         geo_clt0=(90.-geo_center_lat)*dtr
         geo_cclt0=COS(geo_clt0)
         geo_sclt0=SIN(geo_clt0)
         geo_lng0=geo_center_lon*dtr

c        Scaling
         SELECT CASE( geo_prjctn )

         CASE( 'A' )

            geo_rng_max=MIN(geo_range_max,10000.)/6366.

            geo_scale_y=.5*MAX(geo_size_x,geo_size_y)/geo_rng_max

c           Scaling is chosen to force a circular presentation
            geo_scale_x=geo_scale_y

            geo_origin_x=.5*geo_size_x
            geo_origin_y=.5*geo_size_y

c           Adjust range max to include the corners
            tmp=SQRT((.5*geo_size_x)**2+(.5*geo_size_y)**2)/geo_scale_y

            geo_rng_max=MIN(19999.,INT(tmp*6366./1000.+.99)*1000.)/6366.

         CASE( 'G' )

c           Limit range max to practical value
            geo_rng_max=MIN(geo_range_max,8000.)/6366.

            if (geo_center_lat .gt. 0.) then

               call GEO_COORD
     &             (geo_center_lat-9.*geo_rng_max*6.366,geo_center_lon,
     &              geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)
            else

               call GEO_COORD
     &             (geo_center_lat+9.*geo_rng_max*6.366,geo_center_lon,
     &              geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)
            end if

            geo_scale_y=.5*MAX(geo_size_x,geo_size_y)/ABS(geo_y1)

c           Scaling is chosen to force a circular presentation
            geo_scale_x=geo_scale_y

            geo_origin_x=.5*geo_size_x
            geo_origin_y=.5*geo_size_y

c           Adjust range max to include the corners
            inside=1
            tmp=geo_rng_max*6366.
            do while (inside .gt. 0)

               inside=0
               if (tmp .lt. 10000.) then

                  geo_rng_max=tmp/6366.

                  do br=0.,180.,10.

                     geo_brng=br*dtr
                     geo_rng=geo_rng_max

                     call RECVR2
     &                   (geo_lng0,geo_clt0,
     &                    geo_cclt0,geo_sclt0,
     &                    geo_brng,geo_rng,
     &                    rlng,rclt,crclt,srclt)

                     lat=90.-rclt/dtr
                     lon=rlng/dtr

                     call GEO_COORD
     &                   (lat,lon,
     &                    geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)

                     if (plt_x1 .ge.         0. .and.
     &                   plt_x1 .le. geo_size_x .and.
     &                   plt_y1 .ge.         0. .and.
     &                   plt_y1 .le. geo_size_y) inside=inside+1
                  end do
                  tmp=tmp+1000.
               end if
            end do

         CASE( 'O' )

c           Limit range max to practical value
            geo_rng_max=MIN(geo_range_max,10000.)/6366.

            geo_scale_y=.5*MAX(geo_size_x,geo_size_y)/SIN(geo_rng_max)

c           Scaling is chosen to force a circular presentation
            geo_scale_x=geo_scale_y

            geo_origin_x=.5*geo_size_x
            geo_origin_y=.5*geo_size_y

c           Adjust range max to include the corners
            tmp=SQRT((.5*geo_size_x)**2+(.5*geo_size_y)**2)/geo_scale_y

            if (tmp .ge. 1.) then

               geo_rng_max=10000./6366.
            else

               geo_rng_max=INT(ASIN(tmp)*6366./1000.+.99)*1000./6366.
            end if

         CASE( 'S' )

c           Limit range max to practical value
            geo_rng_max=MIN(geo_range_max,10000.)/6366.

            tmp=SIN(geo_rng_max)/(1.+COS(geo_rng_max))
            geo_scale_y=.5*MAX(geo_size_x,geo_size_y)/(2.*tmp)

c           Scaling is chosen to force a circular presentation
            geo_scale_x=geo_scale_y

            geo_origin_x=.5*geo_size_x
            geo_origin_y=.5*geo_size_y

c           Adjust range max to include the corners
            inside=1
            tmp=geo_rng_max*6366.
            do while (inside .gt. 0)

               inside=0
               if (tmp .lt. 10000.) then

                  geo_rng_max=tmp/6366.

                  do br=0.,180.,10.

                     geo_brng=br*dtr
                     geo_rng=geo_rng_max

                     call RECVR2
     &                   (geo_lng0,geo_clt0,
     &                    geo_cclt0,geo_sclt0,
     &                    geo_brng,geo_rng,
     &                    rlng,rclt,crclt,srclt)

                     lat=90.-rclt/dtr
                     lon=rlng/dtr

                     call GEO_COORD
     &                   (lat,lon,
     &                    geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)

                     if (plt_x1 .ge.         0. .and.
     &                   plt_x1 .le. geo_size_x .and.
     &                   plt_y1 .ge.         0. .and.
     &                   plt_y1 .le. geo_size_y) inside=inside+1
                  end do
                  tmp=tmp+1000.
               end if
            end do

         END SELECT

      CASE( 'M','R' )

c        Mercator or Rectangular

c        Define some working values
         geo_corner1_lat=map_lat(1)
         geo_corner1_lon=map_lon(1)
         geo_corner2_lat=map_lat(2)
         geo_corner2_lon=map_lon(2)

         if (geo_corner2_lon .ge. geo_corner1_lon)

     &      geo_corner2_lon=geo_corner2_lon-360.

         if (geo_corner1_lon .eq. -180. .and.
     &       geo_corner2_lon .eq. -540.) then

c           Correct this special (ambiguous) case
            geo_corner1_lon= 180.
            geo_corner2_lon=-180.
         end if

c        Scaling
         geo_scale_x=1.
         geo_scale_y=1.

         geo_origin_x=0.
         geo_origin_y=0.

         SELECT CASE( geo_prjctn )

         CASE( 'M' )

            geo_clt0=0.

            call GEO_COORD
     &          (geo_corner1_lat,geo_corner1_lon,
     &           geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)

            geo_clt0=geo_y1

            call GEO_COORD
     &          (geo_corner2_lat,geo_corner2_lon,
     &           geo_rng,geo_brng,geo_x2,geo_y2,plt_x2,plt_y2)

            geo_scale_x=geo_size_x/(geo_x2-geo_x1)
            geo_scale_y=geo_size_y/ geo_y2

            call GEO_COORD
     &          (geo_corner1_lat,geo_corner1_lon,
     &           geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)

            call GEO_COORD
     &          (geo_corner2_lat,geo_corner2_lon,
     &           geo_rng,geo_brng,geo_x2,geo_y2,plt_x2,plt_y2)

         CASE( 'R' )

            call GEO_COORD
     &          (geo_corner1_lat,geo_corner1_lon,
     &           geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)

            call GEO_COORD
     &          (geo_corner2_lat,geo_corner2_lon,
     &           geo_rng,geo_brng,geo_x2,geo_y2,plt_x2,plt_y2)

            geo_scale_x=geo_size_x/(geo_x2-geo_x1)
            geo_scale_y=geo_size_y/(geo_y2-geo_y1)

            call GEO_COORD
     &          (geo_corner1_lat,geo_corner1_lon,
     &           geo_rng,geo_brng,geo_x1,geo_y1,plt_x1,plt_y1)

            call GEO_COORD
     &          (geo_corner2_lat,geo_corner2_lon,
     &           geo_rng,geo_brng,geo_x2,geo_y2,plt_x2,plt_y2)

         END SELECT

         geo_origin_x=-plt_x1
         geo_origin_y=-plt_y1

      CASE DEFAULT

         write(error_msg,
     &       '(''[GEO_DEFS]:'',1x,
     &         ''projection name: '',a,1x,
     &         ''not recognized'')') map_projection
         call LWPC_ERROR('ERROR',error_msg)

      END SELECT

      RETURN
      END      ! GEO_DEFS
      SUBROUTINE GEO_COORD
     &          (geo_lat,geo_lon,
     &           geo_rng,geo_brng,
     &           geo_x,geo_y,
     &           plt_x,plt_y)

c***********************************************************************
c                   subroutine geo_coord
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     This subroutine converts map coordinates to plot coordinates.

c     Map projections:  azimuthal equidistant
c                       gnomonic
c                       mercator
c                       orthographic
c                       rectangular
c                   stereographic

c     Latitudes and longitudes in radians
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     geo_lat       [r] latitude  to be converted
c     geo_lon       [r] longitude to be converted

c  Parameters returned:
c     geo_rng       [r] range   of input point from map center
c     geo_brng      [r] bearing of input point from map center
c     geo_x         [r] x coordinate of mapped position
c     geo_y         [r] y coordinate of mapped position
c     plt_x         [r] x coordinate of mapped position in inches
c     plt_y         [r] y coordinate of mapped position in inches

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     atan
c     cos
c     log
c     sign
c     sin
c     tan

c     gcdbr2

c  References:

c  Change History:

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      real          clt
      real          cnvrt     ! Function
      real          dtr
      real          geo_brng
      real          geo_lat
      real          geo_lon
      real          geo_rng
      real          geo_x
      real          geo_y
      real          lng
      real          plt_x
      real          plt_y
      real          t1
      real          t2
      real          t3
      real          tmp

      data          dtr/0.01745329252/

c     Conversion of latitude to inches for Mercator projection.
      CNVRT(tmp)=SIGN(LOG(TAN((45.+.5*ABS(tmp))*dtr)),tmp)


      SELECT CASE (geo_prjctn)

      CASE( 'A' )

c        Azimuthal Equidistant
         clt=(90.-geo_lat)*dtr
         lng=geo_lon*dtr

         call GCDBR2
     &       (geo_lng0-lng,geo_clt0,geo_cclt0,geo_sclt0,
     &        clt,COS(clt),SIN(clt),geo_rng,geo_brng,0)

         geo_x=geo_rng*SIN(geo_brng)
         geo_y=geo_rng*COS(geo_brng)

      CASE( 'G' )

c        Gnomonic
         clt=(90.-geo_lat)*dtr
         lng=geo_lon*dtr

         call GCDBR2
     &       (geo_lng0-lng,geo_clt0,geo_cclt0,geo_sclt0,
     &        clt,COS(clt),SIN(clt),geo_rng,geo_brng,0)

         t1=          SIN(clt)*SIN(geo_lng0-lng)
         t2=geo_sclt0*COS(clt)
         t3=geo_cclt0*SIN(clt)*COS(geo_lng0-lng)

         geo_x= t1    /COS(geo_rng)
         geo_y=(t2-t3)/COS(geo_rng)

      CASE( 'M' )

c        Mercator
         tmp=geo_lon
         if (tmp .gt. geo_corner1_lon) tmp=tmp-360.

         geo_x=tmp-geo_corner1_lon
         geo_y=CNVRT(geo_lat)-geo_clt0

      CASE( 'O' )

c        Orthographic
         clt=(90.-geo_lat)*dtr
         lng=geo_lon*dtr

         call GCDBR2
     &       (geo_lng0-lng,geo_clt0,geo_cclt0,geo_sclt0,
     &        clt,COS(clt),SIN(clt),geo_rng,geo_brng,0)

         geo_x=SIN(geo_rng)*SIN(geo_brng)
         geo_y=SIN(geo_rng)*COS(geo_brng)

      CASE( 'R' )

c        Rectangular
         tmp=geo_lon
         if (tmp .gt. geo_corner1_lon) tmp=tmp-360.

         geo_x=tmp-geo_corner1_lon
         geo_y=geo_lat-geo_corner1_lat

      CASE( 'S' )

c        Stereographic
         clt=(90.-geo_lat)*dtr
         lng=geo_lon*dtr

         call GCDBR2
     &       (geo_lng0-lng,geo_clt0,geo_cclt0,geo_sclt0,
     &        clt,COS(clt),SIN(clt),geo_rng,geo_brng,0)

         if (geo_rng*6366. .ge. 10000.) then
            geo_x=2.*SIN(geo_brng)
            geo_y=2.*COS(geo_brng)
         else
            tmp=SIN(geo_rng)/(1.+COS(geo_rng))
            geo_x=2.*tmp*SIN(geo_brng)
            geo_y=2.*tmp*COS(geo_brng)
         end if

      END SELECT

      plt_x=geo_origin_x+geo_scale_x*geo_x
      plt_y=geo_origin_y+geo_scale_y*geo_y

      RETURN
      END      ! GEO_COORD
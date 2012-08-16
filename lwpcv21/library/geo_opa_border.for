      SUBROUTINE GEO_OPA_BORDER
     &          (opa_lat,opa_lon)

c***********************************************************************
c                   subroutine geo_opa_border
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     This program draws outline of an op area for geographic displays.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     opa_lat       [r,2] latitude  bounds of op area
c     opa_lon       [r,2] longitude bounds of op area

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs

c     geo_coord
c     grf_color
c     grf_draw
c     grf_move

c  References:

c  Change History:

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      real          opa_lat(2)
      real          opa_lon(2)

      logical       outside
      logical       split
      integer       outline
      real          corner1_lat
      real          corner1_lon
      real          corner2_lat
      real          corner2_lon
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x
      real          geo_y
      real          grid_minor
      real          lat
      real          lon
      real          opa1_lat
      real          opa1_lon
      real          opa2_lat
      real          opa2_lon
      real          plt_x
      real          plt_y
      real          rng_max
      real          scale_x
      real          scale_y
      real          step_lat
      real          step_lon

      data          dtr/.01745329252/
      data          grid_minor/1./


c     Transfer input to local variable
      corner1_lat=geo_corner1_lat
      corner1_lon=geo_corner1_lon
      corner2_lat=geo_corner2_lat
      corner2_lon=geo_corner2_lon
      scale  _x  =geo_scale  _x
      scale  _y  =geo_scale  _y
      rng_max    =geo_rng_max

      opa1_lat=opa_lat(1)
      opa2_lat=opa_lat(2)
      opa1_lon=opa_lon(1)
      opa2_lon=opa_lon(2)

      if (opa2_lon .ge. opa1_lon)
     &   opa2_lon=opa2_lon-360.

c     Store current color
      call GRF_COLOR ('store')

c     Set oparea color
      call GRF_COLOR ('oparea')

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal Equidistant, Gnomonic, Orthographic or Stereographic

c        Draw op area
         step_lat=.005/(scale_y*dtr)
         step_lon=.005/(scale_x*dtr)
         do outline=0,4

c           Draw left meridian
c           in increments of GRID_MINOR degrees
            outside=.true.
            lon=opa1_lon-step_lon*outline
            do lat=opa1_lat,opa2_lat,grid_minor

               call GEO_COORD
     &             (lat,lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &             geo_rng .le. rng_max) then

                  if (outside) then

                     call GRF_MOVE (plt_x,plt_y)
                  else

                     call GRF_DRAW (plt_x,plt_y)
                  end if
                  outside=.false.
               else

                  outside=.true.
               end if
            end do

c           Draw top latitude
c           in increments of GRID_MINOR degrees
            outside=.true.
            lat=opa2_lat-step_lat*outline
            do lon=opa1_lon,opa2_lon,-grid_minor

               call GEO_COORD
     &             (lat,lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &             geo_rng .le. rng_max) then

                  if (outside) then

                     call GRF_MOVE (plt_x,plt_y)
                  else

                     call GRF_DRAW (plt_x,plt_y)
                  end if
                  outside=.false.
               else

                  outside=.true.
               end if
            end do

c           Draw right meridian
c           in increments of GRID_MINOR degrees
            outside=.true.
            lon=opa2_lon+step_lon*outline
            do lat=opa2_lat,opa1_lat,-grid_minor

               call GEO_COORD
     &             (lat,lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &             geo_rng .le. rng_max) then

                  if (outside) then

                     call GRF_MOVE (plt_x,plt_y)
                  else

                     call GRF_DRAW (plt_x,plt_y)
                  end if
                  outside=.false.
               else

                  outside=.true.
               end if
            end do

c           Draw bottom latitude
c           in increments of GRID_MINOR degrees
            outside=.true.
            lat=opa1_lat+step_lat*outline
            do lon=opa2_lon,opa1_lon,grid_minor

               call GEO_COORD
     &             (lat,lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &             geo_rng .le. rng_max) then

                  if (outside) then

                     call GRF_MOVE (plt_x,plt_y)
                  else

                     call GRF_DRAW (plt_x,plt_y)
                  end if
                  outside=.false.
               else

                  outside=.true.
               end if
            end do
         end do

      CASE( 'M','R' )

c        Mercator or Rectangular

c        Check that longitudes increase properly
         if (ABS(opa2_lon-opa1_lon) .eq. 360. .or.

     &       opa1_lon .eq. opa2_lon) then

            opa2_lon=opa1_lon-360.
         else
     &   if (opa2_lon .gt. opa1_lon) then

            opa2_lon=opa2_lon-360.
         end if

c        Make sure that the op area is within the map area
         if (opa1_lat .ge. corner2_lat) go to 99
         if (opa2_lat .le. corner1_lat) go to 99

         if (opa1_lat .lt. corner1_lat) opa1_lat=corner1_lat
         if (opa2_lat .gt. corner2_lat) opa2_lat=corner2_lat

         if (opa1_lon .gt. corner1_lon .and.
     &       opa2_lon .ge. corner1_lon) then

            opa1_lon=opa1_lon-360.
            opa2_lon=opa2_lon-360.

            if (opa1_lon .le. corner2_lon) go to 99
         end if

         if (opa1_lon .le. corner2_lon .and.
     &       opa2_lon .lt. corner2_lon) then

            opa1_lon=opa1_lon+360.
            opa2_lon=opa2_lon+360.

            if (opa2_lon .ge. corner1_lon ) go to 99
         end if

c        Test if the op area is split
         if (opa1_lon .gt. corner1_lon   .or.
     &       opa2_lon .lt. corner2_lon) then

            if (opa1_lon .gt. corner1_lon) then

               if (opa1_lon-360. .lt. corner2_lon) then

                   opa1_lon=corner1_lon
                   split=.false.
               else

                   opa1_lon=opa1_lon-360.
                   split=.true.
               end if
            end if

            if (opa2_lon .lt. corner2_lon) then

               if (opa2_lon+360. .gt. corner1_lon) then

                   opa2_lon=corner2_lon
                   split=.false.
               else

                   opa2_lon=opa2_lon+360.
                   split=.true.
               end if
            end if
         else

            split=.false.
         end if

c        Draw op area
         do outline=0,4

c           Draw left edge of op area
            call GEO_COORD
     &          (opa1_lat,opa1_lon,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            call GRF_MOVE (plt_x+.01*outline,plt_y+.01*outline)

            call GEO_COORD
     &          (opa2_lat,opa1_lon,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            call GRF_DRAW (plt_x+.01*outline,plt_y-.01*outline)

c           Draw top of the op area
            if (split) then

c              Draw to the right hand edge of the mapped area
               call GEO_COORD
     &             (opa2_lat,corner2_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_DRAW (plt_x,plt_y-.01*outline)

c              Move to the left hand edge of the mapped area
               call GEO_COORD
     &             (opa2_lat,corner1_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_MOVE (plt_x,plt_y-.01*outline)

c              Draw to the right hand edge of the op area
               call GEO_COORD
     &             (opa2_lat,opa2_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_DRAW (plt_x-.01*outline,plt_y-.01*outline)
            else

c              The op area is completely enclosed by the map area
               call GEO_COORD
     &             (opa2_lat,opa2_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_DRAW (plt_x-.01*outline,plt_y-.01*outline)
            end if

c           Draw right edge of the op area
            call GEO_COORD
     &          (opa1_lat,opa2_lon,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            call GRF_DRAW (plt_x-.01*outline,plt_y+.01*outline)

c           Draw bottom latitude
            if (split) then

c              Draw to the left hand edge of the mapped area
               call GEO_COORD
     &             (opa1_lat,corner1_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_DRAW (plt_x,plt_y+.01*outline)

c              Move to the right hand edge of the mapped area
               call GEO_COORD
     &             (opa1_lat,corner2_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_MOVE (plt_x,plt_y+.01*outline)

c              Draw to the left hand edge of the op area
               call GEO_COORD
     &             (opa1_lat,opa1_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_DRAW (plt_x+.01*outline,plt_y+.01*outline)
            else

c              The op area is completely enclosed by the map area
               call GEO_COORD
     &             (opa1_lat,opa1_lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               call GRF_DRAW (plt_x+.01*outline,plt_y+.01*outline)
            end if
         end do

      END SELECT

c     Reset color
99    call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_OPA_BORDER
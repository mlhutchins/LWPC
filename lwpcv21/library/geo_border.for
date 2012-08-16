      SUBROUTINE GEO_BORDER(grid,tic_major,tic_minor)

c***********************************************************************
c                   subroutine geo_border
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     This program draws borders and grids for geographic displays.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     grid          [i] =1 for a standard latitude-longitude grid
c                       =2 for a quasi-rectangular grid over the Arctic

c     tic_major     [r] major tic mark interval
c     tic_minor     [r] minor tic mark interval

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     atan2
c     cos
c     int
c     sin

c     geo_coord
c     geo_label
c     grf_color
c     grf_draw
c     grf_move
c     grf_string
c     str_length

c  References:

c  Change History:

c     12 Mar 1997   Modified to use new string routines in GRF.

c     18 Dec 1997   Added stereographic projection;
c                   added option for a quasi-rectangular grid over
c                   the Arctic.
c 2/10/10 MLH	Fixed alignment of line continuations (&)
c               Fixed syntax errors in if statements

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      integer       grid
      real          tic_major
      real          tic_minor

      character*  8 latlbl
      character*  8 latlb2
      character*  8 lonlbl
      character*  8 lonlb2
      character* 80 pltlbl
      logical       space
      logical       outside
      integer       str_length          ! Function
      real          ang
      real          center_lat
      real          center_lon
      real          corner1_lat
      real          corner1_lon
      real          corner2_lat
      real          corner2_lon
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x
      real          geo_y
      real          grid_major
      real          grid_minor
      real          lat
      real          lon
      real          plt_x
      real          plt_x2
      real          plt_y
      real          plt_y2
      real          radius
      real          retX
      real          sav_clt0
      real          sav_cclt0
      real          sav_sclt0
      real          step
      real          tic_label
      real          tic_size
      real          tic_space
      real          tic_step

      data          dtr/.01745329252/
      data          grid_major/10./
      data          grid_minor/1./


c     Transfer input to local variable
      corner1_lat=geo_corner1_lat
      corner1_lon=geo_corner1_lon
      corner2_lat=geo_corner2_lat
      corner2_lon=geo_corner2_lon
      center _lat=geo_center _lat
      center _lon=geo_center _lon

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal, Gnomonic, Orthographic or Stereographic

         call GEO_LABEL
     &        (center_lat,center_lon,latlbl,lonlbl,-1)

         SELECT CASE( geo_prjctn )

         CASE( 'A' )

c           Azimuthal Equidistant projection
            write(pltlbl,
     &           '(''Azimuthal equidistant centered at ('',
     &             a,1x,a,'')'')')
     &             latlbl(:STR_LENGTH(latlbl)),
     &             lonlbl(:STR_LENGTH(lonlbl))

         CASE( 'G' )

c           Gnomonic projection
            write(pltlbl,
     &           '(''Gnomonic centered at ('',
     &             a,1x,a,'')'')')
     &             latlbl(:STR_LENGTH(latlbl)),
     &             lonlbl(:STR_LENGTH(lonlbl))

         CASE( 'O' )

c           Orthographic projection
            write(pltlbl,
     &           '(''Orthographic centered at ('',
     &             a,1x,a,'')'')')
     &             latlbl(:STR_LENGTH(latlbl)),
     &             lonlbl(:STR_LENGTH(lonlbl))

         CASE( 'S' )

c           Stereographic projection
            write(pltlbl,
     &           '(''Stereographic centered at ('',
     &             a,1x,a,'')'')')
     &             latlbl(:STR_LENGTH(latlbl)),
     &             lonlbl(:STR_LENGTH(lonlbl))

         END SELECT

      CASE( 'M','R' )

c        Mercator or Rectangular

         call GEO_LABEL
     &        (corner1_lat,corner1_lon,latlbl,lonlbl,-1)

         call GEO_LABEL
     &        (corner2_lat,corner2_lon,latlb2,lonlb2,-1)

         SELECT CASE( geo_prjctn )

         CASE( 'M' )

c           Mercator projection
            write(pltlbl,
     &           '(''Mercator from ('',
     &             a,1x,a,'') to ('',a,1x,a,'')'')')
     &             latlbl(:STR_LENGTH(latlbl)),
     &             lonlbl(:STR_LENGTH(lonlbl)),
     &             latlb2(:STR_LENGTH(latlb2)),
     &             lonlb2(:STR_LENGTH(lonlb2))

         CASE( 'R' )

c           Rectangular projection
            write(pltlbl,
     &           '(''Rectangular from ('',
     &             a,1x,a,'') to ('',a,1x,a,'')'')')
     &             latlbl(:STR_LENGTH(latlbl)),
     &             lonlbl(:STR_LENGTH(lonlbl)),
     &             latlb2(:STR_LENGTH(latlb2)),
     &             lonlb2(:STR_LENGTH(lonlb2))

         END SELECT

      END SELECT

c     Store current color
      call GRF_COLOR ('store')

c     Set border color
      call GRF_COLOR ('border')

c     Attach map label
      call GRF_STRING
     &     (.5*geo_size_x,-.3,.1,pltlbl,0.,'CC',retX)

c     Set up longitudinal tic labels so they don't overlap
      tic_label=tic_major

      if (tic_label*ABS(geo_scale_x) .lt. .5)
     &     tic_label=INT(.5/ABS(geo_scale_x)/tic_minor+.5)*tic_minor

c     Draw border
      call GRF_MOVE (0.,        0.        )
      call GRF_DRAW (0.,        geo_size_y)
      call GRF_DRAW (geo_size_x,geo_size_y)
      call GRF_DRAW (geo_size_x,0.        )
      call GRF_DRAW (0.,        0.        )

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal, Gnomonic, Orthographic or Stereographic

         SELECT CASE( geo_prjctn )

         CASE( 'A','O','S' )

c           Outline the globe

            SELECT CASE( geo_prjctn )

            CASE( 'A' )

               radius=geo_scale_y*20000./6366.

            CASE( 'O' )

               radius=geo_scale_y

            CASE( 'S' )

               radius=geo_scale_y*2.

            END SELECT

            plt_x=geo_origin_x
            plt_y=geo_origin_y+radius
            call GRF_MOVE (plt_x,plt_y)
            outside=.true.
            do ang=1.,360.,1.

               plt_x=geo_origin_x+radius*SIN(ang*dtr)
               plt_y=geo_origin_y+radius*COS(ang*dtr)

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &              plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

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

         END SELECT

         if (grid .gt. 0) then

c           Draw the longitude/latitude grid
            if (grid .eq. 2) then

c              Put a quasi-rectangular grid in the Arctic;
c              store the current reference point
               sav_ clt0=geo_ clt0
               sav_cclt0=geo_cclt0
               sav_sclt0=geo_sclt0
               geo_ clt0=90.*dtr
               geo_cclt0=0.
               geo_sclt0=1.
            end if

c           Change the color
            call GRF_COLOR ('grid')

c           Draw meridians
c           from 180 degrees West to 170 degrees East
c           in increments of GRID_MAJOR degrees
            do lon=180.,-170.,-grid_major

c              The latitude indicators are dashes
c              from 80 degrees North to 80 degrees South
c              in increments of GRID_MINOR degrees
               space=.false.
               outside=.true.
               do lat=80.,-80.,-grid_minor

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &                 plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &                 geo_rng .le. geo_rng_max) then

                     if (space .or. outside) then

                        call GRF_MOVE (plt_x,plt_y)
                     else

                        call GRF_DRAW (plt_x,plt_y)
                     end if
                     outside=.false.
                  else

                     outside=.true.
                  end if
                  space=.not.space
               end do
            end do

c           Draw latitudes
c           from 80 degrees North to 80 degrees South
c           in increments of GRID_MAJOR degrees
            do lat=80.,-80.,-grid_major

c              The longitude indicators are dashes
c              from 180 degrees West to 180 degrees East
c              in increments of GRID_MINOR degrees
               space=.false.
               outside=.true.
               do lon=180.,-180.,-grid_minor

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &                 plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &                 geo_rng .le. geo_rng_max) then

                     if (space .or. outside) then

                        call GRF_MOVE (plt_x,plt_y)
                     else

                        call GRF_DRAW (plt_x,plt_y)
                     end if
                     outside=.false.
                  else

                     outside=.true.
                  end if
                  space=.not.space
               end do
            end do

            if (grid .eq. 1) then

c              Label the latitudes along the central meridian
c              in TIC_MAJOR degree increments
               lon=center_lon+180.
               do lat=-INT(89./tic_major)*tic_major,
     &                  INT(89./tic_major)*tic_major,tic_major

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (plt_x .ge. 0. .and. plt_x+.3 .le. geo_size_x .and.
     &                 plt_y .ge. 0. .and. plt_y+.1 .le. geo_size_y 
     &                .and. geo_rng .le. geo_rng_max) then

                     call GEO_COORD
     &                    (lat,lon+1.,
     &                     geo_rng,geo_brng,geo_x,geo_y,
     &                     plt_x2,plt_y2)

                     ang=ATAN((plt_y-plt_y2)/(plt_x-plt_x2))

                     plt_x2=plt_x-.3*COS(ang)
                     plt_y2=plt_y-.3*SIN(ang)

                     call GEO_LABEL
     &                    (lat,lon,latlbl,lonlbl,-1)

                     call GRF_STRING
     &                    (plt_x2,plt_y2,.1,latlbl,ang/dtr,'LB',retX)
                  end if
               end do
               lon=center_lon
               do lat=INT(89./tic_major)*tic_major,
     &                -INT(89./tic_major)*tic_major,-tic_major

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (plt_x .ge. 0. .and. plt_x+.3 .le. geo_size_x .and.
     &                 plt_y .ge. 0. .and. plt_y+.1 .le. geo_size_y
     &                 .and. geo_rng .le. geo_rng_max) then

                     call GEO_COORD
     &                    (lat,lon+1.,
     &                     geo_rng,geo_brng,geo_x,geo_y,
     &                     plt_x2,plt_y2)

                     ang=ATAN((plt_y-plt_y2)/(plt_x-plt_x2))

                     plt_x2=plt_x-.3*COS(ang)
                     plt_y2=plt_y-.3*SIN(ang)

                     call GEO_LABEL
     &                    (lat,lon,latlbl,lonlbl,-1)

                     call GRF_STRING
     &                    (plt_x2,plt_y2,.1,latlbl,ang/dtr,'LB',retX)
                  end if
               end do

c              Label the longitudes
c              from 180 degrees West to 170 degrees East
c              in increments of TIC_MAJOR degrees
               lat=center_lat
               lon=center_lon
               call GEO_COORD
     &              (lat,lon,
     &               geo_rng,geo_brng,geo_x,geo_y,
     &               plt_x,plt_y)

               if (center_lat .ge. 0.) then

                  do while (plt_y .gt. .3*geo_size_y)

                     lat=lat-tic_minor
                     call GEO_COORD
     &                    (lat,lon,
     &                     geo_rng,geo_brng,geo_x,geo_y,
     &                     plt_x,plt_y)
                  end do
               else

                  do while (plt_y .lt. .7*geo_size_y)

                     lat=lat+tic_minor
                     call GEO_COORD
     &                    (lat,lon,
     &                     geo_rng,geo_brng,geo_x,geo_y,
     &                     plt_x,plt_y)
                  end do
               end if
               if (center_lat .ge. 0.) then

                  lat=lat-.5*tic_minor
               else

                  lat=lat+.5*tic_minor
               end if
               do lon=180.,-170.,-tic_label

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (plt_x .ge. 0. .and. plt_x+.3 .le. geo_size_x .and.
     &                 plt_y .ge. 0. .and. plt_y+.1 .le. geo_size_y
     &                 .and. geo_rng .le. geo_rng_max) then

                     call GEO_COORD
     &                    (lat,lon+1.,
     &                     geo_rng,geo_brng,geo_x,geo_y,
     &                     plt_x2,plt_y2)

                     ang=ATAN((plt_y-plt_y2)/(plt_x-plt_x2))

                     if (ABS(ang)/dtr .lt.  60. .or.
     &                    ABS(ang)/dtr .gt. 120.) then

                        plt_x2=plt_x-.2*COS(ang)
                        plt_y2=plt_y-.2*SIN(ang)

                        call GEO_LABEL
     &                       (lat,lon,latlbl,lonlbl,-1)

                        call GRF_STRING
     &                       (plt_x2,plt_y2,.1,lonlbl,ang/dtr,'LB',retX)
                     end if
                  end if
               end do
            else

c              Restore the original reference point
               geo_ clt0=sav_ clt0
               geo_cclt0=sav_cclt0
               geo_sclt0=sav_sclt0
            end if
         end if

      CASE( 'M','R' )

c        Mercator or Rectangular

c        Draw tic marks

c        Draw minor and major tic marks along each meridian
         tic_size=.05
         tic_step=MAX(tic_major-tic_minor,tic_minor)
         do tic_space=tic_minor,tic_major,tic_step

            lon=corner1_lon
            do step=-1.,1.,2.
               do lat=corner1_lat+tic_space,
     &                 corner2_lat-tic_minor,tic_space

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  plt_x2=plt_x-tic_size*step
                  plt_y2=plt_y

                  call GRF_MOVE (plt_x, plt_y )
                  call GRF_DRAW (plt_x2,plt_y2)
               end do
               lon=corner2_lon
            end do
            tic_size=.1
         end do

c        Draw minor and major tic marks along the top and bottom
         tic_size=.05
         tic_step=MAX(tic_label-tic_minor,tic_minor)
         do tic_space=tic_minor,tic_label,tic_step

            lat=corner2_lat
            do step=-1.,1.,2
               do lon=corner1_lon-tic_space,
     &                 corner2_lon+tic_minor,-tic_space

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  plt_x2=plt_x
                  plt_y2=plt_y+tic_size*step

                  call GRF_MOVE (plt_x, plt_y )
                  call GRF_DRAW (plt_x2,plt_y2)
               end do
               lat=corner1_lat
            end do
            tic_size=.1
         end do

c        Label major tic marks in latitude
         lon=corner1_lon
         do lat=corner1_lat,corner2_lat,tic_major

            call GEO_COORD
     &           (lat,lon,
     &            geo_rng,geo_brng,geo_x,geo_y,
     &            plt_x,plt_y)

            call GEO_LABEL
     &           (lat,lon,latlbl,lonlbl,-1)

            call GRF_STRING
     &           (plt_x-.05,plt_y,.1,latlbl,0.,'RC',retX)
         end do
         if (lat .ne. corner2_lat) then

c           Add label for top
            lat=corner2_lat
            call GEO_COORD
     &           (lat,lon,
     &            geo_rng,geo_brng,geo_x,geo_y,
     &            plt_x,plt_y)

            call GEO_LABEL
     &           (lat,lon,latlbl,lonlbl,-1)

            call GRF_STRING
     &           (plt_x-.05,plt_y,.1,latlbl,0.,'RC',retX)
         end if

c        Label major tic marks in longitude
         lat=corner1_lat
         do lon=corner1_lon,corner2_lon,-tic_label

            call GEO_COORD
     &           (lat,lon,
     &            geo_rng,geo_brng,geo_x,geo_y,
     &            plt_x,plt_y)

            call GEO_LABEL
     &           (lat,lon,latlbl,lonlbl,-1)

            call GRF_STRING
     &           (plt_x,plt_y-.15,.1,lonlbl,0.,'CC',retX)
         end do
         if (lon .ne. corner2_lon) then

c           Add label for right edge
            lon=corner2_lon
            call GEO_COORD
     &           (lat,lon,
     &            geo_rng,geo_brng,geo_x,geo_y,
     &            plt_x,plt_y)

            call GEO_LABEL
     &           (lat,lon,latlbl,lonlbl,-1)

            call GRF_STRING
     &           (plt_x,plt_y-.15,.1,lonlbl,0.,'CC',retX)
         end if

         if (grid .gt. 0) then

c           Draw the longitude/latitude grid

c           Change the color
            call GRF_COLOR ('grid')

c           Draw meridians
c           in increments of GRID_MAJOR degrees
            do lon=corner1_lon-grid_major,
     &              corner2_lon+grid_minor,-grid_major

c              The latitude indicators are dashes
c              in increments of GRID_MINOR degrees
               space=.true.
               do lat=corner1_lat,corner2_lat,grid_minor

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (space) then

                     call GRF_MOVE (plt_x,plt_y)
                  else

                     call GRF_DRAW (plt_x,plt_y)
                  end if
                  space=.not.space
               end do
            end do

c           Draw latitudes
c           in increments of GRID_MAJOR degrees
            do lat=corner1_lat+grid_major,
     &              corner2_lat-grid_minor,grid_major

c              The longitude indicators are dashes
c              in increments of GRID_MINOR degrees
               space=.true.
               do lon=corner1_lon,corner2_lon,-grid_minor

                  call GEO_COORD
     &                 (lat,lon,
     &                  geo_rng,geo_brng,geo_x,geo_y,
     &                  plt_x,plt_y)

                  if (space) then

                     call GRF_MOVE (plt_x,plt_y)
                  else

                     call GRF_DRAW (plt_x,plt_y)
                  end if
                  space=.not.space
               end do
            end do
         end if

      END SELECT

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_BORDER
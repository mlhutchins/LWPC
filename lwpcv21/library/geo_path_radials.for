      SUBROUTINE GEO_PATH_RADIALS
     &          (Tx_id,Tx_lat,Tx_lon,
     &           path_color,path_label,
     &           path_length,bflag,
     &           mxpath,nrpath,
     &           path_bear,path_rng,
     &           Rx_lat,Rx_lon,Rx_rng,
     &           opa_lat,opa_lon)

c***********************************************************************
c                   subroutine geo_path_radials
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     Graph great circle radials from a selected geographical point.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Input sign convention is + for N/W and - for S/E.

c  Parameters passed:
c     Tx_id         [s   ] character string for Tx id
c     Tx_lat        [r   ] transmitter latitude
c     Tx_lon        [r   ] transmitter longitude
c     path_color    [s, 3] colors (1-radial; 2-1MM inc; 3-5MM inc)
c     path_label    [l   ] draw header label (T-do; F-don't)
c     path_length   [r   ] path length for all paths
c     bflag         [i   ] defines how the paths are defined
c                          =0: use the op area
c                          =1: use the bearing angles
c                          =2: use the receiver coordinates

c     mxpath        [i   ] dimension of PATH_BEAR and Rx_LAT/LON
c     nrpath        [i   ] number of entries in PATH_BEAR or Rx_LAT/LON
c     path_bear     [r, *] array of path bearings
c     path_rng      [r, *] array of path ranges
c     Rx_lat        [r, *] array of receiver latitudes
c     Rx_lon        [r, *] array of receiver longitudes
c     Rx_rng        [r, *] array of receiver ranges
c     opa_lat       [r, 2] op area  latitide  (bottom,top)
c     opa_lon       [r, 2] op area  longitude (left,right)

c     NOTE: If bflag=2, then the bearings to the receivers are put into
c           the array PATH_BEAR and PATH_RNG is the maximum distance to
c           the receivers.

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     cos
c     int
c     sin

c     geo_coord
c     geo_label
c     grf_color
c     grf_draw
c     grf_move
c     grf_number
c     grf_string
c     grf_symbol
c     str_length

c  References:

c  Change History:

c     12 Mar 1997   Modified to use new string routines in GRF.

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      character*(*) path_color(3)
      character*(*) Tx_id
      logical       path_label
      integer       nrpath
      integer       mxpath
      real          opa_lat(2)
      real          opa_lon(2)
      real          path_bear(mxpath)
      real          path_rng (mxpath)
      real          path_length
      real          Rx_lat   (mxpath)
      real          Rx_lon   (mxpath)
      real          Rx_rng   (mxpath)
      real          Tx_lat
      real          Tx_lon

      character*  8 latlbl
      character*  8 lonlbl
      character* 80 pltlbl
      logical       first
      logical       inside
      integer       bflag
      integer       dst
      integer       dst_max
      integer       nbr
      integer       str_length          ! Function
      integer       symbol(3)
      real          br
      real          brsltn(2)
      real          crclt
      real          ctclt
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x
      real          geo_y
      real          lat
      real          lon
      real          plt_x
      real          plt_x2
      real          plt_y
      real          plt_y2
      real          rclt
      real          retX
      real          rlng
      real          rng_max
      real          srclt
      real          stclt
      real          symbol_size(3)
      real          tclt
      real          tlng

      data          brsltn/15.,3./
      data          dtr/.01745329252/
      data          symbol/3,1,14/
      data          symbol_size/.08,.08,.05/


c     Store current color
      call GRF_COLOR ('store')

c     Set the color
      call GRF_COLOR (path_color(1))

c     Label the plot with Tx data if required
      if (path_label) then

         call GEO_LABEL (Tx_lat,Tx_lon,latlbl,lonlbl,1)

         write(pltlbl,
     &       '(''Paths centered on '',a,
     &         '' ('',a,'', '',a,'')'')')
     &         Tx_id (:STR_LENGTH(Tx_id )),
     &         latlbl(:STR_LENGTH(latlbl)),
     &         lonlbl(:STR_LENGTH(lonlbl))

         call GRF_STRING
     &       (.5*geo_size_x,-.75,.1,pltlbl,0.,'CC',retx)
      end if

c     Label path origin
      lat=Tx_lat
      lon=Tx_lon

      call GEO_COORD
     &    (lat,lon,
     &     geo_rng,geo_brng,geo_x,geo_y,
     &     plt_x,plt_y)

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

         if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &       plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &       geo_rng .le. geo_rng_max) then

            inside=.true.
         else

            inside=.false.
         end if

      CASE( 'M','R' )

         if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &       plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

            inside=.true.
         else

            inside=.false.
         end if

      END SELECT

      if (inside) then

         call GRF_SYMBOL
     &       (plt_x,plt_y,symbol_size(2),symbol(1),0.,-1)
      end if

c     Determine bearing angles
      rng_max=path_length

      call LWP_SET_BRNG
     &    (Tx_lat,Tx_lon,bflag,opa_lat,opa_lon,brsltn,
     &     mxpath,nrpath,path_bear,path_rng,
     &     Rx_lat,Rx_lon,Rx_rng,rng_max)

c     Plot the radials
      tlng=Tx_lon*dtr
      tclt=(90.-Tx_lat)*dtr
      ctclt=COS(tclt)
      stclt=SIN(tclt)

      do nbr=1,nrpath

         br=path_bear(nbr)
         dst_max=path_rng(nbr)

c        Ensure easily understood value on the plot
         br=MOD(br,360.)

c        Plot the path
         first=.true.

c        Set the color
         call GRF_COLOR (path_color(1))

         do dst=500,dst_max,50

            call RECVR2
     &          (tlng,tclt,ctclt,stclt,br*dtr,dst/6366.,
     &           rlng,rclt,crclt,srclt)

            lat=90.-rclt/dtr
            lon=rlng/dtr

            call GEO_COORD
     &          (lat,lon,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            SELECT CASE( geo_prjctn )

            CASE( 'A','G','O','S' )

               if (dst .eq. 500) then

                  inside=.false.
               else
     &         if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &             geo_rng .le. geo_rng_max) then

                  inside=.true.
               else

                  inside=.false.
               end if

            CASE( 'M','R' )

               if (dst .eq. 500) then

                  inside=.false.
               else
     &         if (ABS(plt_x-plt_x2) .gt. .5*geo_size_x .or.
     &             ABS(plt_y-plt_y2) .gt. .5*geo_size_y) then

                  inside=.false.
               else
     &         if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

                  inside=.true.
               else

                  inside=.false.
               end if
               plt_x2=plt_x
               plt_y2=plt_y

            END SELECT

            if (inside) then

               if (first) then

                  call GRF_MOVE (plt_x,plt_y)
                  first=.false.
               else

                  call GRF_DRAW (plt_x,plt_y)
               end if
            else

               call GRF_MOVE (plt_x,plt_y)
            end if
         end do

c        Put a symbol at each megameter
         do dst=1000,dst_max,1000

            call RECVR2
     &          (tlng,tclt,ctclt,stclt,br*dtr,dst/6366.,
     &           rlng,rclt,crclt,srclt)

            lat=90.-rclt/dtr
            lon=rlng/dtr

            call GEO_COORD
     &          (lat,lon,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            SELECT CASE( geo_prjctn )

            CASE( 'A','G','O','S' )

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &             geo_rng .le. geo_rng_max) then

                  inside=.true.
               else

                  inside=.false.
               end if

            CASE( 'M','R' )

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x-.1 .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y-.1) then

                   inside=.true.
               else

                  inside=.false.
               end if

            END SELECT

            if (inside) then

               if (MOD(dst,5000) .ne. 0) then

c                 Symbol at 1000 megameter distance
                  call GRF_COLOR (path_color(2))

                  call GRF_SYMBOL
     &                (plt_x,plt_y,symbol_size(2),symbol(2),0.,-1)
               else

c                 Symbol at 5000 megameter distance
                  call GRF_COLOR (path_color(3))

                  call GRF_SYMBOL
     &                (plt_x,plt_y,symbol_size(1),symbol(3),0.,-1)

                  if (MOD(br,30.) .ne. 0.) then

                     write(pltlbl,'(i2.2)') dst/1000
                  else

                     write(pltlbl,'(i2.2,''/'',i3.3)') dst/1000,INT(br)
                  end if

                  if (plt_x .le. geo_size_x-.08*STR_LENGTH(pltlbl)) then

c                    Label the symbol
                     call GRF_STRING
     &                   (plt_x+.05,plt_y,.1,pltlbl,0.,'LC',retX)
                  end if
               end if
            end if
         end do
      end do

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_PATH_RADIALS
      SUBROUTINE PLT_AREA_SIZE
     &          (mxlat,nrlat,mxlon,nrlon,
     &           ampl,xlat,xlon,
     &           value,area_a,area_o)

c Returns area enclosed by a specific signal level

c     mxlon         maximum number of points in longitude
c     mxlat         maximum number of points in latitude
c     nrlon         output  number of points in longitude
c     nrlat         output  number of points in latitude
c     ampl          array of signal levels
c     xlat(1)       latitude  of lower left  corner
c     xlon(1)       longitude of lower left  corner
c     xlat(2)       latitude  of upper right corner
c     xlon(2)       longitude of upper right corner
c     value         signal level
c     area_a        percentage of the total area enclosed by
c                   the signal level
c     area_o        percentage of the ocean area enclosed by
c                   the signal level

      include      'lwpc_geo.cmn'

      logical       inside,land

      dimension     ampl(mxlon,mxlat),xlat(2),xlon(2)


c     Determine the interval in latitude
      dlat=(xlat(2)-xlat(1))/(nrlat-1)

c     Determine the interval in longitude
      if (xlon(1) .gt. xlon(2)) then
         dlon=(xlon(1)-xlon(2))/(nrlon-1)
      else
         dlon=(xlon(1)-xlon(2)+360.)/(nrlon-1)
      end if

      ocean=0.
      total=0.
      area_a=0.
      area_o=0.

      xlt=xlat(1)
      do l=1,nrlat

         cos_lat=cos(xlt*.01745329252)
         xln=xlon(1)
         do k=1,nrlon

c           Find out where this point will be plotted
            call GEO_COORD
     &          (xlt,xln,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            SELECT CASE( geo_prjctn )

            CASE( 'A','G','O','S' )

c              Azimuthal Equidistant, Gnomonic, Orthographic or Stereographic

               if (plt_x   .ge.         0.  .and.
     &             plt_x   .le. geo_size_x  .and.
     &             plt_y   .ge.         0.  .and.
     &             plt_y   .le. geo_size_y  .and.
     &             geo_rng .le. geo_rng_max) then

                  inside=.true.
               else

                  inside=.false.
               end if

            CASE( 'M','R' )

c              Mercator or Rectangular

               if (plt_x   .ge.         0.  .and.
     &             plt_x   .le. geo_size_x  .and.
     &             plt_y   .ge.         0.  .and.
     &             plt_y   .le. geo_size_y)  then

                  inside=.true.
               else

                  inside=.false.
               end if

            END SELECT

            if (inside) then

c              This point is inside the mapped area
               call LAND_SEA (xln,xlt,land)

               total=total+cos_lat
               if (.not.land) ocean=ocean+cos_lat

               if (ampl(k,l) .ge. value) then

                  area_a=area_a+cos_lat
                  if (.not.land) area_o=area_o+cos_lat
               end if
            end if
            xln=xln-dlon
            if (xln .lt. -180.) xln=xln+360.
         end do
         xlt=xlt+dlat
      end do

      if (total .gt. 0.) area_a=100.*area_a/total
      if (ocean .gt. 0.) area_o=100.*area_o/ocean

      RETURN
      END      ! PLT_AREA_SIZE
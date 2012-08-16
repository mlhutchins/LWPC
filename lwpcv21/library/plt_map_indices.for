      SUBROUTINE PLT_MAP_INDICES
     &          (op_lat,op_lon,delta_lat,delta_lon,map_lat,map_lon,
     &           index_lat,index_lon)

c***********************************************************************
c                         subroutine plt_map_indices
c***********************************************************************

c  Program Source:   Naval Ocean Systems Center - Code 542

c  Date:
c     05 Nov 1990

c  Function:
c     Determines the indices corresponding to the boundaries of a map
c     area with respect to the specified op area. The map area MUST be
c     a subset of the op area.

c  Parameters passed:
c     op_lat         [r,2] latitude  bounds of the op  area
c     op_lon         [r,2] longitude bounds of the op  area
c     delta_lat      [r  ] latitude  resolution of the op area grid
c     delta_lon      [r  ] longitude resolution of the op area grid

c     map_lat        [r,2] latitude  bounds of the map area
c     map_lon        [r,2] longitude bounds of the map area

c  Parameters returned:
c     index_lat      [i,3] latitude indices; the first value indicates
c                          the number of the branch within this routine
c                          used to set the grid indices; a value less
c                          than one indicates a map area which is not
c                          enclosed by the op area; the other two values
c                          are the indices of the latitudinal bounds of
c                          the map area with respect to the op area

c     index_lon      [i,5] longitude indices; the first value indicates
c                          the number of the branch within this routine
c                          used to set the grid indices; a value less
c                          than one indicates a map area which is not
c                          enclosed by the op area; the second and third
c                          values are the indices of the longitudinal
c                          bounds of the map area with respect to the
c                          op area; the second pair of values are non-
c                          zero if the op area goes all around the globe
c                          and the map area straddles its boundary

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     int

c  References:

c  Change History:
c     21 Feb 1996    Ensure that the longitude boundaries are +/- 180.

c********************!**************************************************

cxxcDEBUG
cxx      include      'lwpc_lun.cmn'

      logical        inside
      integer        branch
      real           map_lat(2),map_lon(2)
      dimension      op_lat(2),op_lon(2),
     &               index_lat(3),index_lon(5)

cxxcDEBUG
cxx      write(lwpcLOG_lun,*) 'plt_map_indices'
cxx      write(lwpcLOG_lun,*) 'lat ',op_lat,delta_lat
cxx      write(lwpcLOG_lun,*) 'lon ',op_lon,delta_lon

c     Initialize the latitudinal output
      branch=0
      j1=0
      j2=0

      xlat1=op_lat(1)
      xlat2=op_lat(2)
      xlt1=map_lat(1)
      xlt2=map_lat(2)

      if (xlt1 .ge. xlat1 .and. xlt2 .le. xlat2) then

c        The map area is enclosed by the op area.
         branch=1

         j1=INT((xlt1-xlat1)/delta_lat)+1
         j2=INT((xlt2-xlat1)/delta_lat)+1
      end if

      index_lat(1)=branch
      index_lat(2)=j1
      index_lat(3)=j2

c     Initialize the longitudinal output
      branch=0
      j1=0
      j2=0
      k1=0
      k2=0

      xlon1=op_lon(1)
      xlon2=op_lon(2)
      xln1=map_lon(1)
      xln2=map_lon(2)

c     Ensure that the longitude boundaries are +/- 180
      if (xlon2 .lt. -180.) xlon2=xlon2+360.
      if (xln2  .lt. -180.) xln2 =xln2 +360.

      if (xlon1 .eq. xlon2 .or. ABS(xlon1-xlon2) .eq. 360.) then

         nrlon=INT(360./delta_lon)+1
      else
     &if (xlon1 .gt. xlon2) then

         nrlon=(xlon1-xlon2)/delta_lon+1.
      else

         nrlon=(xlon1-xlon2+360.)/delta_lon+1.
      end if

      if (xlon1 .eq. xlon2 .or. ABS(xlon1-xlon2) .eq. 360.) then

c        The op area is a 360 strip; all map areas will fit.
         if (xln1 .eq. xln2 .or. ABS(xln1-xln2) .eq. 360.) then

c           The map area is also a 360 strip.
            if (xln1 .eq. xlon1) then

c              The map area coincides with the op area.
               branch=1

               j1=1
               j2=nrlon
            else

c              The boundary of the map area is offset from the op area.

c              Determine the distances from the map area boundaries to
c              the op area boundary.
               dxln=xln1-xlon1
               if (dxln .gt.  180.) then

                  dxln=360.-dxln
               else
     &         if (dxln .lt. -180.) then

                  dxln=360.+dxln
               end if

               if (dxln .ge. 0. .and. dxln .le. 180.) then

c                 The map area starts west of the op area.
                  branch=2

                  j1=nrlon-INT(dxln/delta_lon)
                  j2=nrlon
                  k1=2
                  k2=j1
               else

c                 The map area starts east of the op area.
                  branch=3

                  j1=-INT(dxln/delta_lon)+1
                  j2=nrlon
                  k1=2
                  k2=j1
               end if
            end if
         else

c           The map area is not a 360 strip.

c           Determine the distances from the map area boundaries to
c           the op area boundary.
            if (xln1 .lt. 0. .and. xln2 .gt. 0.) then

c             The date line is included in the map area
              if (xlon1 .lt. 0.) then

                 dxln1=xln1-xlon1
                 dxln2=xln2-xlon1-360
              else

                 dxln1=xln1-xlon1+360.
                 dxln2=xln2-xlon1
              end if
            else

c             The date line is not included in the map area
              dxln1=xln1-xlon1
              dxln2=xln2-xlon1
            end if
            if (dxln1 .gt.  180.) then

               dxln1=dxln1-360.
            else
     &      if (dxln1 .lt. -180.) then

               dxln1=dxln1+360.
            end if
            if (dxln2 .gt.  180.) then

               dxln2=dxln2-360.
            else
     &      if (dxln2 .lt. -180.) then

               dxln2=dxln2+360.
            end if

            if (dxln1 .gt. 0. .and. dxln1 .le.  180. .and.
     &          dxln2 .ge. 0. .and. dxln2 .le.  180.) then

c              The map area boundaries are west of the op area.
               branch=4

               j1=nrlon-INT(dxln1/delta_lon)
               j2=nrlon-INT(dxln2/delta_lon)
            else
     &      if (dxln1 .le. 0. .and. dxln1 .ge. -180. .and.
     &          dxln2 .lt. 0. .and. dxln2 .ge. -180.) then

c              The map area boundaries are east of the op area.
               branch=5

               j1=-INT(dxln1/delta_lon)+1
               j2=-INT(dxln2/delta_lon)+1
            else
     &      if (dxln1 .gt. 0. .and. dxln2 .lt. 0.) then

c              The op area boundary is inside the map area.
               branch=6

               j1=nrlon-INT(dxln1/delta_lon)
               j2=nrlon
               k1=2
               k2=-INT(dxln2/delta_lon)+1
            else

c              The op area boundary is 180 degrees away.
               branch=7

               j1=-INT(dxln1/delta_lon)+1
               j2=nrlon-INT(dxln2/delta_lon)

            end if
         end if
      else

c        The op area is not a strip.

c        Find the midpoint of the map area.
         if (xln1 .gt. xln2) then

            xln0=.5*(xln1+xln2)
         else

            xln0=.5*(xln1+xln2+360.)
         end if
         if (xln0 .gt.  180.) then

            xln0=xln0-360.
         else
     &   if (xln0 .lt. -180.) then

            xln0=xln0+360.
         end if

c        Now determine if the midpoint is inside the op area. This is
c        the only way a map area can be a valid subset of the op area.
         if (xlon1 .gt. xlon2) then

            if (xlon1 .ge. xln0 .and. xln0 .ge. xlon2) then

               inside=.true.
            else

               inside=.false.
            end if
         else

            if (xlon1 .ge. xln0 .or. xln0 .ge. xlon2) then

               inside=.true.
            else

               inside=.false.
            end if
         end if

         if (inside) then

            if (xlon1 .gt. xlon2) then

c              The op area does not cross the 180 meridian.
               if (xln1 .le. xlon1 .and. xln2 .ge. xlon2) then

c                 The map area is bounded by the op area.
                  branch=8

                  j1=-INT((xln1-xlon1)/delta_lon)+1
                  j2=-INT((xln2-xlon1)/delta_lon)+1
               end if
            else

c              The op area spans the 180 meridian.
               if (xln1 .gt. 0. .and. xln2 .ge. 0.) then

c                 The map area is entirely in the western hemisphere.
                  branch=9

                  j1=nrlon-INT((xln1-xlon2)/delta_lon)
                  j2=nrlon-INT((xln2-xlon2)/delta_lon)
               else
     &         if (xln1 .le. 0. .and. xln2 .lt. 0.) then

c                 The map area is entirely in the eastern hemisphere.
                  branch=10

                  j1=-INT((xln1-xlon1)/delta_lon)+1
                  j2=-INT((xln2-xlon1)/delta_lon)+1
               else

c                 The map area also spans the 180 meridian.
                  branch=11

                  j1=     -INT((xln1-xlon1)/delta_lon)+1
                  j2=nrlon-INT((xln2-xlon2)/delta_lon)
               end if
            end if
         end if
      end if

      index_lon(1)=branch
      index_lon(2)=j1
      index_lon(3)=j2
      index_lon(4)=k1
      index_lon(5)=k2

cxxcDEBUG
cxx      write(lwpcLOG_lun,*) 'lat ',map_lat,index_lat
cxx      write(lwpcLOG_lun,*) 'lon ',map_lon,index_lon

      RETURN
      END      ! PLT_MAP_INDICES
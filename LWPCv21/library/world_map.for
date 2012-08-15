      SUBROUTINE WORLD_MAP
     &          (map_type,grid,tics)

c***********************************************************************
c                   subroutine world_map
c***********************************************************************

c  Program Source: Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Plots a map of the world.

c     Input sign convention is + for N/W and - for S/E

c  Parameters passed:
c     map_type      [s  ] land/sea map type;
c                         ='    ':  no map
c                         ='land':  use COAST$D.DAT
c                         ='coas':  use COAST$D.DAT
c                         ='cond':  use COND$D.DAT

c     grid          [i] =1 for a standard latitude-longitude grid
c                       =2 for a quasi-rectangular grid over the Arctic

c     tics          [r,2] grid interval and labelled interval; deg

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     geo_border
c     geo_coast_map
c     geo_cond_map
c     str_upper

c  References:

c  Change History:
c     31 Jul 1991   Rearranged calculation of scaling parameters to
c                   prevent division by zero when map latitudes or
c                   longitudes are the same but the projection is not
c                   rectangular, mercator or gnomonic.

c     03 Aug 1994   Added orthographic projection.

c     20 Aug 1994   Changed to use new geophysics routines.

c     12 Jul 1994   Changed definitions of map centers for azim and
c                   ortho; added BORDER_COLOR, COAST_COLOR, LAND_COLOR,
c                   GRID, TICS, GRID_COLOR and OPA_COLOR.

c     04 Sep 1994   Dropped RANGE_MAX from argument list because it is
c                   already encoded in MAP_LAT(2); plot coast map last.

c     04 Mar 1995   Changed to new geophysics routines; get colors from
c                   GRAPHICS.INI file.

c     06 Apr 1996   Revised to use new coast map routine.

c     18 Dec 1997   Modified to use new GEO_BORDER option for a quasi-
c                   rectangular grid over the Arctic.

c*******************!***************************************************

c     LWPC map data
      include      'lwpc_geo.cmn'

      character*(*) map_type(2)
      character*  4 type
      integer       grid

      integer       map_index(2)
      data          map_index/1001,7000/

      dimension     tics(2)


c     Establish some working values
      tic_minor=tics(1)
      tic_major=tics(2)

c     Loop over the map types
      do n=1,2

         type=map_type(n)
         call STR_UPPER (type,0,0)

         if (type .eq. 'COND') call GEO_COND_MAP

         if (type .eq. 'LAND') call GEO_COAST_MAP (type,map_index)

         if (type .eq. 'COAS') call GEO_COAST_MAP (type,map_index)
      end do

      call GEO_BORDER (grid,tic_major,tic_minor)

      RETURN
      END      ! WORLD_MAP
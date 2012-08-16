      SUBROUTINE GEO_QOVAL_MAP
     &          (oval_label,month,day,year,UT,effq)

c***********************************************************************
c                   subroutine geo_qoval_map
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     Produces a map of the auroral oval.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c  Parameters passed:
c     oval_label    [l] place oval parameters above map
c     month         [c] name of the month
c     day           [i] day number in the month
c     year          [i] year
c     UT            [r] UT in hours
c     effq          [r] auroral q index

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     cos
c     int
c     mod
c     sin

c     geo_coord
c     geo_qoval
c     grf_color
c     grf_draw
c     grf_move
c     grf_string

c  References:

c  Change History:

c     12 Mar 1997   Modified to use new string routines in GRF.

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      character*(*) month
      logical       oval_label
      integer       day
      integer       year
      real          effq
      real          UT

      character* 80 pltlbl
      logical       first
      logical       inside
      integer       npt
      integer       nrpts
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
      real          retX

      integer       mxpts

      parameter    (mxpts=97)

      real          pb(mxpts)
      real          pl(mxpts)
      real          eb(mxpts)
      real          el(mxpts)


c     Get the oval boundaries.
      call GEO_QOVAL
     &    (effq,month,day,year,UT,mxpts,nrpts,pb,pl,eb,el)

c     Store current color
      call GRF_COLOR ('store')

c     Set the color
      call GRF_COLOR ('qoval')

c     Label the plot if required
      if (oval_label) then

         write(pltlbl,
     &       '(''Q oval: Q='',f4.1,1x,a3,2(''/'',i2.2),
     &         '':'',i4.4,''UT'')')
     &         effq,
     &         month,day,MOD(year,100),INT(INT(UT)*40.+UT*60.)

         call GRF_STRING
     &       (.5*geo_size_x,-.6,.1,pltlbl,0.,'CC',retX)
      end if

c     Draw the poleward boundary
      first=.true.
      do npt=1,nrpts

         lat=pb(npt)
         lon=pl(npt)

         call GEO_COORD
     &       (lat,lon,
     &        geo_rng,geo_brng,geo_x,geo_y,
     &        plt_x,plt_y)

         SELECT CASE( geo_prjctn )

         CASE( 'A','G','O','S' )

            if (npt .eq. 1) then

               inside=.false.
            else
     &      if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &          plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &          geo_rng .le. geo_rng_max) then

               inside=.true.
            else

               inside=.false.
            end if

         CASE( 'M','R' )

            if (npt .eq. 1) then

               inside=.false.
            else
     &      if (ABS(plt_x-plt_x2) .gt. .5*geo_size_x .or.
     &          ABS(plt_y-plt_y2) .gt. .5*geo_size_y) then

               inside=.false.
            else
     &      if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &          plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

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

c     Draw the equatorward boundary
      first=.true.
      do npt=1,nrpts

         lat=eb(npt)
         lon=el(npt)

         call GEO_COORD
     &       (lat,lon,
     &        geo_rng,geo_brng,geo_x,geo_y,
     &        plt_x,plt_y)

         SELECT CASE( geo_prjctn )

         CASE( 'A','G','O','S' )

            if (npt .eq. 1) then

               inside=.false.
            else
     &      if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &          plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &          geo_rng .le. geo_rng_max) then

               inside=.true.
            else

               inside=.false.
            end if

         CASE( 'M','R' )

            if (npt .eq. 1) then

               inside=.false.
            else
     &      if (ABS(plt_x-plt_x2) .gt. .5*geo_size_x .or.
     &          ABS(plt_y-plt_y2) .gt. .5*geo_size_y) then

               inside=.false.
            else
     &      if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &          plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

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

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_QOVAL_MAP
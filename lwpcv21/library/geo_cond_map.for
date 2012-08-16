      SUBROUTINE GEO_COND_MAP

c***********************************************************************
c                   subroutine geo_cond_map
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     10 May 1996

c  Function:
c     This program provides graphical display of the VLF ground
c     conductivity map.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     The colors are set as follows:
c        green   3e-2/1e-2
c        blue    3e-3/1e-3
c        purple  3e-5
c        red     1e-5
c        black   3e-4
c        yellow  1e-4

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:

c  Parameters returned:

c  Common blocks referenced:
c     geo$data
c     grf$cntr
c     lwpc_lun

c  Functions and subroutines referenced:
c     abs

c     geo_coord
c     grf_cntr_fill
c     grf_color
c     grf_limits
c     open_dat
c     recvr2

c     Requires COND$F.DAT

c  References:

c  Change History:

c     18 Dec 1997   Added stereographic projection.

c     10 Oct 1999   Better test for the antipode inserted.

c*******************!***************************************************

      IMPLICIT NONE

c     LWPC parameters
      include      'lwpc_geo.cmn'
      include      'lwpc_lun.cmn'
      include      'grf_cntr.cmn'

c     Conductivity map data
      integer       mxcoord
      parameter    (mxcoord=4000)

      integer  *  2 cond_lat(mxcoord)
      integer  *  2 cond_lon(mxcoord)
      integer  *  2 ndx
      integer  *  2 nrp

      character*  8 color_list(6)
      character* 40 file_name
      logical       first
      integer       k
      integer       n
      integer       m
      integer       mcode
      integer       mxpass
      integer       nrcoord
      integer       nrpass
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
cxxcDEBUG
cxx      real          xp(mxcoord)
cxx      real          yp(mxcoord)

      data          first/.true./
      data          file_name/'cond$f.dat'/

      data          color_list/'red','purple','yellow','black',
     &                         'blue','green'/

      data          dtr/.01745329252/
      data          pi/3.14159265359/


      if (first) then

c        Open the data file:
         call OPEN_DAT (lwpcGROUND_lun,file_name,'unformatted')

         first=.false.
      end if

c     Store current color
      call GRF_COLOR ('store')

      nrholes=0

21    read (lwpcGROUND_lun,end=29)
     &      ndx,nrp,
     &     (cond_lon(n),cond_lat(n),n=1,nrp)

      mcode=ndx/1000

      nrcoord=nrp

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal, Gnomonic, Orthographic or Stereographic

c        Check if this area is inside of mapped area
         k=0
         n=0
         latx=-999.
         lonx=-999.
         do m=1,nrcoord

            lat=cond_lat(m)/2.
            lon=cond_lon(m)/2.

            if (ABS(lat-latx) .gt. .5 .or.
     &          ABS(lon-lonx) .gt. .5) then

               call GEO_COORD
     &             (lat,lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               k=k+1
               if (geo_rng .lt. geo_rng_max) n=n+1

               latx=lat
               lonx=lon
            end if
         end do
         if (n .gt. 0) then

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

                  lat=cond_lat(m)/2.
                  lon=cond_lon(m)/2.

                  call GEO_COORD
     &                (lat,lon,
     &                 geo_rng,geo_brng,geo_x,geo_y,
     &                 plt_x,plt_y)
cxxcDEBUG
cxx                  write(lwpcLOG_lun,
cxx     &                '(i5,2f7.1,f6.1,f8.1,2f6.1)')
cxx     &                  m,lat,lon,
cxx     &                  geo_brng*57.296,geo_rng*6366.,
cxx     &                  plt_x,plt_y
cxxcDEBUG
cxx                  xp(m)=plt_x
cxx                  yp(m)=plt_y

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
cxxcDEBUG
cxx                        write(lwpcLOG_lun,
cxx     &                      '(i5,2f7.1,f6.1,f8.1,2f6.0,'' ant'')'
cxx     &                        m,lat,lon,
cxx     &                        geo_brng*57.296,geo_rng*6366.,
cxx     &                        geo_center_lat,geo_center_lon

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
cxxcDEBUG
cxx                     write(lwpcLOG_lun,
cxx     &                   '(i5,2f7.1,f6.1,f8.1,2f6.1,'' mod'')')
cxx     &                     m,lat,lon,
cxx     &                     geo_brng*57.296,geo_rng*6366.,
cxx     &                     plt_x,plt_y
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
cxxcDEBUG
cxx            call GRF_COLOR
cxx     &          ('red')
cxx            call GRF_CURVE
cxx     &          (xp,yp,nrcoord,0.,0.,1.,1.,1,20)

            if (nrcntrp .gt. 0) then

               nrpts(0)=nrcntrp

c              Ensure that all points are inside the boundary
               call GRF_LIMITS
     &             (mxholes,nrholes,nrpts,mxcntrp,cntrpx,cntrpy,
     &              geo_size_x,geo_size_y)

               call GRF_CNTR_FILL
     &             (0.,1.,0.,1.,'100%',color_list(mcode))
            end if
         end if

      CASE( 'M','R' )

c        Mercator or Rectangular

c        Set up plot resolution
         tst_scale=.0001

         nrpass=0
         mxpass=1
         do while (nrpass .lt. mxpass)

            m=1
            prv_x=99.
            prv_y=99.
            nrcntrp=0
            do while (m .lt. nrcoord)

               lat=cond_lat(m)/2.
               lon=cond_lon(m)/2.

               call GEO_COORD
     &             (lat,lon,
     &              geo_rng,geo_brng,geo_x,geo_y,
     &              plt_x,plt_y)

               if (nrpass .eq. 0) then

                  if (lon .gt. geo_corner1_lon) then

c                    This line extends to the left of the map;
c                    set up a second pass
                     mxpass=2
                     plt_x=0.
                  else
     &            if (lon .lt. geo_corner2_lon) then

c                    This line extends to the right of the map;
c                    set up a second pass
                     mxpass=2
                     plt_x=geo_size_x
                  end if
               else

c                 This is the second pass;
c                 exclude what has already been drawn
                  if (lon .ge. geo_corner1_lon) then

                     plt_x=0.
                  else
     &            if (lon .le. geo_corner2_lon) then

                     plt_x=geo_size_x
                  end if
               end if

               if (plt_y .lt.         0.) then

                  plt_y=0.
               else
     &         if (plt_y .gt. geo_size_y) then

                  plt_y=geo_size_y
               end if

               if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &             plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

                  if (nrcntrp .eq. 0 .or.
     &               ((plt_x-prv_x)**2
     &               +(plt_y-prv_y)**2 .ge. tst_scale)) then

                     if (nrcntrp .eq. mxcntrp-3) then

c                       Current plot resolution is too fine;
c                       restart with coarser value
                        tst_scale=tst_scale*5.
                        m=nrcoord
                        nrcntrp=-1
                     else

c                       Store this point
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

               nrpts(0)=nrcntrp

               call GRF_CNTR_FILL
     &             (0.,1.,0.,1.,'100%',color_list(mcode))
            end if

            if (nrcntrp .ge. 0) then

               nrpass=nrpass+1
               if (nrpass .lt. mxpass) then

c                 Shift this line segment to the left and
c                 do another pass
                  do m=1,nrcoord
                     cond_lon(m)=cond_lon(m)-720
                  end do
               end if
            end if
         end do

      END SELECT
      go to 21
29    REWIND (lwpcGROUND_lun)

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_COND_MAP
      SUBROUTINE GEO_DIP_MAP
     &          (mxdip,nrdip,dip_list,
     &           dip_max,dip_min,dip_inc,
     &           dip_line)

c***********************************************************************
c                   subroutine geo_dip_map
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     Magnetic dip in a geographic display.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Input sign convention is + for N/W and - for S/E.

c  Parameters passed:
c     mxdip         [i  ] dimension of dip_list
c     nrdip         [i  ] number    of dip_list
c                         =0, use dip_max, dip_min and dip_inc
c     dip_list      [r,*] list of solar zenith angles (degrees)
c     dip_max       [r  ] maximum   contour (degrees)
c                         =-99, use max value found in the map
c     dip_min       [r  ] minimum   contour (degrees)
c                         =-99, use min value found in the map
c     dip_inc       [r  ] increment contour (degrees)

c     dip_line      [i  ] line type to use:
c                         =0, do filled contours
c                         <0, do automatic sequence
c                         >0, do specified line type

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     int
c     max
c     min
c     mod

c     geo_cntr
c     grf_color
c     newmag

c  References:

c  Change History:

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      integer       dip_line
      integer       mxdip
      integer       nrdip
      real          dip_inc
      real          dip_list(mxdip)
      real          dip_max
      real          dip_min

      character*  8 fill
      character* 12 color
      integer       line
      integer       mxnx
      integer       mxny
      integer       nrnv
      integer       nrnx
      integer       nrny
      integer       nv
      integer       nx
      integer       ny
      real          b
      real          bmf
      real          bp
      real          br
      real          bt
      real          dip_mx
      real          dip_mn
      real          dp
      real          dlat
      real          dlon
      real          dtr
      real          lat
      real          lon
      real          vc1
      real          vc2

      parameter    (mxnx=73,
     &              mxny=37)

      real          dip(mxnx,mxny)
      real          grd_lat(2)
      real          grd_lon(2)
      real          vcntr(2)

      data          dlat/5./
      data          dlon/5./
      data          dtr/.01745329252/
      data          color/'dipmap'/


c     Store current color
      call GRF_COLOR ('store')

      SELECT CASE( geo_prjctn )

      CASE( 'A','G','O','S' )

c        Azimuthal Equidistant, Gnomonic, Orthographic or Stereographic
         grd_lat(1)= -90.
         grd_lat(2)=  90.
         grd_lon(1)= 180.
         grd_lon(2)=-180.

      CASE( 'M','R' )

c        Mercator or Rectangular
         grd_lat(1)=geo_corner1_lat
         grd_lat(2)=geo_corner2_lat
         grd_lon(1)=geo_corner1_lon
         grd_lon(2)=geo_corner2_lon

      END SELECT

c     Set dip range
      dip_mx=-99.
      dip_mn= 99.

c     Set grid boundaries
      lon=grd_lon(2)
      if (lon .ge. grd_lon(1)) lon=lon-360.

c     Generate an array of dip values
      nrnx=(grd_lon(1)-    lon   )/dlon+1.
      nrny=(grd_lat(2)-grd_lat(1))/dlat+1.

c     Set initial latitude
      lat=grd_lat(1)

c     Loop on latitudes
      do ny=1,nrny

c        Set initial longitude
         lon=grd_lon(1)

c        Loop on longitudes
         do nx=1,nrnx

            if (lon .lt. -180.) lon=lon+360.

            call NEWMAG
     &          (1,0.,lon*dtr,(90.-lat)*dtr,bmf,dp,b,br,bp,bt)

            dip(nx,ny)=dp/dtr

            dip_mx=MAX(dip_mx,dip(nx,ny))
            dip_mn=MIN(dip_mn,dip(nx,ny))

            lon=lon-dlon
         end do

         lat=lat+dlat
      end do

c     Set contour parameters
      if (nrdip .eq. 0) then

c        Force range to be whole increments of the specified increment
         dip_mx=INT(dip_mx/dip_inc+.99)*dip_inc
         dip_mn=INT(dip_mn/dip_inc-.99)*dip_inc

         if (dip_max .ne. -99.) dip_mx=dip_max
         if (dip_min .ne. -99.) dip_mn=dip_min

         nrnv=(dip_mx-dip_mn)/dip_inc+.99
      else

         nrnv=nrdip-1
      end if

      line=dip_line

      do nv=1,nrnv

         if (nrdip .eq. 0) then

            vc1=dip_mx-dip_inc*(nv-1)
            vc2=dip_mx-dip_inc* nv
         else

            vc1=dip_list(nv  )
            vc2=dip_list(nv+1)
         end if

         if (dip_line .eq. 0) then

            vcntr(1)=vc1
            vcntr(2)=vc2
         else

            vcntr(1)=vc1
            vcntr(2)=-99.
         end if

         if (dip_line .eq. 0) then

c           Use fill types from GRAPHICS.INI
            write(fill,'(''fill'',i2.2)') MOD(nv,2)+9
         else
     &   if (dip_line .lt. 0) then

c           Automatic line types
            if (vc1 .eq. 0.) then

               line=4
            else
     &      if (MOD(ABS(vc1),10.) .eq. 0.) then

               line=1
            else

               line=3
            end if
         end if

         call GEO_CNTR
     &       (dip,mxnx,mxny,nrnx,nrny,
     &        grd_lat,grd_lon,
     &        vcntr,line,fill,color)
      end do

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_DIP_MAP
      SUBROUTINE GEO_CHI_MAP
     &          (month,day,year,UT,
     &           mxchi,nrchi,chi_list,
     &           chi_max,chi_min,chi_inc,
     &           chi_label,chi_line)

c***********************************************************************
c                   subroutine geo_chi_map
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Aug 1995

c  Function:
c     Draw contours of constant solar zenith angle.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     month         [s  ] month name
c     day           [i  ] day of the month
c     year          [i  ] year
c     UT            [r  ] Universal Time in hours
c     mxchi         [i  ] dimension of chi_list
c     nrchi         [i  ] number    of chi_list
c                         =0, use CHI_MAX, CHI_MIN and CHI_INC
c     chi_list      [r,*] list of solar zenith angles (degrees)
c     chi_max       [r  ] maximum   contour (degrees)
c                         =-99, use max value found in the map
c     chi_min       [r  ] minimum   contour (degrees)
c                         =-99, use min value found in the map
c     chi_inc       [r  ] increment contour (degrees)
c     chi_label     [l  ] generate label (T-do; F-don't)
c     chi_line      [i  ] line type to use:
c                         =0, do filled contours
c                         <0, do automatic sequence
c                         >0, do specified line type

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     cos
c     float
c     max
c     min
c     mod
c     sin

c     almnac
c     gcdbr2
c     geo_cntr
c     geo_label
c     grf_color
c     grf_string
c     hrs_to_hhmm
c     month_number
c     str_length

c  References:

c  Change History:

c     12 Mar 1997   Modified to use new string routines in GRF.

c     18 Dec 1997   Added stereographic projection.

c*******************!***************************************************

      IMPLICIT NONE

c     Map parameters
      include      'lwpc_geo.cmn'

      character*(*) month
      logical       chi_label
      integer       chi_line
      integer       day
      integer       mxchi
      integer       nrchi
      integer       year
      real          chi_inc
      real          chi_list(mxchi)
      real          chi_max
      real          chi_min
      real          UT

      character*  8 fill
      character*  8 latlbl
      character*  8 lonlbl
      character* 12 color
      character* 80 pltlbl
      integer       line
      integer       mnth
      integer       mxnx
      integer       mxny
      integer       nrnv
      integer       nrnx
      integer       nrny
      integer       nv
      integer       nx
      integer       ny
      integer       str_length          ! Function
      real          br
      real          chi_mx
      real          chi_mn
      real          crclt
      real          csclt
      real          dlat
      real          dlon
      real          dtr
      real          gcd
      real          hrs_to_hhmm         ! Function
      real          lat
      real          lon
      real          rclt
      real          sclt
      real          rlng
      real          slng
      real          srclt
      real          ssclt
      real          ssp_lat
      real          ssp_lon
      real          vc1
      real          vc2
      real          retX

      parameter    (mxnx=73,
     &              mxny=37)

      real          chi(mxnx,mxny)
      real          grd_lat(2)
      real          grd_lon(2)
      real          vcntr(2)

      data          dlat/5./
      data          dlon/5./
      data          dtr/.01745329252/
      data          color/'terminator'/


c     Store current color
      call GRF_COLOR ('store')

c     Determine location of sub-solar point
      call MONTH_NUMBER (month,mnth)
      call ALMNAC (year,mnth,day,UT,sclt,slng)
      ssclt=SIN(sclt)
      csclt=COS(sclt)

      ssp_lat=90.-sclt/dtr
      ssp_lon=slng/dtr

c     Label the plot if required
      if (chi_label) then

         call GEO_LABEL (ssp_lat,ssp_lon,latlbl,lonlbl,1)

         write(pltlbl,
     &       '(''SOLAR: '',a3,''/'',i2.2,''/'',i2.2,
     &         '':'',i4.4,''UT'',
     &         '' SSP: ('',a,1x,a,'')'')')
     &         month,day,MOD(year,100),INT(HRS_TO_HHMM(UT)),
     &         latlbl(:STR_LENGTH(latlbl)),
     &         lonlbl(:STR_LENGTH(lonlbl))

         call GRF_STRING
     &       (.5*geo_size_x,-.45,.1,pltlbl,0.,'CC',retX)
      end if

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

c     Set chi range
      chi_mx=-99.
      chi_mn= 99.

c     Set grid boundaries
      lon=grd_lon(2)
      if (lon .ge. grd_lon(1)) lon=lon-360.

c     Generate an array of chi values
      nrnx=(grd_lon(1)-    lon   )/dlon+1.
      nrny=(grd_lat(2)-grd_lat(1))/dlat+1.

c     Set initial latitude
      lat=grd_lat(1)

c     Loop on latitudes
      do ny=1,nrny

         rclt=(90.-lat)*dtr
         crclt=COS(rclt)
         srclt=SIN(rclt)

c        Set initial longitude
         lon=grd_lon(1)

c        Loop on longitudes
         do nx=1,nrnx

            if (lon .lt. -180.) lon=lon+360.

            rlng=lon*dtr

            call GCDBR2
     &          (slng-rlng,sclt,csclt,ssclt,rclt,crclt,srclt,gcd,br,0)

            chi(nx,ny)=gcd/dtr

            chi_mx=MAX(chi_mx,chi(nx,ny))
            chi_mn=MIN(chi_mn,chi(nx,ny))

            lon=lon-dlon
         end do

         lat=lat+dlat
      end do

c     Set contour parameters
      if (nrchi .eq. 0) then

c        Force range to be whole increments of the specified increment
         chi_mx=INT(chi_mx/chi_inc+.99)*chi_inc
         chi_mn=INT(chi_mn/chi_inc-.99)*chi_inc

         if (chi_max .ne. -99.) chi_mx=chi_max
         if (chi_min .ne. -99.) chi_mn=chi_min

         nrnv=(chi_mx-chi_mn)/chi_inc+.99
      else

         nrnv=nrchi-1
      end if

      line=chi_line

      do nv=1,nrnv

         if (nrchi .eq. 0) then

            vc1=chi_mx-chi_inc*(nv-1)
            vc2=chi_mx-chi_inc* nv
         else

            vc1=chi_list(nv  )
            vc2=chi_list(nv+1)
         end if

         if (chi_line .eq. 0) then

            vcntr(1)=vc1
            vcntr(2)=vc2
         else

            vcntr(1)=vc1
            vcntr(2)=-99.
         end if

         if (chi_line .eq. 0) then

c           Use fill types from GRAPHICS.INI
            write(fill,'(''fill'',i2.2)') MOD(nv,2)+11
         else
     &   if (chi_line .lt. 0) then

c           Automatic line types
            line=MOD(nv,5)
            if (line .eq. 0) line=5
         end if

         call GEO_CNTR
     &       (chi,mxnx,mxny,nrnx,nrny,
     &        grd_lat,grd_lon,
     &        vcntr,line,fill,color)
      end do

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_CHI_MAP
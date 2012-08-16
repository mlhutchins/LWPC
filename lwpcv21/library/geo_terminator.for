      SUBROUTINE GEO_TERMINATOR
     &          (term_label,term_line,sub_solar,
     &           month,day,year,UT,chi_list)

c***********************************************************************
c                   subroutine geo_terminator
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     Draw lines of constant solar zenith angle.

c     Projections:  azimuthal equidistant
c                   gnomonic
c                   mercator
c                   orthographic
c                   rectangular
c                   stereographic

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     term_label    [l  ] draw terminator header label (T-do; F-don't
c     term_line     [i  ] line type (>0 - used; <=0 - ignored)
c     sub_solar     [l  ] draw sub_solar point (T - do; F - don't)
c     month         [s  ] month name
c     day           [i  ] day of the month
c     year          [i  ] year
c     UT            [r  ] Universal Time in hours
c     chi_list      [r,2] solar zenith angles for terminator

c  Parameters returned:

c  Common blocks referenced:
c     geo$data

c  Functions and subroutines referenced:
c     abs
c     cos
c     int
c     max
c     min
c     mod
c     sin

c     almnac
c     geo_coord
c     geo_label
c     grf_color
c     grf_curve
c     grf_draw
c     grf_move
c     grf_string
c     grf_symbol
c     hrs_to_hhmm
c     month_number
c     recvr2
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
      integer       day
      integer       term_line
      integer       year
      logical       sub_solar
      logical       term_label
      real          chi_list(2)
      real          UT

      character*  8 latlbl
      character*  8 lonlbl
      character* 80 pltlbl
      logical       inside
      logical       loop
      integer       j1
      integer       j2
      integer       line
      integer       mnth
      integer       np
      integer       nrpts
      integer       nrseg
      integer       nrup
      integer       symbol(2)
      integer       str_length          ! Function
      real          bearing
      real          bearing_inc
      real          chi
      real          chi_inc
      real          chi_max
      real          chi_min
      real          crclt
      real          csclt
      real          dtr
      real          geo_brng
      real          geo_rng
      real          geo_x
      real          geo_y
      real          hrs_to_hhmm         ! Function
      real          lat
      real          lon
      real          plt_x
      real          plt_x2
      real          plt_y
      real          plt_y2
      real          rclt
      real          sclt
      real          retX
      real          rlng
      real          slng
      real          srclt
      real          ssclt
      real          ssp_lat
      real          ssp_lon

      parameter    (nrpts=721,nrseg=5)

      logical       up(nrpts)
      real          xp(nrpts)
      real          yp(nrpts)

      data          dtr/.01745329252/
      data          symbol/1,11/


c     Transfer input to local variable

c     Determine location of sub-solar point
      call MONTH_NUMBER (month,mnth)
      call ALMNAC (year,mnth,day,UT,sclt,slng)
      ssclt=SIN(sclt)
      csclt=COS(sclt)

      ssp_lat=90.-sclt/dtr
      ssp_lon=slng/dtr

c     Store current color
      call GRF_COLOR ('store')

c     Set the color
      call GRF_COLOR ('terminator')

c     Label the plot if required
      if (term_label) then

         call GEO_LABEL (ssp_lat,ssp_lon,latlbl,lonlbl,1)

         write(pltlbl,
     &       '(''SOLAR: '',a3,''/'',i2.2,''/'',i2.2,
     &         '':'',i4.4,''UT  Chi '',f4.1,'','',f5.1,
     &         '' SSP: ('',a,1x,a,'')'')')
     &         month,day,MOD(year,100),INT(HRS_TO_HHMM(UT)),
     &         chi_list,
     &         latlbl(:STR_LENGTH(latlbl)),
     &         lonlbl(:STR_LENGTH(lonlbl))

         call GRF_STRING
     &       (.5*geo_size_x,-.45,.1,pltlbl,0.,'CC',retX)
      end if

c     Plot the subsolar point if required
      if (sub_solar) then

         call GEO_COORD
     &       (ssp_lat,ssp_lon,
     &        geo_rng,geo_brng,geo_x,geo_y,
     &        plt_x,plt_y)

         SELECT CASE( geo_prjctn )

         CASE( 'A','G','O','S' )

            if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &          plt_y .ge. 0. .and. plt_y .le. geo_size_y .and.
     &          geo_rng .le. geo_rng_max) then

               inside=.true.
            else

               inside=.false.
            end if

         CASE( 'M','R' )

            if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .or.
     &          plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

               inside=.true.
            else

               inside=.false.
            end if

         END SELECT

         if (inside) then

            call GRF_SYMBOL (plt_x,plt_y,.1,symbol(1),0.,-1)
            call GRF_SYMBOL (plt_x,plt_y,.2,symbol(2),0.,-1)
         end if
      end if

c     Get the increment of the solar zenith angle needed to get the
c     required number of lines between the input range.
      chi_max=MAX(chi_list(1),chi_list(2))
      chi_min=MIN(chi_list(1),chi_list(2))
      chi_inc=(chi_max-chi_min)/nrseg

c     Increment in bearing angle to get the requested number of points
      bearing_inc=360./(nrpts-1)

c     Set the line type
      if (term_line .gt. 0) then

c        Use specified line type
         line=term_line
      else

c        Use automatic line sequence
         line=0
      end if

c     Loop over solar zenith angles
      do chi=chi_min,chi_max,chi_inc

         np=0
         nrup=0
         do bearing=0.,360.,bearing_inc

c           Get coordinates of the point which is at a distance CHI
c           from the subsolar point at the specified bearing angle
            call RECVR2 (slng,sclt,csclt,ssclt,bearing*dtr,chi*dtr,
     &                   rlng,rclt,crclt,srclt)

            lat=90.-rclt/dtr
            lon=rlng/dtr

            call GEO_COORD
     &          (lat,lon,
     &           geo_rng,geo_brng,geo_x,geo_y,
     &           plt_x,plt_y)

            SELECT CASE( geo_prjctn )

            CASE( 'A','G','O','S' )

               if (np .eq. 1) then

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

               if (np .eq. 1) then

                  inside=.false.
               else
     &         if (ABS(plt_x-plt_x2) .gt. .5*geo_size_x .or.
     &                  ABS(plt_y-plt_y2) .gt. .5*geo_size_y) then

                  inside=.false.
               else
     &         if (plt_x .ge. 0. .and. plt_x .le. geo_size_x .and.
     &                  plt_y .ge. 0. .and. plt_y .le. geo_size_y) then

                  inside=.true.
               else

                  inside=.false.
               end if
               plt_x2=plt_x
               plt_y2=plt_y

            END SELECT

            np=np+1
            xp(np)=plt_x
            yp(np)=plt_y

            if (inside) then

               up(np)=.false.
            else

               up(np)=.true.
               nrup=nrup+1
            end if
         end do

c        Now plot the curve in segments; segments are those parts
c        of the curve which are visible on the map.

c        Set the line type
         if (term_line .eq. 0) then

            line=line+1
            if (line .eq. nrseg+1) line=5
         end if

         if (nrup .lt. nrpts) then

            j2=1
            do while (j2 .le. nrpts)

c              Find the first visible point on the curve
               j1=j2
               loop=.true.
               do while (loop)

                  if (up(j1)) then

                     if (j1 .eq. nrpts) then

                        loop=.false.
                     else

                        j1=j1+1
                     end if
                  else

                     loop=.false.
                  end if
               end do

               if (nrpts-j1 .gt. 1) then

c                 Find the last visible point
                  j2=j1+1
                  loop=.true.
                  do while (loop)

                     if (up(j2)) then

                        loop=.false.
                     else

                        if (j2 .eq. nrpts) then

                           loop=.false.
                        else

                           j2=j2+1
                        end if
                     end if
                  end do

                  call GRF_CURVE
     &                (xp(j1),yp(j1),j2-j1,0.,0.,1.,1.,line,20)
               else

c                 Terminate loop
                  j2=nrpts+1
               end if
            end do
         end if
      end do

c     Reset color
      call GRF_COLOR ('reset')

      RETURN
      END      ! GEO_TERMINATOR
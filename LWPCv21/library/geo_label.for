      SUBROUTINE GEO_LABEL
     &          (lat,lon,lat_lbl,lon_lbl,nrdec)

c***********************************************************************
c                   subroutine geo_label
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     01 Mar 1995

c  Function:
c     This program writes strings for coordinate labels for the
c     geographic displays.

c     Latitudes and longitudes in degrees
c     Sign convention is + for N/E and - for S/W

c  Parameters passed:
c     lat           [r] latitude
c     lon           [r] longitude
c     lat_lbl       [c] label for latitude
c     lon_lbl       [c] label for longitude
c     nrdec         [i] number of decimal places output
c                       Only the integer part if <0

c  Parameters returned:

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     abs
c     int
c     mod

c  References:

c  Change History:

c*******************!***************************************************

      IMPLICIT NONE

      character*(*) lat_lbl
      character*(*) lon_lbl
      integer       nrdec
      real          lat
      real          lon

      character* 12 frmt


      if (nrdec .lt. 0) then

         write(frmt,'(''(i2,a)'')')
         if (lat .ge. 0.) then

            write(lat_lbl,frmt) INT(ABS(lat)),'N'
         else

            write(lat_lbl,frmt) INT(ABS(lat)),'S'
         end if

         write(frmt,'(''(i3,a)'')')
         if (lon .gt. 0. .and. MOD(lon,360.) .lt. 180.) then

            write(lon_lbl,frmt) INT(MOD(lon,360.)),'W'
         else
     &   if (lon .lt. 0. .and. MOD(lon,360.) .lt. -180.) then

            write(lon_lbl,frmt) INT(360.+MOD(lon,360.)),'W'
         else
     &   if (lon .le. 0. .and. MOD(lon,360.) .ge. -180.) then

            write(lon_lbl,frmt) INT(ABS(MOD(lon,360.))),'E'
         else

            write(lon_lbl,frmt) INT(360.-ABS(MOD(lon,360.))),'E'
         end if
      else

         if (nrdec .lt. 7) then

            write(frmt,'(''(f'',i1,''.'',i1'',a)'')') 3+nrdec,nrdec
         else

            write(frmt,'(''(f'',i2,''.'',i1'',a)'')') 3+nrdec,nrdec
         end if

         if (lat .ge. 0.) then

            write(lat_lbl,frmt) ABS(lat),'N'
         else

            write(lat_lbl,frmt) ABS(lat),'S'
         end if
         if (nrdec .lt. 6) then

            write(frmt,'(''(f'',i1,''.'',i1'',a)'')') 4+nrdec,nrdec
         else

            write(frmt,'(''(f'',i2,''.'',i1'',a)'')') 4+nrdec,nrdec
         end if
         if (lon .gt. 0. .and. MOD(lon,360.) .lt. 180.) then

            write(lon_lbl,frmt) MOD(lon,360.),'W'
         else
     &   if (lon .lt. 0. .and. MOD(lon,360.) .lt. -180.) then

            write(lon_lbl,frmt) 360.+MOD(lon,360.),'W'
         else
     &   if (lon .le. 0. .and. MOD(lon,360.) .ge. -180.) then

            write(lon_lbl,frmt) ABS(MOD(lon,360.)),'E'
         else

            write(lon_lbl,frmt) 360.-ABS(MOD(lon,360.)),'E'
         end if
      end if

      RETURN
      END      ! GEO_LABEL
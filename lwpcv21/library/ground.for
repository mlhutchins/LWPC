      SUBROUTINE GROUND
     &          (xlon,xlat,ncode,sigma,epsr)

c***********************************************************************
c                   subroutine ground
c***********************************************************************

c  Program Source:  Naval Ocean Systems Center - Code 542

c  Date:
c     05 Apr 1990

c  Function:
c     Returns ground conductivity data

c  Parameters passed:
c     xlon          [r] West longitude of the point in degrees
c     xlat          [r] North latitude of the point in degrees

c  Parameters returned:
c     ncode         [i] conductivity code
c     sigma         [r] gound conductivity in Seimens
c     epsr          [r] ground dielectric constant

c     NOTES: A list of SIGMA and EPSR is also placed into common GRND$:
c            ncode  sigma  epsr
c              1    1.e-5    5
c              2    3.e-5    5
c              3    1.e-4   10
c              4    3.e-4   10
c              5    1.e-3   15
c              6    3.e-3   15
c              7    1.e-2   15
c              8    3.e-2   15
c              9    1.e-1   15
c             10    4.      81

c  Common blocks referenced:
c     grnd$data

c  Functions and subroutines referenced:
c     abs
c     close

c     open_dat

c     Requires COND$D.DAT

c  References:

c  Change History:
c     26 Jan 1993   Changed to use OPEN_DAT.

c     21 Oct 1995   Changed to get the logical unit from LWPC_LUN.CMN.

c 2/26/2010 MLH Stored conductivty values in grnd$data and read from there each run

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

c     List of ground conductivities
      common/grnd$data/
     &              sss(10),rrr(10),lcode2(361),map2(4505)

c     Conductivity map data
      integer       lcode(361)
      integer       map(4505)

      character*200 file_name,error_msg !,full_name

      logical       first

      dimension     ss(10),rr(10)

      data          file_name/'cond$d.dat'/,
     &              first/.true./,

     &              ss/1.e-5,3.e-5,1.e-4,3.e-4,1.e-3,
     &                 3.e-3,1.e-2,3.e-2,1.e-1,4./,
     &              rr/5.,   5.,   10.,  10.,  15.,
     &                 15.,  15.,  15.,  15.,  81./

      if (first) then

         call OPEN_DAT (lwpcGROUND_lun,file_name,'formatted')

c        Read the data:
         read (lwpcGROUND_lun,'(9i8)') lcode,map
         CLOSE(lwpcGROUND_lun)
       
c        Put the conductivity values in common /GRND$/
         do l=1,10
            sss(l)=ss(l)
            rrr(l)=rr(l)
         end do
         
         do l=1,361
            lcode2(l)=lcode(l)
         
         end do
         
         do l=1,4505
            map2(l)=map(l)
         end do
         
         first=.false.
      end if

c     Read map and lcode values to prevent data corruption
      map=map2
      lcode=lcode2

      phi=xlon
      if (phi .gt. 180.) then
         phi=phi-360.
      else
         if (phi .lt. -180.) phi=phi+360.
      end if

      if (ABS(phi) .gt. 180. .or. ABS(xlat) .gt. 90.01) then
         write(error_msg,
     &       '(''[GROUND]: '',
     &         ''Xlon Xlat'',2f9.2)') xlon,xlat
         call LWPC_ERROR('ERROR',error_msg)
      end if

c     Find the latitude strip
      lat=181.-2.*xlat
      if (lat .lt.   1) then
         lat=  1
      end if
      if (lat .gt. 360) then
         lat=360
      end if

      lon=361.-2.*phi
      if (lon .lt. 1 .or. lon .gt. 720) then
         lon=1
      end if

      l1=lcode(lat)
      l2=lcode(lat+1)-1

      do l=l1,l2

         m1=map(l)/10000
         m2=map(l)-10000*m1
         mlon=m2/10

         if (mlon .ge. lon) go to 31
      end do
31    ncd=m2-mlon*10
      mlon=m1/10
      if (mlon .ge. lon) then
         ncd=m1-mlon*10
      end if

      if (ncd .lt. 0 .or. ncd .gt. 9) then

         write(error_msg,
     &       '(''[GROUND]: '',
     &         ''Xlon Xlat lon lat l1 l2'',
     &         ''mlon ncode'',2f9.2,4i6,2i8)')
     &         xlon,xlat,lon,lat,l1,l2,mlon,ncd
         call LWPC_ERROR('ERROR',error_msg)
      end if

      if (ncd .eq. 0) then
         ncode=10
      else
         ncode=ncd
      end if
      sigma=ss(ncode)
      epsr= rr(ncode)

      RETURN
      END      ! GROUND
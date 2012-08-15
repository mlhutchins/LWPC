      SUBROUTINE GEO_CGM
     &          (to_cgm, geo_lat,geo_lon, cgm_lat,cgm_lon)

c***********************************************************************
c                   subroutine geo_cgm
c***********************************************************************

c  Program Source:
c     Naval Ocean Systems Center - Code 542

c  Date:
c     20 Jul 1991

c  Function:
c     Routine to convert to/from:
c        geographic coordinates/corrected geomagnetic coordinates

c  Parameters passed:
c     to_cgm        [l] T:  geographic to corrected geomagnetic
c                       F:  corrected geomagnetic to geographic

c     If TO_CGM is .TRUE.:
c     geo_lat       [r] geographic latitude; degrees North
c     geo_lon       [r] geographic longitude; degrees West

c     If TO_CGM is .FALSE.:
c     cgm_lat       [r] corrected geomagnetic latitude; degrees North
c     cgm_lon       [r] corrected geomagnetic longitude; degrees West

c  Parameters returned:
c     If TO_CGM is .TRUE.:
c     cgm_lat       [r] corrected geomagnetic latitude; degrees North
c     cgm_lon       [r] corrected geomagnetic longitude; degrees West

c     If TO_CGM is .FALSE.:
c     geo_lat       [r] geographic latitude; degrees North
c     geo_lon       [r] geographic longitude; degrees West

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     close
c     mod

c     cgm_to_geo
c     geo_to_cgm
c     open_dat

c  Entry:

c  References:

c  Change History:
c     21 Oct 1995   Changed to get the logical unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      character*245 file_name
      logical       first,to_cgm

      dimension     cg(2,36,89)

      data          file_name/'cglalo.dat'/,
     &              first/.true./


      if (first) then
         call OPEN_DAT (lwpcCGM_lun,file_name,'unformatted')
         READ (lwpcCGM_lun) cg
         CLOSE(lwpcCGM_lun)
         first=.false.
      end if

      if (to_cgm) then

c        Convert GEO_LON to East longitude.
         gglon=MOD(geo_lon,360.)
         if (gglon .gt. 0.) then
            gglon=360.-gglon
         else
            gglon=-gglon
         end if

c        Convert from geographic to corrected geomagnetic
         call GEO_TO_CGM (cg, geo_lat,gglon, cgm_lat,cgm_lon)

c        Convert CGM_LON to West longitude.
         cgm_lon=MOD(cgm_lon,360.)
         if (cgm_lon .gt. 0.) then
            cgm_lon=-cgm_lon
            if (cgm_lon .lt. -180.) cgm_lon=cgm_lon+360.
         else
            cgm_lon=360.-cgm_lon
            if (cgm_lon .gt. 180.) cgm_lon=cgm_lon-360.
         end if

      else

c        Convert CGM_LON to East longitude.
         cglon=MOD(cgm_lon,360.)
         if (cglon .gt. 0.) then
            cglon=360.-cglon
         else
            cglon=-cglon
         end if

c        Convert from corrected geomagnetic to geographic
         call CGM_TO_GEO (cg, cgm_lat,cglon, geo_lat,geo_lon)

c        Convert GEO_LON to West longitude.
         geo_lon=MOD(geo_lon,360.)
         if (geo_lon .gt. 0.) then
            geo_lon=-geo_lon
            if (geo_lon .lt. -180.) geo_lon=geo_lon+360.
         else
            geo_lon=360.-geo_lon
            if (geo_lon .gt. 180.) geo_lon=geo_lon-360.
         end if
      end if

      RETURN
      END      ! GEO_CGM
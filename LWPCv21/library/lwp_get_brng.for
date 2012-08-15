      SUBROUTINE LWP_GET_BRNG
     &          (tlat,tlon,op_lat,op_lon,rbear,rhom)

c***********************************************************************
c                  subroutine lwp_get_brng
c***********************************************************************

c  Program Source: Naval Ocean Systems Center - Code 542

c  Date:
c     13 Aug 1991

c  Function:
c     Returns minimum and maximum bearing and minimum and maximum
c     range for paths which span an operating area.

c  Parameters passed:
c     tlat           [r   ] latitude  of the transmitter; deg North
c     tlon           [r   ] longitude of the transmitter; deg West
c     op_lat         [r, 2] latitude  bounds of the area; deg North
c     op_lon         [r, 2] longitude bounds of the area; deg West
c     rbear          [r, 3] RBEAR(3): bearing resolution; deg

c  Parameters returned:
c     rbear          [r, 3] RBEAR(1): minimum bearing; deg E of N
c                           RBEAR(2): maximum bearing; deg E of N
c     rhom           [r, 2] range parameters
c                           RHOM(1): minimum range; km
c                           RHOM(2): maximum range; km

c  Common blocks referenced:

c  Functions and subroutines referenced:

c  References:

c  Change History:

c*******************!***************************************************

      logical       flagb,flagr,flaga

      dimension     op_lat(2),op_lon(2),rbear(3),rhom(2)

      data          dtr/.0174532925219943/,rtd/57.2957795131/,
     &              pi/3.1415926535898/,twopi/6.2831853071796/


c     Transmitter parameters
      tlng=tlon*dtr
      tclt=(90.-tlat)*dtr
      ctclt=COS(tclt)
      stclt=SIN(tclt)

c     Corners of op area
      xlon1=op_lon(1)
      xlon2=op_lon(2)
      xlng1=xlon1*dtr
      xlng2=xlon2*dtr
      if (xlng2 .ge. xlng1) xlng2=xlng2-twopi

      xlat1=op_lat(1)
      xclt1=(90.-xlat1)*dtr
      cxclt1=COS(xclt1)
      sxclt1=SIN(xclt1)
      xlat2=op_lat(2)
      xclt2=(90.-xlat2)*dtr
      cxclt2=COS(xclt2)
      sxclt2=SIN(xclt2)

c     Check for polar operating areas
      if (xlat2 .eq. 90. .and.
     &   (ABS(xlon1-xlon2) .eq. 360. .or. xlon1-xlon2 .eq. 0.)) then

c        Northern polar op area
         if (tlat .gt. xlat1) then

c           Transmitter is inside op area
            rbear(1)=0.
            rbear(2)=360.-rbear(3)
            rhom(1)=0.
            rhom(2)=MIN(20000.,1000.*AINT((tclt+xclt1)*6.366+.9))
         else

c           Transmitter outside op area
            args=sxclt1/stclt
            if (args .gt. 1.) then

c              Antipode is in operating area
               rbear(1)=0.
               rbear(2)=360.-rbear(3)
               rhom(1)=MIN(20000.,1000.*AINT((tclt-xclt1)*6.366))
               rhom(2)=20000.
            else

c              Get tangent bearings
               angl=AINT(ASIN(args)*rtd/rbear(3)+.9)*rbear(3)
               bmin=360.-angl
               bmax=360.+angl
               rbear(1)=AINT(bmin/rbear(3)   )*rbear(3)
               rbear(2)=AINT(bmax/rbear(3)+.9)*rbear(3)
               if (rbear(1) .eq. 0. .and. rbear(2) .eq. 360.)
     &             rbear(2)=rbear(2)-rbear(3)
               rhom(1)=MIN(20000.,1000.*AINT((tclt-xclt1)*6.366))
               rhom(2)=MIN(20000.,1000.*AINT((tclt+xclt1)*6.366+.9))
            end if
         end if
         RETURN
      else
     &   if (xlat1 .eq. -90. .and.
     &      (ABS(xlon1-xlon2) .eq. 360. .or. xlon1-xlon2 .eq. 0.)) then

c        Southern polar op area
         if (tlat .lt. xlat2) then

c           Transmitter is inside op area
            rbear(1)=0.
            rbear(2)=360.-rbear(3)
            rhom(1)=0.
            rhom(2)=MIN(20000.,
     &                  40000.-1000.*AINT((xclt2+tclt)*6.366+.9))
         else

c           Transmitter outside op area
            args=sxclt2/stclt
            if (args .gt. 1.) then
c              Antipode is in operating area
               rbear(1)=0.
               rbear(2)=360.-rbear(3)
               rhom(1)=MIN(20000.,1000.*AINT((xclt2-tclt)*6.366))
               rhom(2)=20000.
            else

c              Get tangent bearings
               angl=AINT(ASIN(args)*rtd/rbear(3)+.9)*rbear(3)
               bmin=180.-angl
               bmax=180.+angl
               rbear(1)=AINT(bmin/rbear(3)   )*rbear(3)
               rbear(2)=AINT(bmax/rbear(3)+.9)*rbear(3)
               if (rbear(1) .eq. 0. .and. rbear(2) .eq. 360.)
     &            rbear(2)=rbear(2)-rbear(3)
               rhom(1)=MIN(20000.,1000.*AINT((xclt2-tclt)*6.366))
               rhom(2)=MIN(20000.,
     &                     40000.-1000.*AINT((xclt2+tclt)*6.366+.9))
            end if
         end if
         RETURN
      end if

c     Non-polar operating area
      flagr=.false.
      flaga=.false.

      if (ABS(xlon1-xlon2) .eq. 360. .or. xlon1-xlon2 .eq. 0. .or.
     &   (xlon1 .gt. xlon2 .and.
     &   (tlon  .le. xlon1 .and. tlon .ge. xlon2))  .or.
     &   (xlon1 .lt. xlon2 .and.
     &   (tlon  .le. xlon1 .or.  tlon .ge. xlon2))) then

c        Transmitter is bounded by operating area meridians
         flagr=.true.
         if (tclt .lt. xclt1 .and. tclt .gt. xclt2) then

c           Transmitter is inside of op area
            rbear(1)=0.
            rbear(2)=360.-rbear(3)

c           Get ranges to corners of the op area
            dl=tlng-xlng1
            if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &                   r11,b11,0)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &                   r12,b12,0)
            dl=tlng-xlng2
            if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &                   r21,b21,0)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &                   r22,b22,0)
            rhom(1)=0.
            rhom(2)=MIN(20000.,
     &                   1000.*AINT(MAX(r11,r12,r21,r22)*6.366+.9))
            RETURN
         end if
      end if

c     Parameters of the antipode of the transmitter
      if (tlon .lt. 0.) then
         alon=tlon+180.
      else
         alon=tlon-180.
      end if
      alat=-tlat
      alng=alon*dtr
      aclt=pi-tclt
      caclt=-ctclt
      saclt= stclt

      if (ABS(xlon1-xlon2) .eq. 360. .or. xlon1-xlon2 .eq. 0. .or.
     &   (xlon1 .gt. xlon2 .and.
     &   (alon  .le. xlon1 .and. alon .ge. xlon2))  .or.
     &   (xlon1 .lt. xlon2 .and.
     &   (alon  .le. xlon1 .or.  alon .ge. xlon2))) then

c        Antipode is bounded by operating area meridians
         flaga=.true.
         if (aclt .lt. xclt1 .and. aclt .gt. xclt2) then

c           Antipode is inside of op area
            rbear(1)=0.
            rbear(2)=360.-rbear(3)

c           Get ranges to corners of the op area
            dl=alng-xlng1
            if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &                   r11,b11,0)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &                   r12,b12,0)
            dl=alng-xlng2
            if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &                   r21,b21,0)
            call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &                   r22,b22,0)
            rhom(1)=MIN(20000.,
     &                   1000.*AINT(MIN(r11,r12,r21,r22)*6.366))
            rhom(2)=20000.
            RETURN
         end if
      end if

c     Check if the transmitter is bounded by the op area meridians and
c     is below the op area:
      if (flagr .and. tlat .le. xlat1) then

c        The bearings must vary from 180 to greater than 360
         flagb=.true.

c        Get tangent bearings
         angl=ASIN(sxclt1/stclt)
         bmin=twopi-angl
         bmax=twopi+angl

c        Compute distances to corners of the op area
         dl=tlng-xlng1
         if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
         call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &                r11,b11,0)
         call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &                r12,b12,0)
         dl=tlng-xlng2
         if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
         call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &                r21,b21,0)
         call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &                r22,b22,0)

         if (bmin .lt. b11      ) bmin=b11
         if (bmax .gt. b21+twopi) bmax=b21+twopi

         rbear(1)=AINT(bmin*rtd/rbear(3)   )*rbear(3)
         rbear(2)=AINT(bmax*rtd/rbear(3)+.9)*rbear(3)
         if (rbear(1) .eq. 0. .and. rbear(2) .eq. 360.)
     &       rbear(2)=rbear(2)-rbear(3)
         rmin=MIN(r11,r12,r21,r22)
         rmax=MAX(r11,r12,r21,r22)
         rhom(1)=MIN(20000.,1000.*AINT(rmin*6.366))
         rhom(2)=MIN(20000.,1000.*AINT(rmax*6.366+.9))
         RETURN
      else

c        Check for trans-polar paths; look for the antipode being
c        bounded by the meridians of the op area:
         if (flaga .and. tclt+xclt1 .lt. pi) then

c           The bearings must vary from 180 to greater than 360
            flagb=.true.
         else
            flagb=.false.
         end if
      end if

c     Compute bearings to corners of the op area
      dl=tlng-xlng1
      if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
      call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &             r11,b11,0)
      if (flagb .and. dl .ge. 0.) b11=b11+twopi
      call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &             r12,b12,0)
      if (flagb .and. dl .ge. 0.) b12=b12+twopi
      dl=tlng-xlng2
      if (ABS(dl) .gt. pi) dl=dl-SIGN(twopi,dl)
      call GCDBR2 (dl,tclt,ctclt,stclt,xclt2,cxclt2,sxclt2,
     &             r21,b21,0)
      if (flagb .and. dl .ge. 0.) b21=b21+twopi
      call GCDBR2 (dl,tclt,ctclt,stclt,xclt1,cxclt1,sxclt1,
     &             r22,b22,0)
      if (flagb .and. dl .ge. 0.) b22=b22+twopi
      bmin=MIN(b11,b12,b21,b22)
      bmax=MAX(b11,b12,b21,b22)
      rmin=MIN(r11,r12,r21,r22)
      rmax=MAX(r11,r12,r21,r22)
      if (flagr) then
         if (tclt .lt. xclt1) then

c           Transmitter is above operating area
            rmin=MIN(rmin,xclt2-tclt)
            rmax=MAX(rmax,xclt1-tclt)
         else

c           Transmitter is below operating area
            rmin=MIN(rmin,tclt-xclt1)
            rmax=MAX(rmax,tclt-xclt2)
         end if
      end if

      if (flaga) then
         if (aclt .lt. xclt1) then

c           Antipode is above operating area
            rmin=MIN(rmin,twopi-tclt-xclt1)
            rmax=MAX(rmax,twopi-tclt-xclt2)
         else

c           Antipode is below operating area
            rmin=MIN(rmin,tclt+xclt2)
            rmax=MAX(rmax,tclt+xclt1)
         end if
      end if
      rbear(1)=AINT(bmin*rtd/rbear(3)   )*rbear(3)
      rbear(2)=AINT(bmax*rtd/rbear(3)+.9)*rbear(3)
      if (rbear(1) .eq. 0. .and. rbear(2) .eq. 360.)
     &   rbear(2)=rbear(2)-rbear(3)
      rhom(1)=MIN(20000.,1000.*AINT(rmin*6.366))
      rhom(2)=MIN(20000.,1000.*AINT(rmax*6.366+.9))
      RETURN

      END      ! LWP_GET_BRNG
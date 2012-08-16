      SUBROUTINE LWP_TO_EDGE
     &          (tlat,tlon,op_lat,op_lon,bearng,range)

c     Returns the range to the edge of an operating area along a
c     specified path

c     tlat          latitude  of the transmitter; deg North
c     tlon          longitude of the transmitter; deg West
c     op_lat        latitude  bounds of the area; deg North
c     op_lon        longitude bounds of the area; deg West
c     bearng        bearing of the path; deg East of North
c     range         range to the edge of the area; km

c     NOTES:
c                   op_lat(1),op_lon(1) lower left  corner of the area
c                   op_lat(2),op_lon(2) upper right corner of the area

      logical       end_loop

      dimension     op_lat(2),op_lon(2)

      data          dtr/.0174532925219943/,
     &              pi/3.1415926535898/,twopi/6.2831853071796/


c     Path bearing
      xtr=bearng*dtr

c     Transmitter parameters
      tlng=tlon*dtr
      tclt=(90.-tlat)*dtr
      ctclt=COS(tclt)
      stclt=SIN(tclt)

c     Parameters of the antipode of the transmitter
      if(tlng .lt. 0.) then
         alng=tlng+pi
      else
         alng=tlng-pi
      end if
      aclt=pi-tclt
      caclt=-ctclt
      saclt= stclt

c     Corners of op area
      xlon1=op_lon(1)
      xlon2=op_lon(2)
      if(xlon2 .ge. xlon1) xlon2=xlon2-360.
      xlng1=xlon1*dtr
      xlng2=xlon2*dtr
      xlat1=op_lat(1)
      xclt1=(90.-xlat1)*dtr
      cxclt1=COS(xclt1)
      sxclt1=SIN(xclt1)
      xlat2=op_lat(2)
      xclt2=(90.-xlat2)*dtr
      cxclt2=COS(xclt2)
      sxclt2=SIN(xclt2)

      rlng=alng
      if(rlng .ge. xlng1) rlng=rlng-twopi
      if(rlng .lt. xlng1 .and. rlng .gt. xlng2) then

c        Antipode is bounded by operating area meridians
         if(aclt .lt. xclt1 .and. aclt .gt. xclt2) then

c           Antipode is inside of op area
            range=20000.
            RETURN
         end if
      end if

c     Check for operating areas which form strips
      if(xlat2 .ge. 0. .and. xlon1-xlon2 .eq. 360.) then

c        Northern strip op area

         rclt=aclt
         rho=20000.
         range=20000.

         do while (rclt .gt. xclt1 .and. rho .gt. 0.)
            xclt=rclt
            range=rho
            rho=rho-2000.
            call RECVR2 (tlng,tclt,ctclt,stclt,xtr,rho/6366.,
     &                   rlng,rclt,crclt,srclt)
         end do
         rclt=xclt
         rho=range
         do while (rclt .gt. xclt1 .and. rho .ge. 0.)
            range=rho
            rho=rho-500.
            call RECVR2 (tlng,tclt,ctclt,stclt,xtr,rho/6366.,
     &                   rlng,rclt,crclt,srclt)
         end do
      else

         if(xlat1 .le. 0. .and. xlon1-xlon2 .eq. 360.) then

c           Southern strip op area

            rclt=aclt
            rho=20000.
            range=20000.
            do while (rclt .lt. xclt2 .and. rho .gt. 0.)
               xclt=rclt
               range=rho
               rho=rho-2000.
               call RECVR2 (tlng,tclt,ctclt,stclt,xtr,rho/6366.,
     &                      rlng,rclt,crclt,srclt)
            end do
            rclt=xclt
            rho=range
            do while (rclt .lt. xclt2 .and. rho .ge. 0.)
               range=rho
               rho=rho-500.
               call RECVR2 (tlng,tclt,ctclt,stclt,xtr,rho/6366.,
     &                      rlng,rclt,crclt,srclt)
            end do
         else

c           Op area is not a strip

            rclt=aclt
            rlng=alng
            rho=20000.
            range=20000.
            end_loop=.false.
            do while (.not.end_loop .and. rho .gt. 0.)
               xclt=rclt
               xlng=rlng
               range=rho
               rho=rho-2000.
               call RECVR2 (tlng,tclt,ctclt,stclt,xtr,rho/6366.,
     &                      rlng,rclt,crclt,srclt)

               if (rclt .lt. xclt1 .and. rclt .gt. xclt2) then
                  if (rlng .gt. xlng1) rlng=rlng-twopi
                  if (rlng .gt. xlng2) end_loop=.true.
               end if
            end do
            rclt=xclt
            rlng=xlng
            rho=range
            end_loop=.false.
            do while (.not.end_loop .and. rho .ge. 0.)
               range=rho
               rho=rho-500.
               call RECVR2 (tlng,tclt,ctclt,stclt,xtr,rho/6366.,
     &                      rlng,rclt,crclt,srclt)

               if (rclt .lt. xclt1 .and. rclt .gt. xclt2) then
                  if (rlng .gt. xlng1) rlng=rlng-twopi
                  if (rlng .gt. xlng2) end_loop=.true.
               end if
            end do
         end if
      end if

      RETURN
      END      ! LWP_TO_EDGE
      SUBROUTINE LWP_SET_BRNG
     &          (tlat,tlon,bflag,op_lat,op_lon,brsltn,
     &           mxpath,nrpath,brng,rng,
     &           rxlat,rxlon,rxrho,rng_mx)

c     Returns a list of bearings for paths which span an operating area

c     tlat          transmitter latitude  in deg N
c     tlon          transmitter longitude in deg W
c     bflag         determines how the bearings are set
c     brsltn        bearing angle resolution in deg
c     brng          output list of bearing angles in deg
c     rng           output list of maximum ranges in km

c     mxpath        maximum number of bearings allowed
c     nrpath                number of bearings generated
c     rxlat         latitude  of receivers
c     rxlon         longitude of receivers
c     rxrho         range     to receivers in km
c     rng_mx        maximum range in km

      character*200 error_msg
      logical       flag
      integer       bflag

      dimension     op_lat(2),op_lon(2),brng(mxpath),rng(mxpath),
     &              rxlat(mxpath),rxlon(mxpath),rxrho(mxpath),
     &              brsltn(2),rbear(3),rhom(2),glat(2,24),glon(2,24)

c     Boundaries of low conductivity areas
      data      glat/  75.5,  82.5,    68.5,  75.5,    61.0,  68.5,
     &                 61.0,  68.5,    56.0,  61.0,    51.0,  61.0,
     &                 51.0,  52.5,    33.5,  36.0,    77.5,  80.0,
     &                 74.5,  77.0,    70.5,  74.5,    65.5,  69.5,
     &                 60.5,  65.5,    63.0,  70.5,    64.5,  68.0,
     &                 48.0,  59.5,    50.5,  59.5,   -18.5, -15.5,
     &                -26.0, -21.0,   -33.0, -26.0,   -72.5, -66.0,
     &                -90.0, -72.5,   -90.0, -67.5,   -90.0, -77.5/,
     &          glon/  96.0,  20.0,   123.5,  20.0,   118.0,  62.0,
     &                 52.0,  37.0,   111.0,  96.0,    78.0,  57.5,
     &                119.0, 116.5,    85.5,  80.0,   -13.0, -23.0,
     &                -58.0, -67.5,   -54.0, -57.0,   -64.0, -67.5,
     &                -58.5, -60.5,   -90.0,-120.0,  -165.5, 172.0,
     &                -90.0,-120.0,  -120.0,-137.0,    71.5,  68.0,
     &                 68.5,  66.5,    70.0,  68.5,    74.0,  60.0,
     &                150.0,  30.0,    30.0,-160.0,  -160.0, 150.0/,
     &          dtr/.01745329252/


      if (bflag .eq. 0) then

c        Bearings and ranges based on op area

c        Get initial bearing angles using parameters of the op area and
c        the fine bearing angle resolution
         rbear(3)=brsltn(2)
         call LWP_GET_BRNG (tlat,tlon,op_lat,op_lon,rbear,rhom)
         rng_mx=AMIN1(20000.,rhom(2))

c        Set up list of bearings and ranges to the edge of the op area
c        using the coarse bearing resolution
         brngmn=AINT(rbear(1)/brsltn(1)   )*brsltn(1)
         brngmx=AINT(rbear(2)/brsltn(1)+.9)*brsltn(1)
         if (brngmn .eq.   0. .and.
     &       brngmx .eq. 360.) brngmx=brngmx-brsltn(1)
         nrpath=0
         do br=brngmn,brngmx,brsltn(1)

            nrpath=nrpath+1
            brng(nrpath)=br
            call LWP_TO_EDGE
     &          (tlat,tlon,op_lat,op_lon,br,rng(nrpath))
         end do

c        Re-define first and last bearing angles, as required,
c        using the fine bearing angle resolution
         brng(  1  )=rbear(1)
         call LWP_TO_EDGE
     &       (tlat,tlon,op_lat,op_lon,brng(  1  ),rng(  1  ))

         if (brng(nrpath) .ne. 360.) then
c           Adjust the last bearing
            brng(nrpath)=rbear(2)
            call LWP_TO_EDGE
     &          (tlat,tlon,op_lat,op_lon,brng(nrpath),rng(nrpath))
         end if

c        Verify that angles start with values less than 360
         if (brng(1) .ge. 360.) then
            do j=1,nrpath
               brng(j)=brng(j)-360.
            end do
         end if

c        Some ranges may be zero because the paths just miss the
c        op area; adjust these paths to have the length of the
c        nearest path with finite length
         jm=0
         do while (jm .lt. nrpath .and. rng(jm+1) .eq. 0.)
            jm=jm+1
         end do
         if (jm .lt. nrpath) then

            do j=1,jm
               rng(j)=rng(jm+1)
            end do

            jm=nrpath+1
            do while (jm .gt. 1 .and. rng(jm-1) .eq. 0.)
               jm=jm-1
            end do

            if (jm .gt. 1) then
               do j=nrpath,jm,-1
                  rng(j)=rng(jm-1)
               end do
            end if
         else

            do j=1,nrpath
               rng(j)=rng_mx
            end do
         end if

c        Additional bearing angles needed to cover low
c        conductivity areas
         brngmn=brng(  1  )
         brngmx=brng(nrpath)
         rbear(3)=brsltn(2)
         do i=1,24

c           Get range of bearings which span the low conductivity area
            call LWP_GET_BRNG
     &          (tlat,tlon,glat(1,i),glon(1,i),rbear,rhom)

            brmin=rbear(1)
            brmax=rbear(2)
            br=brmin
            do while (br .le. brmax)
               b=amod(br,360.)
               if (brngmx .le. 360. .and.
     &            (b .gt. brngmn .and. b .lt. brngmx) .or.
     &             brngmx .gt. 360. .and.
     &            (b .gt. brngmn .or.  b .lt. brngmx-360.)) then

c                 Look for this bearing in the current list of bearings
                  flag=.true.
                  j=1
                  do while (flag .and. j .le. nrpath)
                     if (ABS(b-amod(brng(j),360.)) .lt. .5*brsltn(2))
     &                  flag=.false.
                     j=j+1
                  end do
                  if (flag) then ! another bearing may be required
                     if (brngmx .gt. 360. .and. b .lt. brngmn) b=b+360.

c                    Get the range to the edge of the op area
                     call LWP_TO_EDGE
     &                   (tlat,tlon,op_lat,op_lon,b,range)

                     if (rhom(1) .lt. range+1000.) then
c                       Add this bearing because the low conductivity
c                       area is within 1000 km of the op area
                        if (nrpath .eq. mxpath) then

                           write(error_msg,
     &                         '(''[LWP_SET_BRNG]: '',
     &                           ''Too many bearings'')')
                           call LWPC_ERROR ('ERROR',error_msg)

                        else

c                          Insert the new bearing into the list
                           if (b .lt. brng(nrpath)) then
                              j=1
                              do while (b .gt. brng(j))
                                 j=j+1
                              end do
                              do k=nrpath+1,j+1,-1
                                 brng(k)=brng(k-1)
                                 rng(k)=rng(k-1)
                              end do
                           else
                              j=nrpath+1
                           end if
                           nrpath=nrpath+1
                           brng(j)=b
                           rng(j)=range
                        end if
                     end if
                  end if
               end if
               br=br+brsltn(2)
            end do
         end do

c        Re-define ranges to the edge of the op area
         do nb=1,nrpath
            call LWP_TO_EDGE
     &          (tlat,tlon,op_lat,op_lon,brng(nb),rng(nb))
         end do

c        Some ranges may be zero because the paths just miss the op
c        area; adjust these paths to have the length of the nearest
c        path with finite length
         jm=0
         do while (jm .lt. nrpath .and. rng(jm+1) .eq. 0.)
            jm=jm+1
         end do
         if (jm .lt. nrpath) then

            do j=1,jm
               rng(j)=rng(jm+1)
            end do

            jm=nrpath+1
            do while (jm .gt. 1 .and. rng(jm-1) .eq. 0.)
               jm=jm-1
            end do

            if (jm .gt. 1) then
               do j=nrpath,jm,-1
                  rng(j)=rng(jm-1)
               end do
            end if
         else

            do j=1,nrpath
               rng(j)=rng_mx
            end do
         end if

c        Finally, adjust the ranges so that paths are at least as long
c        as the longest adjacent one.  First, find the longest path.
         j0=1
         rngmax=rng(1)
         do j=2,nrpath
            if (rngmax .lt. rng(j)) then
               j0=j
               rngmax=rng(j)
            end if
         end do

         if (j0 .eq. 1) then

c           The longest path is the first one
c           Start at J0 and go around to the last path
            if (ABS(brng(nrpath)-brng(1)) .le. brsltn(1)+brsltn(2)) then
               rjm1=rng(nrpath)
            else
               rjm1=0.
            end if
            do j=1,nrpath-1
               rj=rng(j)
               rjp1=rng(j+1)
               if (rng(j) .lt. AMAX1(rjm1,rj,rjp1))
     &             rng(j)=AMAX1(rjm1,rj,rjp1)
               rjm1=rj
            end do
            rj=rng(nrpath)
            if (ABS(brng(nrpath)-brng(1)) .le. brsltn(1)+brsltn(2)) then
               rjp1=rng(1)
            else
               rjp1=0.
            end if
            rng(nrpath)=AMAX1(rjm1,rj,rjp1)

         else
     &   if (j0 .eq. nrpath) then

c           The longest path is the last one
c           Start at J0 and go around to the first path
            if (ABS(brng(nrpath)-brng(1)) .le. brsltn(1)+brsltn(2)) then
               rjp1=rng(1)
            else
               rjp1=0.
            end if
            do j=nrpath,2,-1
               rj=rng(j)
               rjm1=rng(j-1)
               if (rng(j) .lt. AMAX1(rjm1,rj,rjp1))
     &             rng(j)=AMAX1(rjm1,rj,rjp1)
               rjp1=rj
            end do
            rj=rng(1)
            if (ABS(brng(nrpath)-brng(1)) .le. brsltn(1)+brsltn(2)) then
               rjm1=rng(nrpath)
            else
               rjm1=0.
            end if
            rng(1)=AMAX1(rjm1,rj,rjp1)
         else

c           The longest path is an intermediate one
c           First, start at J0 and go around to the last path
            rjm1=rng(j0-1)
            do j=j0,nrpath-1
               rj=rng(j)
               rjp1=rng(j+1)
               if (rng(j) .lt. AMAX1(rjm1,rj,rjp1))
     &             rng(j)=AMAX1(rjm1,rj,rjp1)
               rjm1=rj
            end do
            rj=rng(nrpath)
            if (ABS(brng(nrpath)-brng(1)) .le. brsltn(1)+brsltn(2)) then
               rjp1=rng(1)
            else
               rjp1=0.
            end if
            rng(nrpath)=AMAX1(rjm1,rj,rjp1)
c           Now, start at J0 and go around to the first path
            rjp1=rng(j0+1)
            do j=j0,2,-1
               rj=rng(j)
               rjm1=rng(j-1)
               if (rng(j) .lt. AMAX1(rjm1,rj,rjp1))
     &             rng(j)=AMAX1(rjm1,rj,rjp1)
               rjp1=rj
            end do
            rj=rng(1)
            if (ABS(brng(nrpath)-brng(1)) .le. brsltn(1)+brsltn(2)) then
               rjm1=rng(nrpath)
            else
               rjm1=0.
            end if
            rng(1)=AMAX1(rjm1,rj,rjp1)
         end if

         if (brng(nrpath) .eq. 360.) then
c           This is a 0 to 360 fan of bearings;
c           drop the one at 360 since it is redundant
            nrpath=nrpath-1
         end if
      else
     &if (bflag .eq. 1) then

c        User specified bearings
         do j=1,nrpath
            rng(j)=rng_mx
         end do
      else
     &if (bflag .eq. 2) then

c        Bearings based on list of receivers
         do j=1,nrpath

            call GCDBR
     &          ((tlon-rxlon(j))*dtr,(90.-tlat)*dtr,(90.-rxlat(j))*dtr,
     &           r,b,0)
            rxrho(j)=r*6366.197
            brng(j)=b/dtr
            rng(j)=rng_mx
         end do
      else

         write(error_msg,
     &       '(''[LWP_SET_BRNG]: '',
     &         ''Invalid value for BFLAG: '',i3)') bflag
         call LWPC_ERROR ('ERROR',error_msg)
      end if

c     Now make sure that the bearings are in the range 0 to 360
      if (nrpath .gt. 1) then
         do j=1,nrpath
            if (brng(j) .ge. 360.) brng(j)=brng(j)-360.
         end do
      end if

      RETURN
      END      ! LWP_SET_BRNG
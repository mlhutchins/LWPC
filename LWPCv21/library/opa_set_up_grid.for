      SUBROUTINE OPA_SET_UP_GRID
     &          (delta_min,
     &           mxlat,nrlat,mxlon,nrlon,
     &           xlat1,xlon1,xlat2,xlon2,
     &           xclt,cxclt,sxclt,xlng)

c Generates grid of latitude and longitude;
c Sign convention is + for West and North

c mxlat       maximum number of points in latitude               INTEGER
c nrlat       number of points output for latitude               INTEGER

c mxlon       maximum number of points in longitude              INTEGER
c nrlon       number of points output for longitude              INTEGER

c delta_min   minimum increment of latitude/longitude               REAL

c xlat1       latitude  of the lower left  corner                   REAL
c xlon1       longitude of the lower left  corner                   REAL
c xlat2       latitude  of the upper right corner                   REAL
c xlon2       longitude of the upper right corner                   REAL

c xlng        longitude points in radians                           REAL
c xclt        latitude  points in radians                           REAL
c cxclt       cosine(xclt)                                          REAL
c sxclt         sine(xclt)                                          REAL

      real            xclt(mxlat),cxclt(mxlat),sxclt(mxlat),xlng(mxlon)

      data            xlats1,xlats2,xlons1,xlons2/4*720./


c     Test if this is a new op area grid
      if (xlon1 .eq. xlons1 .and. xlat1 .eq. xlats1 .and.
     &    xlon2 .eq. xlons2 .and. xlat2 .eq. xlats2) return

c     Longitude increment is chosen to give maximum resolution within
c     the op area consistent with the maximum number of points allowed
      xlng1=xlon1
      xlng2=xlon2
      if (xlng2 .ge. xlng1) xlng2=xlng2-360.

      delta=10.
      mx=(xlng1-xlng2)/delta+1.
      do while (mx .le. mxlon .and. delta .ge. delta_min)
         nrlon=mx
         delta=delta/2.
         mx=(xlng1-xlng2)/delta+1.
      end do
      delta=delta*2.

      sum=xlng1
      do j=1,nrlon
         xlng(j)=sum*.01745329252 ! convert to radians
         sum=sum-delta
         if (sum .lt. -180.) sum=sum+360.
      end do

c     Latitude increment is chosen to give maximum resolution within
c     the op area consistent with the maximum number of points allowed
      xclt1=90.-xlat1
      xclt2=90.-xlat2

      delta=10.
      my=(xclt1-xclt2)/delta+1.
      do while (my .le. mxlat .and. delta .ge. delta_min)
         nrlat=my
         delta=delta/2.
         my=(xclt1-xclt2)/delta+1.
      end do
      delta=delta*2.

      sum=xclt1
      do j=1,nrlat
         xclt(j)=sum*.01745329252 ! convert to radians
         cxclt(j)=cos(xclt(j))
         sxclt(j)=sin(xclt(j))
         sum=sum-delta
      end do

c     Save test parameters
      xlons1=xlon1
      xlons2=xlon2
      xlats1=xlat1
      xlats2=xlat2

      return
      END      ! OPA_SET_UP_GRID
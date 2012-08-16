      SUBROUTINE PLT_LNP_TO_GRD
     &          (nrcmp,mxlat,mxlon,
     &           nrlat1,nrlon1,nrlat2,nrlon2,
     &           oplat1,oplon1,oplat2,oplon2,
     &           ampn,stdn,vsdn,tmp)

c***********************************************************************

c Interpolates from coarse LNP grid to LWPC spacing.

c Keep it simple:
c    Assume that the boundaries of the two grids are the same.

c***********************************************************************

c     nrcmp             number of components of noise
c     mxlat             dimension of the grid arrays in latitude
c     mxlon             dimension of the grid arrays in longitude

c     nrlat1            input  number of points in latitude
c     nrlon1            input  number of points in longitude
 
c     nrlat2            output number of points in latitude
c     nrlon2            output number of points in longitude

c     oplat1,
c     oplon1

c     oplat2,
c     oplon2

c     ampn
c     stdn
c     vsdn

c     tmp

c***********************************************************************

c  Change history:

c*******************!***************************************************

      real          ampn(mxlon,mxlat,2),stdn(mxlon,mxlat,2),
     &              vsdn(mxlon,mxlat,2),
     &              tmp (mxlon,mxlat  )
      real          lat1,lat2,lon1,lon2


c     Test for identical input and output grids
      if (nrlat1 .eq. nrlat2 .and. nrlon1 .eq. nrlon2) RETURN

c     Set up grids
      lat1=oplat1
      lat2=oplat2
      lon1=oplon1
      lon2=oplon2
      if (lon2 .eq. lon1) lon2=lon2-360.

      dlat1=(lat2-lat1)/(nrlat1-1)
      dlat2=(lat2-lat1)/(nrlat2-1)
      dlon1=(lon2-lon1)/(nrlon1-1)
      dlon2=(lon2-lon1)/(nrlon2-1)

      do nc=1,nrcmp

c        Transfer input to temporary arrays
         do nlat1=1,nrlat1
            do nlon1=1,nrlon1
               tmp(nlon1,nlat1)=ampn(nlon1,nlat1,nc)
            end do
         end do

c        Interpolate input grid into the output grid
         xlon1=lon1
         xlon2=lon1
         nlon1=1
         do nlon2=1,nrlon2

            slope1=(xlon2-xlon1)/dlon1

            xlat1=lat1
            xlat2=lat1
            nlat1=1
            do nlat2=1,nrlat2

               slope2=(xlat2-xlat1)/dlat1

               a1=slope1*(tmp(nlon1+1,nlat1  )-tmp(nlon1,nlat1  ))+
     &                    tmp(nlon1  ,nlat1  )
               a2=slope1*(tmp(nlon1+1,nlat1+1)-tmp(nlon1,nlat1+1))+
     &                    tmp(nlon1  ,nlat1+1)

               ampn(nlon2,nlat2,nc)=slope2*(a2-a1)+a1

               xlat2=xlat2+dlat2
               if (xlat2 .gt. xlat1+dlat1) then
                  xlat1=xlat1+dlat1
                  nlat1=nlat1+1
               end if
            end do
            xlon2=xlon2+dlon2
            if (xlon2 .lt. xlon1+dlon1) then
               xlon1=xlon1+dlon1
               nlon1=nlon1+1
            end if
         end do

c        Transfer input to temporary arrays
         do nlat1=1,nrlat1
            do nlon1=1,nrlon1
               tmp(nlon1,nlat1)=stdn(nlon1,nlat1,nc)
            end do
         end do

c        Interpolate input grid into the output grid
         xlon1=lon1
         xlon2=lon1
         nlon1=1
         do nlon2=1,nrlon2

            slope1=(xlon2-xlon1)/dlon1

            xlat1=lat1
            xlat2=lat1
            nlat1=1
            do nlat2=1,nrlat2

               slope2=(xlat2-xlat1)/dlat1

               a1=slope1*(tmp(nlon1+1,nlat1  )-tmp(nlon1,nlat1  ))+
     &                    tmp(nlon1  ,nlat1  )
               a2=slope1*(tmp(nlon1+1,nlat1+1)-tmp(nlon1,nlat1+1))+
     &                    tmp(nlon1  ,nlat1+1)

               stdn(nlon2,nlat2,nc)=slope2*(a2-a1)+a1

               xlat2=xlat2+dlat2
               if (xlat2 .gt. xlat1+dlat1) then
                  xlat1=xlat1+dlat1
                  nlat1=nlat1+1
               end if
            end do
            xlon2=xlon2+dlon2
            if (xlon2 .lt. xlon1+dlon1) then
               xlon1=xlon1+dlon1
               nlon1=nlon1+1
            end if
         end do

c        Transfer input to temporary arrays
         do nlat1=1,nrlat1
            do nlon1=1,nrlon1
               tmp(nlon1,nlat1)=vsdn(nlon1,nlat1,nc)
            end do
         end do

c        Interpolate input grid into the output grid
         xlon1=lon1
         xlon2=lon1
         nlon1=1
         do nlon2=1,nrlon2

            slope1=(xlon2-xlon1)/dlon1

            xlat1=lat1
            xlat2=lat1
            nlat1=1
            do nlat2=1,nrlat2

               slope2=(xlat2-xlat1)/dlat1

               a1=slope1*(tmp(nlon1+1,nlat1  )-tmp(nlon1,nlat1  ))+
     &                    tmp(nlon1  ,nlat1  )
               a2=slope1*(tmp(nlon1+1,nlat1+1)-tmp(nlon1,nlat1+1))+
     &                    tmp(nlon1  ,nlat1+1)

               vsdn(nlon2,nlat2,nc)=slope2*(a2-a1)+a1

               xlat2=xlat2+dlat2
               if (xlat2 .gt. xlat1+dlat1) then
                  xlat1=xlat1+dlat1
                  nlat1=nlat1+1
               end if
            end do
            xlon2=xlon2+dlon2
            if (xlon2 .lt. xlon1+dlon1) then
               xlon1=xlon1+dlon1
               nlon1=nlon1+1
            end if
         end do
      end do

      RETURN
      END      ! PLT_LNP_TO_GRD
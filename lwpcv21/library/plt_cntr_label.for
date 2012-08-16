      SUBROUTINE PLT_CNTR_LABEL
     &          (brief,xlbl,ylbl,
     &           ntlist,ntl,
     &           plot_lbl,cntr_lbl,
     &           xmtr_lbl,data_lbl,
     &           prfl_lbl,file_lbl,
     &           rcvr_lbl)

c Generates label for contour plots

c***********************************************************************

c Change history:
c     11 Nov 1993   Dropped PLT_UNIT for conversion to OS/2.

c     09 Feb 1994   Changed PLT_DEVICE and PLT_ORIENTATION to
c                   CHARACTER*(*).

c     30 Jun 1994   Dropped PRGM_ID and RN_DATE from argument list.

c     12 Jul 1994   Modified to use new WORLD_MAP.

c     03 Oct 1994   Moved world map call to PLT_CNTR and PLT_EXCD so
c                   that it overlays the filled contours.

c     07 Jun 1995   Moved the graphics initialization to the calling
c                   routine; added brief label format.

c     12 Mar 1997   Modified to use new string routines in GRF.

c*******************!***************************************************

      character*(*) cntr_lbl
      character* 28 xmtr_lbl(-1:6)
      character* 48 rcvr_lbl(-1:1)
      character* 40 prfl_lbl(-1:6),data_lbl(-1:6)
      character* 80 plot_lbl
      character*120 file_lbl(-1:6),
     &              pltlbl
      logical       brief
      integer       ntlist(5)


c     User plot label at bottom left corner of the plotting media
      call GRF_STRING (.5,0.,.1,plot_lbl,0.,'LB',retX)

c     Shift the rest of the plot up and to the right
      call GRF_ORIGIN (.5,1.)

      if (brief) then

         x0=xlbl+.3
         y0=ylbl-.15

         xp=x0
         yp=y0
         adjX=0.
         if (ntl .gt. 0) then

c           Do transmitter labels
            do nl=0,ntl
               if (nl .lt. 1) then
                  ntr=nl
               else
                  ntr=ntlist(nl)
               end if
               pltlbl=xmtr_lbl(ntr)
               call GRF_STRING (x0,yp,.1,pltlbl,0.,'LB',retX)
               adjX=MAX(adjX,retX)
               yp=yp-.15
            end do
            yp=yp-.15

c           Use the profile id just once to show the date and time
            call GRF_STRING (x0,yp,.1,prfl_lbl(ntlist(ntl)),0.,
     &                       'LB',retX)
            yp=yp-.15
         end if
         yp=yp-.15

         if (cntr_lbl(:5) .eq. 'Noise'   .or.
     &       cntr_lbl(:3) .eq. 'J/N'     .or.
     &       cntr_lbl(:3) .eq. 'S/N'     .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') then

c           Noise label
            pltlbl=xmtr_lbl(6)
            call GRF_STRING (x0,yp,.1,pltlbl,0.,'LB',retX)
            adjX=MAX(adjX,retX)
            yp=yp-.15
         end if

         x1=x0+adjX+.1
         yp=y0
         adjX=0.
         if (ntl .gt. 0) then

c           Now append the transmitter data
            do nl=0,ntl
               if (nl .lt. 1) then
                  ntr=nl
               else
                  ntr=ntlist(nl)
               end if
               pltlbl=data_lbl(ntr)
               call GRF_STRING (x1,yp,.1,pltlbl,0.,'LB',retX)
               adjX=MAX(adjX,retX)
               yp=yp-.15
            end do
            yp=yp-.15

c           Insert double space to skip over the profile id
            yp=yp-.15
         end if
         yp=yp-.15

         if (cntr_lbl(:5) .eq. 'Noise'   .or.
     &       cntr_lbl(:3) .eq. 'J/N'     .or.
     &       cntr_lbl(:3) .eq. 'S/N'     .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') then

c           Noise data
            pltlbl=data_lbl(6)
            call GRF_STRING (x1,yp,.1,pltlbl,0.,'LB',retX)
            adjX=MAX(adjX,retX)
            yp=yp-.15
         end if
      else

c        Establish the location of the first group of labels
         x0=0.
         y0=ylbl+.3
         if (ntl .gt. 0) y0=y0+.15*(ntl+1)
         if (cntr_lbl(:5) .eq. 'Noise' .or.
     &       cntr_lbl(:3) .eq. 'J/N'   .or.
     &       cntr_lbl(:3) .eq. 'S/N'   .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') y0=y0+.3

         xp=x0
         yp=y0
         adjX=0.
         if (ntl .gt. 0) then

c           Do transmitter names
            do nl=-1,ntl
               if (nl .lt. 1) then
                  ntr=nl
               else
                  ntr=ntlist(nl)
               end if
               pltlbl=xmtr_lbl(ntr)
               call GRF_STRING (x0,yp,.1,pltlbl,0.,'LB',retX)
               adjX=MAX(adjX,retX)
               yp=yp-.15
            end do
         end if
         yp=yp-.15

         if (cntr_lbl(:5) .eq. 'Noise'  .or.
     &       cntr_lbl(:3) .eq. 'J/N'    .or.
     &       cntr_lbl(:3) .eq. 'S/N'    .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') then

c           Noise data labels
            pltlbl=xmtr_lbl(6)
            call GRF_STRING (x0,yp,.1,pltlbl,0.,'LB',retX)
            adjX=MAX(adjX,retX)
         end if

         x1=x0+adjX+.1
         yp=y0
         adjX=0.
         if (ntl .gt. 0) then

c           Now append the transmitter data
            do nl=-1,ntl
               if (nl .lt. 1) then
                  ntr=nl
               else
                  ntr=ntlist(nl)
               end if
               pltlbl=data_lbl(ntr)
               call GRF_STRING (x1,yp,.1,pltlbl,0.,'LB',retX)
               adjX=MAX(adjX,retX)
               yp=yp-.15
            end do
         end if
         yp=yp-.15

         if (cntr_lbl(:5) .eq. 'Noise'  .or.
     &       cntr_lbl(:3) .eq. 'J/N'    .or.
     &       cntr_lbl(:3) .eq. 'S/N'    .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') then

c           Noise data labels
            pltlbl=data_lbl(6)
            call GRF_STRING (x1,yp,.1,pltlbl,0.,'LB',retX)
            adjX=MAX(adjX,retX)
         end if

         x2=x1+adjX+.1
         yp=y0
         adjX=0.
         if (ntl .gt. 0) then

c           Now append the profile ids
            do nl=-1,ntl
               if (nl .lt. 1) then
                  ntr=nl
               else
                  ntr=ntlist(nl)
               end if
               pltlbl=prfl_lbl(ntr)
               call GRF_STRING (x2,yp,.1,pltlbl,0.,'LB',retX)
               adjX=MAX(adjX,retX)
               yp=yp-.15
            end do
         end if
         yp=yp-.15

         if (cntr_lbl(:5) .eq. 'Noise'  .or.
     &       cntr_lbl(:3) .eq. 'J/N'    .or.
     &       cntr_lbl(:3) .eq. 'S/N'    .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') then

c           Noise data labels
            pltlbl=prfl_lbl(6)
            call GRF_STRING (x2,yp,.1,pltlbl,0.,'LB',retX)
            adjX=MAX(adjX,retX)
         end if

         x3=x2+adjX+.1
         yp=y0
         adjX=0.
         if (ntl .gt. 0) then

c           Now append the file names
            do nl=-1,ntl
               if (nl .lt. 1) then
                  ntr=nl
               else
                  ntr=ntlist(nl)
               end if
               pltlbl=file_lbl(ntr)
               call GRF_STRING (x3,yp,.1,pltlbl,0.,'LB',retX)
               adjX=MAX(adjX,retX)
               yp=yp-.15
            end do
         end if
         yp=yp-.15

         if (cntr_lbl(:5) .eq. 'Noise'  .or.
     &       cntr_lbl(:3) .eq. 'J/N'    .or.
     &       cntr_lbl(:3) .eq. 'S/N'    .or.
     &       cntr_lbl(:7) .eq. 'S/(J+N)') then

c           Noise data labels
            pltlbl=file_lbl(6)
            call GRF_STRING (x3,yp,.1,pltlbl,0.,'LB',retX)
            adjX=MAX(adjX,retX)
         end if
      end if

c     Generate receiver data labels
      xp=xlbl+.3
      yp=yp-.15
      do nl=-1,1
         call GRF_STRING (xp,yp,.1,rcvr_lbl(nl),0.,'LB',retX)
         yp=yp-.15
      end do

      xlbl=xp
      ylbl=yp-.15

      RETURN
      END      ! PLT_CNTR_LABEL
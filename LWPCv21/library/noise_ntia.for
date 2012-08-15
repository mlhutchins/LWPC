      SUBROUTINE NOISE_NTIA
     &          (rclt,rlng,freq,bandw,month,UT,
     &           Fam,sigm,Vd,sigv,Du,sigu,Dl,sigl,ampn,sign)

c Evaluates atmospheric noise values using the revised CCIR coefficients
c of Spaulding and Washburn, NTIA Report 85-173, 1985.

c rclt   co-latitude in radians, + for North
c rlng   longitude   in radians, + for West
c freq   frequency in kHz
c bandw  band width in Hz
c month  month number, 1 for Jan
c UT     Universal Time in hours

c Fam    F sub am
c Vd     V sub d
c Du     upper decile
c Dl     lower decile
c sigm   standard deviation of Fam
c sigV   standard deviation of V sub d
c sigu   standard deviation of the upper decile
c sigl   standard deviation of the lower decile

c ampn   noise in dB above 1uv/m
c sign   standard deviation of the noise in dB

c***********************************************************************

c Change history:
c     27 May 92     Added a new common block called LWPC_CFG, which is
c                   used to store configuration information, such as
c                   the location of the LWPC data files.

c     31 Jan 94     Added error checking on open statement.

c     05 May 95     Modified stdv to include Du and sigma Du

c     14 Oct 95     Modified to output all of the available parameters
c                   including Vd and sigma Vd from Spaulding and
c                   Washburn, NTIA Report 85-173, 1985, p 151.

c     25 Oct 95     Modified to use lwpc_LUN.cmn to set logical units.

c     05 Feb 98     Modified to use OPEN_DAT.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      character* 40 file_name

      dimension     abp(2,6),p(29,16,6),dud(5,12,5),fm(14,12),
     &              ss(15),yy(29),b(8,2),
     &              vdc(5,6,4),svc(5,6,4)

      data vdc/
     &-0.41558602,-0.75086980,-0.06268418,-1.61673242, 6.78459487,
     &-0.81912473,-0.90958560, 1.24573700,-1.80429429, 6.43835525,
     &-1.42224362,-1.18235587, 4.15636528,-1.31952622, 4.14994576,
     &-1.43530592,-1.40729681, 4.30851317,-0.61343299, 3.97698107,
     &-0.91361453,-1.02085307, 2.05263792,-1.20481693, 5.76797550,
     &-0.50080827,-0.64017944, 0.60033702,-1.72489385, 6.28328285,
     &-0.41030678,-0.55960790, 0.00841784,-1.82745777, 7.01077413,
     &-0.91796597,-0.85757646, 1.65778832,-1.74210229, 6.31934305,
     &-1.25456604,-0.83354033, 3.35449427,-2.02941542, 5.61265781,
     &-0.86433700,-0.36620300, 2.43257460,-2.43853742, 6.02758044,
     &-0.53006557,-0.21809625, 1.28866179,-2.14385922, 6.00629093,
     &-0.41749611,-0.39489071, 0.56516017,-1.81148452, 6.28661034,
     &-0.16848757,-0.48336779,-0.43701322,-1.72666100, 7.16722078,
     &-0.91533756,-0.88381532, 1.66817783,-2.18486442, 7.16769077,
     &-1.13551135,-0.78353352, 2.75493715,-2.32068743, 6.72491255,
     &-0.19841781, 0.03019611,-0.26471386,-2.60622295, 8.18005102,
     & 0.01027599, 0.20949386,-0.63008580,-2.55700412, 7.18374974,
     &-0.18063403,-0.20615342, 0.24834524,-1.81286248, 5.73012908,
     &-0.23938684,-0.49699564,-0.51079754,-2.14213999, 7.56139657,
     &-0.83830948,-0.70348334, 1.51059375,-2.36513950, 6.94308817,
     &-1.70706706,-1.17987406, 4.82706183,-1.76485628, 5.06494256,
     &-1.41821434,-1.11527452, 3.86485170,-1.40866619, 5.21901993,
     &-0.71187653,-0.61884451, 1.52472219,-1.78588700, 6.06417207,
     &-0.39956035,-0.47092766, 0.46849734,-1.92315743, 6.52222929/

      data svc/
     & 0.57455928, 0.46270770,-1.72173781,-0.99514089, 2.20240447,
     & 0.43548757, 0.42552238,-1.60117703,-1.08102724, 2.58152223,
     &-0.00649233, 0.04742536,-0.40321846,-0.62393815, 2.24785228,
     & 0.04815143, 0.03880005,-0.47981638,-0.75304762, 2.36761696,
     & 0.22095118, 0.10406723,-1.00765279,-0.76681271, 2.49052997,
     & 0.30631463, 0.15128836,-1.08000000,-0.55418334, 1.99341144,
     & 0.38661631, 0.80162317,-0.88703270,-1.52905085, 1.92759641,
     & 0.44005044, 0.71838334,-1.35584006,-1.64482072, 2.45213428,
     & 0.03671702,-0.05615371,-1.01505114,-0.38350154, 3.09611594,
     & 0.16528980, 0.01700605,-1.15989586,-0.30904361, 2.85528452,
     & 0.30793604, 0.38565473,-1.00679367,-1.02253224, 1.98359891,
     & 0.13143975, 0.40260509,-0.40377986,-0.99278232, 1.70204048,
     & 0.35890314, 0.49540937,-0.82712141, 0.76328633, 1.55427990,
     & 0.20996571, 0.32484470,-0.80782213,-0.88055988, 2.08557260,
     &-0.09085645,-0.17529001,-0.49066540,-0.19189031, 2.80704666,
     &-0.00029489,-0.20544629,-0.76822428, 0.11736728, 2.75988679,
     & 0.19109612, 0.18752264,-0.91125029,-0.57111168, 1.98460093,
     & 0.24757077, 0.39143866,-0.64392666,-0.76781344, 1.49625461,
     & 0.50175004, 0.57712940,-1.35720266,-1.04622956, 1.99818624,
     & 0.35592708, 0.42349037,-1.29001962,-0.96177452, 2.30245072,
     &-0.05578141,-0.12184221,-0.56293777,-0.31648731, 2.94384763,
     & 0.10767610,-0.12121232,-0.91529214,-0.27720854, 2.97756952,
     & 0.14056591, 0.06615351,-0.65144019,-0.41072914, 1.86781589,
     & 0.11440173, 0.20759506,-0.35086514,-0.52201531, 1.52884872/

      data          indsav/0/

      data          file_name/'ntia$d.dat'/


      if (freq .eq. 99.) then
c        test data for OPA_GRID
         ampn=20.
         sign=6.
         RETURN
      end if

c     Determine the 3-month block (season) for this month
      if (month .eq. 12) then
         indx=1
      else
         indx=month/3+1
      end if
      if (indx .ne. indsav) then
c        The data in the arrays is not for the correct 3-month block;
c        read the data file to get the correct data
         if (indsav .eq. 0) then
c           This is the first pass through the routine;
c           open the data file
            call OPEN_DAT (lwpcNTIA_lun,file_name,'unformatted')
         end if
c        Read data file until the correct 3-month block is available
         do i=1,indx
            read (lwpcNTIA_lun) abp,p,dud,fm
         end do
         REWIND (lwpcNTIA_lun)
         indsav=indx
      end if

c     Longitudinal analysis

c     Adjust the longitude to be East
      if (rlng .le. 0.) then
         xlng=ABS(rlng)
      else
         xlng=6.283185307-rlng
      end if

      q=.5*xlng
      s1=SIN(q)
      c1=COS(q)
      sx=s1
      cx=c1
      ss(1)=sx
      do j=2,15
         tx=sx
         sx=tx*c1+cx*s1
         cx=cx*c1-tx*s1
         ss(j)=sx
      end do

c     Latitude analysis
      q=3.141592653-rclt
      s1=SIN(q)
      c1=COS(q)
      sx=s1
      cx=c1
      do j=1,29
         yy(j)=sx
         tx=sx*c1+cx*s1
         cx=cx*c1-sx*s1
         sx=tx
      end do

c     Frequency interpolation

c     log of freq in MHz
      alogf=LOG10(freq)-3.

c     Linear interpolation between time blocks
      hour=UT
      if (hour .lt.  0.) hour=hour+24.
      if (hour .ge. 24.) hour=hour-24.

c     3.819718748 converts radians to hours based on 15 degrees/hour

      rhr=hour+xlng*3.819718748
      if (rhr .ge. 24.) rhr=rhr-24.

c     Determine the local time block
      iblk1=.25*rhr+1.
      if (iblk1 .gt. 6) iblk1=iblk1-6

      hr1=4*iblk1-2

      if (rhr .lt. hr1) then
         hr2=hr1-4.
         iblk2=iblk1-1
         if (iblk2 .lt. 1) iblk2=6
      else
     &if (rhr .gt. hr1) then
         hr2=hr1+4.
         iblk2=iblk1+1
         if (iblk2 .gt. 6) iblk2=1
      else
         hr2=hr1
         iblk2=iblk1
      end if

c     print '(''NTIA: rclt,rlng,month,hour,rhr,hr1,iblk1       '',
c    &        f6.3,f7.3,i3,f5.1,f7.2,f4.0,i3)',
c    &        rclt,rlng,month,UT,rhr,hr1,iblk1

      khr=1
      hr=hr1
      iblk=iblk1
      do while (khr .le. 2)

c        Get Fam at 1 MHz
         sum=0.
         do j=1,29
            zz=ss(1)*p(j,1,iblk)
            do k=2,15
               zz=zz+ss(k)*p(j,k,iblk)
            end do
            zz=zz+p(j,16,iblk)
            sum=sum+yy(j)*zz
         end do
         atno=sum+abp(1,iblk)+abp(2,iblk)*q

c        Do frequency interpolation

c        Store time block for calculation of Vd
         iblk0=iblk

         if (rclt .gt. 1.570796327) iblk=iblk+6

         x=alogf

         u1=-.75
         pz=fm(1,iblk)
         px=fm(8,iblk)
         do i=2,7
            pz=u1*pz+fm(i  ,iblk)
            px=u1*px+fm(i+7,iblk)
         end do
         cz=atno*(2.-pz)-px
         if (x .ne. 0.) then
            u1=2.**(x+1.)-2.75
            pz=fm(1,iblk)
            px=fm(8,iblk)
            do i=2,7
               pz=u1*pz+fm(i  ,iblk)
               px=u1*px+fm(i+7,iblk)
            end do
         end if
         Fam=cz*pz+px

c        Calculate Vd

c        Adjust index for local season
         iseas=indx
         if (rclt .gt. 1.570796327) then
            if (iseas .lt. 3) then
               iseas=iseas+2
            else
               iseas=iseas-2
            end if
         end if

c        Vd for 200 Hz bandwidth
         Vd200=vdc(1,iblk0,iseas)
         do i=2,5
            Vd200=Vd200*x+vdc(i,iblk0,iseas)
         end do

c        Convert to specified bandwidth
         Vd=Vd200+(0.4679+0.2111*Vd200)*LOG10(bandw/200.)
         if (Vd .lt. 1.049) Vd=1.049

c        sigma Vd
         sigV=svc(1,iblk0,iseas)
         do i=2,5
            sigV=sigV*x+svc(i,iblk0,iseas)
         end do

c        b(1) thru b(8) are du, dl, sigu, sigl, sigm, fam, vd, sigv
         do i=1,5
            if (i .eq. 5 .and. x .gt. 1.) x=1.
            sum=dud(1,iblk,i)
            do j=2,5
               sum=sum*x+dud(j,iblk,i)
            end do
            b(i,khr)=sum
         end do

         b(6,khr)=Fam
         b(7,khr)=Vd
         b(8,khr)=sigV

c        print '(''NTIA:                          hr ,iblk ,Fam,Fa,Vd'',
c    &           28x,f4.0,i3,3f8.3)',
c    &           hr,iblk,atno,b(6,khr),b(7,khr)

         if (hr1 .eq. hr2) then
            khr=9
         else
            khr=khr+1
            hr=hr2
            iblk=iblk2
         end if
      end do

      if (khr .eq. 3) then

c        Interpolate across time blocks
         do i=1,8
            b(i,1)=b(i,1)+(b(i,2)-b(i,1))*(rhr-hr1)/(hr2-hr1)
         end do
      end if

      Du  =b(1,1)
      Dl  =b(2,1)
      sigu=b(3,1)
      sigl=b(4,1)
      sigm=b(5,1)
      Fam =b(6,1)
      Vd  =b(7,1)
      sigV=b(8,1)

c     Parameters for OPA_GRID
      ampn=Fam-95.5+20.*alogf+10.*LOG10(bandw)
      sign=SQRT((Du/1.28)**2+(sigu/1.28)**2+sigm**2)
      RETURN
      END      ! NOISE_NTIA
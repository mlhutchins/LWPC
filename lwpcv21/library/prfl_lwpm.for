      SUBROUTINE PRFL_LWPM
     &          (initialize,print_hgts,
     &           pflag,prfl_id,
     &           freq,month,day,year,UT,dip,zenith,
     &           hpr_mid,beta,hprime,pindex)

c***********************************************************************

c     Processes LWPM profile specification

c     Pflag        Profile specification
c     2            lwpm day
c     3            lwpm night
c     4            lwpm month/day/year hour:minute

c     LWPM model parameters
c     freq_1       first    frequency
c     beta_1       beta's   for first frequency
c     hprime_1     hprime's for first frequency
c     freq_2       second   frequency
c     beta_2       beta's   for second frequency
c     hprime_2     hprime's for second frequency

c     NRTERM is number of segments in terminator boundary
c     NOTE:  Number of segments in polar cap boundary is NRTERM/2

c***********************************************************************

c Change history:

c     05 Mar 97     Modified for tabular specification of beta and
c                   hprime vs solar zenith angle and geomagnetic dip
c                   angle via an LWPM.PRF data file; also appends a
c                   notice of a non-standard profile to the profile id.
c 2/23/10 MLH Redefined deltad again for polar cap transition
c 3/10/10 MLH Removed above, fixed in compile using -fno-automatic option

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (nrterm=5,nrslab=nrterm+2,
     &              nrprfl=nrslab*2-1,mid=nrslab/2+1)

      character*(*) prfl_id
      logical       initialize
      integer       print_hgts,pflag,pindex,
     &              day,year,UT

      common/ prfl$/zzz(41),ddd(41),bbb(41),hhh(41),nrz,nrd

      character* 40 extra_id
      character* 41 string
      logical       flag,standard,default
      integer       str_length,
     &              prev_index
      dimension     day_night(2),
     &              polar_cap(2),
     &              beta_1(2),hprime_1(2),
     &              beta_2(2),hprime_2(2),
     &              beta_f(2),hprime_f(2)

      data          day_night/90.,99./,
     &              polar_cap/70.,74./,
     &              freq_1/10./,beta_1/0.3,0.3/,hprime_1/74.,87./,
     &              freq_2/60./,beta_2/0.3,0.8/,hprime_2/74.,87./,
     &              prev_index/0/


      if (initialize) then
       
      
c        Check for a file named LWPM.PRF in the current directory.
         INQUIRE (file='lwpm.prf',exist=flag)
         prev_index=0
         standard=.true.
         default=.true.
         if (flag) then
c           Cycle through logical unit numbers
            lu=0
            do while (flag)
               lu=lu+1
               INQUIRE (lu,opened=flag)
            end do

            OPEN (lu,file='lwpm.prf',status='old')
            write(lwpcLOG_lun,'(/''Non-Standard profile'')')
            read (lu,'(a)') string
            write(lwpcLOG_lun,'(3x,a)') string
            if (string(1:1) .eq. ';' .and.
     &          STR_LENGTH(string) .gt. 0) then

c              Non-standard or non-default profile id
               extra_id=string(2:)
               read (lu,'(a)') string
               write(lwpcLOG_lun,'(3x,a)') string
            end if
            if (string(:3) .eq. 'chi' .or.
     &          string(:3) .eq. 'dip') then

               standard=.false.
               default=.false.

c              This file contains a table of beta and hprime vs
c              solar zenith angle and geomagnetic dip angle.
               nrz=0
               nrd=0
               nrp=0
               do while (string(:3) .ne. 'end')
                  read (lu,'(a)') string
                  write(lwpcLOG_lun,'(3x,a)') string
                  if (string(:3) .eq. 'chi') then
                     flag=.true.
                  else
     &            if (string(:3) .eq. 'dip') then
                     flag=.false.
                     nmid=nrp+1
                  else
     &            if (string(:3) .ne. 'end') then
                     read (string,*) xd,bb,hh
                     if (flag) then
                        nrz=nrz+1
                        zzz(nrz)=xd
                        if (xd .eq. 90.) nday=nrp
                     else
                        nrd=nrd+1
                        ddd(nrd)=xd
                     end if
                     nrp=nrp+1
                     bbb(nrp)=bb
                     hhh(nrp)=hh
                  end if
               end do
            else

               default=.false.

c              This file contains modifications to the standard LWPM
c              model.  These parameters must be input in the following
c              order:

c                 day_night(2)
c                 polar_cap(2)
c                 freq_1, beta_1(2), hprime_1(2)
c                 freq_2, beta_2(2), hprime_2(2)

c              Note that the input is list directed, so that comments
c              can be inserted after the values to help decipher the
c              numbers in the file.
               read (string,*) day_night
               read (lu,'(a)') string
               write(lwpcLOG_lun,'(3x,a)') string
               read (string,*) polar_cap
               read (lu,'(a)') string
               write(lwpcLOG_lun,'(3x,a)') string
               read (string,*) freq_1,beta_1,hprime_1
               read (lu,'(a)') string
               write(lwpcLOG_lun,'(3x,a)') string
               read (string,*) freq_2,beta_2,hprime_2
            end if
            CLOSE(lu)
         end if
         if (standard) then
c           Standard LWPM model
c           Get BETA,HPRIME profile specification using the LWPM
c           beta_f(1),hprime_f(1)    parameters for day
c           beta_f(2),hprime_f(2)    parameters for night

            slope=(freq-freq_1)/(freq_2-freq_1)
            beta_f(1)=beta_1(1)+slope*(beta_2(1)-beta_1(1))
            hprime_f(1)=hprime_1(1)+slope*(hprime_2(1)-hprime_1(1))
            beta_f(2)=beta_1(2)+slope*(beta_2(2)-beta_1(2))
            hprime_f(2)=hprime_1(2)+slope*(hprime_2(2)-hprime_1(2))

c           Day to night transition
            deltaz=(day_night(2)-day_night(1))/nrterm

c           Change in beta and hprime from day to night
            deltab=(beta_f(2)-beta_f(1))/(nrterm+1)
            deltah=(hprime_f(2)-hprime_f(1))/(nrterm+1)

c           MID is the midpoint of the diurnal segments;
c           used to define the polar cap

c           Polar cap transition
            deltad=(polar_cap(2)-polar_cap(1))/(mid-2)
            dd=polar_cap(1)

c           Build table of beta and hprime vs. solar zenith angle
c           starting with midnight
            za=-180.
            bt=beta  _f(2)
            hp=hprime_f(2)
            nrz=nrprfl
            if (.not.default)
     &      write(lwpcLOG_lun,'(/''Non-Default profile'')')
            do j=1,nrprfl

               if (za .lt. 0.) then
                  zzz(j)=AINT(10.*za-.5)/10.
               else
                  zzz(j)=AINT(10.*za+.5)/10.
               end if
               bbb(j)=AINT(100.*bt+.5)/100.
               hhh(j)=AINT( 10.*hp+.5)/10.
               if (j .gt. 2 .and. j .le. mid) then
                  dd=dd+deltad
               else
     &         if (j .gt. nrprfl-mid+1 .and. j .le. nrprfl-1) then
                  dd=dd-deltad
               end if
               ddd(j)=AINT(10.*dd+.5)/10.
               if (.not.default)
     &         write(lwpcLOG_lun,*) j,zzz(j),ddd(j),bbb(j),hhh(j)

               if (j .eq. 1) then

c                 Begin sunrise transition
                  za=-day_night(2)
                  bt=bt-deltab
                  hp=hp-deltah
               else

                  if (za .lt. -day_night(1)+.1*deltaz) then

c                    Midnight to noon
                     if (ABS(za+day_night(1)) .lt. .1*deltaz) then

c                       Switch to sunset transition
                        za=day_night(1)
                        bt=beta   _f(1)+deltab
                        hp=hprime _f(1)+deltah
                     else

c                       Continue sunrise transition
                        za=za+deltaz
                        bt=bt-deltab
                        hp=hp-deltah
                     end if
                  else

c                    Noon to midnight
                     za=za+deltaz
                     bt=bt+deltab
                     hp=hp+deltah
                  end if
               end if
            end do
         end if

         if (pflag .eq. 2) then

            prfl_id='LWPM Day'
         else
     &   if (pflag .eq. 3) then

            prfl_id='LWPM Night'
         else
     &   if (pflag .eq. 4) then

            write(prfl_id,
     &          '(''LWPM Date: '',2(i2.2,''/''),i2.2,'':'',i4.4)')
     &            month,day,year,UT
         end if
         if (STR_LENGTH(extra_id) .gt. 0)
     &      prfl_id(STR_LENGTH(prfl_id)+2:)=extra_id

c        Set previous index
         prev_index=0
      else

         if (standard) then

c           Standard LWPM profile model

            hpr_mid=hhh(mid)

            if (pflag .eq. 2) then

c              All day
               pindex=nrslab
               beta  =bbb(pindex)
               hprime=hhh(pindex)
            else

               if (pflag .eq. 3) then

c                 All night
                  zn=-180.
                  pindex=nrprfl
               else

c                 Specific date
                  zn=zenith

c                 Find the profile index based on solar zenith angle
                  pindex=1
                  flag=.true.
                  do while (flag)
                     if (zzz(pindex) .le. zn .and.
     &                  zn .lt. zzz(pindex+1)) then
                        flag=.false.
                     else
                        pindex=pindex+1
                        if (pindex .eq. nrprfl) flag=.false.
                     end if
                  end do
               end if
              if (ABS(dip) .ge. polar_cap(2)) then

c                 Inside the polar cap
                  ndx2=mid
               else
     &         if (ABS(dip) .ge. polar_cap(1)) then

c                 In the polar cap transition
                  !deltad=(polar_cap(2)-polar_cap(1))/(mid-2) unneeded

                  ndx2=(ABS(dip)-polar_cap(1))/deltad+2.
               else

c                 Below the polar cap transition
                  ndx2=1
               end if
               if (zn .ge. 0.) ndx2=nrprfl+1-ndx2

c              Choose profile with the lower height
               if (hhh(pindex) .ge. hhh(ndx2)) pindex=ndx2

               beta  =bbb(pindex)
               hprime=hhh(pindex)
            end if
         else
c           Non-standard profile model
            hpr_mid=hhh(nmid)

            if (pflag .eq. 2) then

c              All day
               pindex=nday
               beta  =bbb(pindex)
               hprime=hhh(pindex)
            else

               if (pflag .eq. 3) then

c                 All night
                  zn=-180.
                  pindex=nrz
               else

c                 Specific date
                  zn=zenith

c                 Find the profile index based on solar zenith angle
                  pindex=1
                  flag=.true.
                  do while (flag)
                     if (zzz(pindex) .le. zn .and.
     &                  zn .lt. zzz(pindex+1)) then
                        flag=.false.
                     else
                        pindex=pindex+1
                        if (pindex .eq. nrprfl) flag=.false.
                     end if
                  end do
               end if

               if (ABS(dip) .ge. ddd(1)) then

c                 In the polar cap transition
                  if (ABS(dip) .ge. ddd(nrd)) then

c                    Inside the polar cap
                     ndx2=nrd
                  else

                     ndx2=1
                     flag=.true.
                     do while (flag)
                        if (ddd(ndx2) .le. ABS(dip) .and.
     &                     ABS(dip) .lt. ddd(ndx2+1)) then
                           flag=.false.
                        else
                           ndx2=ndx2+1
                           if (ndx2 .eq. nrd) flag=.false.
                        end if
                     end do
                  end if

c                 Choose profile with the lower height
                  if (hhh(pindex) .ge. hhh(nmid+ndx2-1))
     &               pindex=nmid+ndx2-1
               end if

c              Set profile values
               beta  =bbb(pindex)
               hprime=hhh(pindex)

            end if
         end if
         if (pindex .ne. prev_index) then

c           Set profile
            call PRFL_EXP (beta,hprime)

c           Get reference heights for current profile.
            call PRFL_HGTS (print_hgts)

c           Set previous index
            prev_index=pindex
         end if
      end if

      RETURN
      END      ! PRFL_LWPM
c$pragma aux Sys_screen_begin "*_"
c$pragma aux Sys_print_begin "*_"

      SUBROUTINE GRF_BEGIN
     &          (device_type,orientation)

c***********************************************************************

c  Function:
c     Initialize plotting.

c  Parameters passed:
c     device_type   [s] device type: SYS-SCN;   System screen
c                                    SYS-PRN;   System printer
c                                    FILE-NAME; HPGL to a file
c
c     orientation   [s] LANDSCAPE or PORTRAIT

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     close
c     open
c     read
c     write

c     hpgl_begin
c     sys_screen_begin
c     sys_print_begin

c     grf_color

c     decode_list_str

c     lwpc_error

c     str_length
c     str_lower
c     str_trim_char

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

c     LWPC parameters
      include      'graphics.cmn'
      include      'lwpc_cfg.cmn'
      include      'lwpc_lun.cmn'

      character*(*) device_type
      character*(*) orientation

      character* 12 list(5),orient
      character*200 string,error_msg
      logical       exists,first
      integer       str_length

      data          first/.true./

      if (first) then

c        Check for local GRAPHICS.INI
         string='graphics.ini'
         INQUIRE (file=string,exist=exists)
         if (.not.exists)
c           Use the one in lwpcDAT_loc
     &      string=lwpcDAT_loc(:STR_LENGTH(lwpcDAT_loc))//'graphics.ini'
         OPEN (lwpcDAT_lun,file=string,status='old',err=900)

         graphics_NrColors=0

c        Get control string
         do while (first)
            read (lwpcDAT_lun,'(a)',end=19) string
            call STR_LOWER (string,0,0)
            if (string(:1) .eq. 'c') then

c              Define color list using list of names and RGBs.
c              This list is of the form: color-name rrr ggg bbb
c              where color-names may not include embedded blanks;
c              after the RGB sequence is the equivalent pen number.
c              The list is terminated by "end" starting in column 1.
               n=0
               do while (string(:3) .ne. 'end')
                  read (lwpcDAT_lun,'(a)') string
                  call STR_LOWER (string,0,0)
                  if (string(:3) .ne. 'end') then
                     n=n+1
                     if (n .gt. MxColors) then
                        write(error_msg,
     &                      '(''[GRF_BEGIN]: '',
     &                        ''Too many colors: '',i3)')
     &                          MxColors
                        call LWPC_ERROR ('ERROR',error_msg)
                     end if
                     call DECODE_LIST_STR (string,5,nl,list)
c                    Ensure that the RGB sequence has no blanks
                     do l=2,4
                        do m=1,3
                           if (list(l)(m:m) .eq. ' ') list(l)(m:m)='0'
                        end do
                     end do
c                    Store color name
                     graphics_ColorList(1,n)=list(1)
c                    Store RGB sequence
                     write(graphics_ColorList(2,n),'(2(a3,'',''),a3)')
     &                     list(2),list(3),list(4)
c                    Store pen number
                     read (list(5),*) graphics_ColorPen(n)
                  end if
               end do
               graphics_NrColors=n
            else
     &      if (string(:1) .eq. 'f') then

c              Define fill list using list of names and fills.
c              This list is of the form: FILLxx fill-name
c              where xx goes from 01 to a maximum of 16, inclusive.
c              The list is terminated by "end" starting in column 1.
               n=0
               do while (string(:3) .ne. 'end')
                  read (lwpcDAT_lun,'(a)') string
                  call STR_LOWER (string,0,0)
                  if (string(:3) .ne. 'end') then
                     n=n+1
                     if (n .gt. MxFills) then
                        write(error_msg,
     &                      '(''[GRF_BEGIN]: '',
     &                        ''Too many fills: '',i3)')
     &                          MxFills
                        call LWPC_ERROR ('ERROR',error_msg)
                     end if
                     call DECODE_LIST_STR (string,5,nl,list)
                     graphics_FillList(1,n)=list(1)
                     graphics_FillList(2,n)=list(2)
                  end if
               end do
               graphics_NrFills=n
            end if
         end do
19       CLOSE(lwpcDAT_lun)
         first=.false.
      end if

c     Configure input parameters.
      graphics_device=device_type
      call STR_TRIM_CHAR (graphics_device,' ',nrchar,'Leading_Blanks')
      call STR_LOWER (graphics_device,1,nrchar)

      orient=orientation
      call STR_TRIM_CHAR (orient,' ',nrchar,'Leading_Blanks')
      call STR_LOWER (orient,1,nrchar)

c     Initialize specific device
      if (graphics_device(:3) .eq. 'sys') then

c        Set up system output;
c        LogicalUnit has been defined by system graphics driver
         if (graphics_device(:5) .eq. 'sys-p' .or.
     &       graphics_device(:5) .eq. 'sys_p') then

c           Set landscape flag
            if (orient(:1) .eq. 'p') then

               graphics_landscape=.false.
            else

               graphics_landscape=.true.
            end if

            call SYS_PRINT_BEGIN
         else

            call SYS_SCREEN_BEGIN
         end if
      else

c        Write graphical output to a file

c        Set logical unit
         graphics_LogicalUnit=lwpcPLT_lun

c        Open output file
         OPEN (graphics_LogicalUnit,file=device_type,status='unknown')

c        Set landscape flag
         if (orient(:1) .eq. 'p') then

            graphics_landscape=.false.
         else

            graphics_landscape=.true.
         end if

c        Set up HPGL plotters
         call HPGL_BEGIN
      end if

c     Set logical origin
      graphics_origin(1)=0.
      graphics_origin(2)=0.

c     Set position
      graphics_position(1)=0.
      graphics_position(2)=0.

c     Set pen status
      graphics_pen='up'

c     Set initial color
      graphics_color(1)='none'
      call GRF_COLOR('labels')

c     Set fill parameters
      graphics_fillname='none'
      graphics_fillindx=-99
      RETURN

c     ERROR Handling Routines
900   write(error_msg,
     &    '(''[GRF_BEGIN]: '',
     &      ''Fatal error trying to open: '',a)')
     &        string(:STR_LENGTH(string))
      call LWPC_ERROR ('ERROR',error_msg)

      END      ! GRF_BEGIN
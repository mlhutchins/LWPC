      SUBROUTINE CLOSE_FORDRIVE

c     Closes the plot driver subroutine depending on the output device

      include      'graphics.cmn'


      if (graphics_device(:3) .ne. 'sys') then

c        Not SYS-SCN; end the program
         STOP
      else
     &if (graphics_device(:5) .eq. 'sys-p' .or.
     &    graphics_device(:5) .eq. 'sys_p') then

c        Is SYS-PRN; end the program
         STOP
      else

c        Is SYS-SCN;
c        keep the program running so the plot stays on the screen
         RETURN
      end if
      END      ! CLOSE_FORDRIVE
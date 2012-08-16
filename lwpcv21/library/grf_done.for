c$pragma aux Sys_print_done "*_"

      SUBROUTINE GRF_DONE

c***********************************************************************

c  Function:
c     Terminate plot

c  Parameters passed:

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     inquire

c     hpgl_done
c     sys_print_done

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      logical       open

      if (graphics_device(1:5) .eq. 'sys-p'.or.
     &    graphics_device(1:5) .eq. 'sys_p') then

         call SYS_PRINT_DONE
         RETURN
      else
     &if (graphics_device(1:3) .eq. 'sys') then

         RETURN
      else

c        Check unit status:
         INQUIRE (graphics_LogicalUnit,opened=open)

         if (open) call HPGL_DONE
      end if

      RETURN
      END      ! GRF_DONE
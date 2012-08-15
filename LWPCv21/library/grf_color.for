c$pragma aux Sys_color "*_" parm (value)

      SUBROUTINE GRF_COLOR
     &          (clr_rgb)

c***********************************************************************

c  Function:
c     Determines the color index and then calls the appropriate device
c     color routine

c  Parameters passed:
c     clr_rgb       [s] color name or RGB as "RRR,GGG,BBB"

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     index

c     decode_lst_int
c     str_length
c     str_lower
c     str_trim_char

c     hpgl_color
c     sys_color

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      character*(*) clr_rgb

      character* 12 xclr_rgb
      character* 12 oldColor(2)
      character*200 error_msg
      logical       match
      integer       ColorIndex,rgb(3),
     &              oldColorNdx

      integer       sys_color,
     &              str_length

c     Default color
      data          oldColorNdx/-9/

c     Determine if color name or rgb value specified
      xclr_rgb=clr_rgb
      nl=STR_LENGTH(xclr_rgb)
      if (nl .eq. 0) then

c        End of graph
         ColorIndex=0
      else

c        Trim leading spaces from the color name
         call STR_TRIM_CHAR (xclr_rgb,' ',nl,'Leading_Blanks')

c        Convert to lower case
         call STR_LOWER (xclr_rgb,1,nl)

         if (xclr_rgb .eq. 'store') then

c           Store current color and exit
            oldColorNdx=ColorIndex
            oldColor(1)=graphics_Color(1)
            oldColor(2)=graphics_Color(2)
            RETURN
         else
     &   if (xclr_rgb .eq. 'reset') then

            if (oldColorNdx .eq. -9) then

c              No stored color; use first one in the list
               ColorIndex       =1
               graphics_Color(1)=graphics_ColorList(1,1)
               graphics_Color(2)=graphics_ColorList(2,1)
            else

c              Restore previous color
               ColorIndex       =oldColorNdx
               graphics_Color(1)=oldColor(1)
               graphics_Color(2)=oldColor(2)
            end if
         else
     &   if (INDEX(xclr_rgb,',') .eq. 0) then

c           Name of color specified

c           If color is same as current, do nothing.
            nl=STR_LENGTH (graphics_Color(1))
            if (xclr_rgb(:nl) .eq. graphics_Color(1)(:nl)) RETURN

c           Set requested color
            nc=1
            match=.false.
            do while (nc .le. graphics_NrColors .and. .not.match)
               nl=STR_LENGTH (graphics_ColorList(1,nc))
               if (xclr_rgb(:nl) .eq.
     &            graphics_ColorList(1,nc)(:nl)) then

                  graphics_Color(1)=graphics_ColorList(1,nc)
                  graphics_Color(2)=graphics_ColorList(2,nc)
                  if (STR_LENGTH(graphics_device) .eq. 3 .and.
     &                graphics_device(:3) .eq. 'sys') then

c                    Output to system device;
c                    set index in the color list
                     ColorIndex=nc
                  else
     &            if (graphics_device(:4) .eq. 'sys-' .or.
     &                graphics_device(:4) .eq. 'sys_') then

c                    Output to system device;
c                    set index in the color list
                     ColorIndex=nc
                  else

c                    Output is to a file;
c                    set equivalent pen number
                     ColorIndex=graphics_ColorPen(nc)
                  end if
                  match=.true.
               else
                  nc=nc+1
               end if
            end do
            if (.not.match) then

               call STR_UPPER (clr_rgb,0,0)
               write(error_msg,
     &             '(''[GRF_COLOR]: '',
     &               ''Requested color ('',a,'') '',
     &               ''not found in the color list'')')
     &                 clr_rgb(:STR_LENGTH(clr_rgb))
               call LWPC_ERROR ('WARNING',error_msg)

               ColorIndex=1
               graphics_Color(1)=graphics_ColorList(1,1)
               graphics_Color(2)=graphics_ColorList(2,1)
            end if
         else

c           RGB specified

c           If color is same as current, do nothing.
            nl=STR_LENGTH (graphics_Color(2))
            if (xclr_rgb(:nl) .eq. graphics_Color(2)(:nl)) RETURN

c           Set requested color
            graphics_Color(1)='rgb'
            graphics_Color(2)=xclr_rgb
            if (graphics_device(:3) .ne. 'sys') then

c              Set first pen; the RGB color will be ignored
               ColorIndex=1
            else

c              Set index to the last color/pen
               ColorIndex=graphics_NrColors
            end if
         end if
      end if

c     Change color
      if (graphics_device(:3) .eq. 'sys') then

         call DECODE_LIST_INT (graphics_Color(2),3,nl,rgb)
         iresult=SYS_COLOR (rgb(1),rgb(2),rgb(3))
      else

         call HPGL_COLOR (ColorIndex)
      end if

      RETURN
      END      ! GRF_COLOR
      SUBROUTINE HPGL_STRING
     &          (xx,yy,height,string,angle,justify,retX)

c***********************************************************************

c  Function:
c     Draws a character string to HPGL device.

c  Parameters passed:
c     xx            [r] x position of first character
c     yy            [r] y position of first character
c     height        [r] height of the characters in inches
c     string        [s] the character string to be drawn
c     angle         [r] angle of the string in degrees relative to
c                       the horizontal
c     justify       [s] 2 character justification string
c                       1st  Action relative to XX
c                       C    Center
c                       R    Right justify
c                       L    Left  justify
c
c                       2nd  Action relative to YY
c                       C    Center
c                       T    Top    justify
c                       B    Bottom justify
c
c                       Example: CT puts the string with its top at YY
c                                centered horizontally at XX.

c  Parameters returned:
c      retX         [r] length of output string (in inches)

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     cos
c     sin

c     str_length

c  Entry:

c  References:

c  Change History:
c 2/16/10 character*1 etx\z03\ changed to *3 etx with parameter etx='z03' added

c*******************!***************************************************

      include      'graphics.cmn'

      character*  2 justify
      character*(*) string

      character*  3 etx
      character*245 out_string
      integer       str_length
      parameter     (etx='z03')
      data          dtr/.01745329252/

c     Count the number of characters in the string
      nr_characters=STR_LENGTH(string)
      if (nr_characters .eq. 0) RETURN

c     Set rotation factor
      if (angle .ne. 0.)
     &   write(graphics_LogicalUnit,
     &       '('' di'',f6.3,f7.3,'';'')')
     &         COS(angle*dtr),
     &         SIN(angle*dtr)

c     Set width (including width to height ratio)
      width=height

c     Establish character sizes (in cm)
c     NOTE: The factor of 1.5 allows for the inter-character space
      ch=2.54*height
      cw=2.54*width/1.5
      write(graphics_LogicalUnit,
     &    '('' si'',f6.3,f7.3,'';'')') cw,ch

c     Reference point
      x0=graphics_origin(1)+graphics_units(1)*xx
      y0=graphics_origin(2)+graphics_units(2)*yy

c     Length and height of the character string
      wth=graphics_units(1)*width *nr_characters
      hgt=graphics_units(2)*height

c     Length and angle of diagonal through the character string
      diag=SQRT(wth**2+hgt**2)
      gamma=ATAN(hgt/wth)/dtr

c     Adjust location of text based on requested justification
      if (justify(1:1) .eq. 'C' .or. justify(1:1) .eq. 'c') then
         x0=x0-.5*diag*COS((angle-gamma)*dtr)+   hgt*SIN(angle*dtr)
      end if
c     if (justify(1:1) .eq. 'L' .or. justify(1:1) .eq. 'l') then
c        Do nothing
c     end if
      if (justify(1:1) .eq. 'R' .or. justify(1:1) .eq. 'r') then
         x0=x0-   diag*COS((angle-gamma)*dtr)+   hgt*SIN(angle*dtr)
      end if

      if (justify(2:2) .eq. 'C' .or. justify(2:2) .eq. 'c') then
         y0=y0-.5*diag*SIN((angle-gamma)*dtr)-   hgt*COS(angle*dtr)
      end if
c     if (justify(2:2) .eq. 'B' .or. justify(2:2) .eq. 'b') then
c        Do nothing
c     end if
      if (justify(2:2) .eq. 'T' .or. justify(2:2) .eq. 't') then
         y0=y0-   diag*SIN((angle-gamma)*dtr)-2.*hgt*COS(angle*dtr)
      end if

c     Move to the beginning of the string
      write(graphics_LogicalUnit,
     &    '('' pu'',2i6,'';'')') INT(x0+.5),INT(y0+.5)

c     Write the string
      out_string='lb'//string(:nr_characters)//etx
      write(graphics_LogicalUnit,
     &    '(1x,a)') out_string

      if (angle .ne. 0.) then

c        Restore to default angle
         write(graphics_LogicalUnit,
     &       '('' di1,0;'')')
      end if

c     Return the length of the string in inches
      retX=width*nr_characters

      RETURN
      END      ! HPGL_STRING
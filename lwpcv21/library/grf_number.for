      SUBROUTINE GRF_NUMBER
     &          (xx,yy,height,value,angle,nr_decimals,justify,retX)

c***********************************************************************

c  Function:
c     Plots numbers

c        For nr_decimals > 0
c           generate a number with specified number of decimal places
c        For nr_decimals = 0
c           generate a whole number value with decimal point
c        For nr_decimals = -1
c           generate an integer value without decimal point
c        For nr_decimals < -1
c           generate and integer scaled by 10**(nr_decimals+1)

c  parameters passed:
c     xx            [r] x position of first character
c     yy            [r] y position of first character
c     height        [r] height of the characters in inches
c     value         [r] value of floating point number to be plotted
c     angle         [r] angle of the number in degrees relative to
c                       the horizontal
c     nr_decimals   [i] number of characters to be plotted
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

c  Functions and subroutines referenced:
c     grf_string

c     write

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      character*  8 frmt
      character* 41 string

c     Create a character string using the floating point number
      if (nr_decimals .ge. 0) then

         write(frmt,'(''(f40.'',i1,'')'')') nr_decimals
         write(string,frmt) value
      else

         inum=value
         if (nr_decimals .ne. -1)
     &      inum=inum*(10.**(nr_decimals+1))
         frmt='(i40)'
         write(string,frmt) inum
      end if

c     Find the first non-blank character in the formatted number
      ichr1=1
      do while (string(ichr1:ichr1) .eq. ' ')
         ichr1=ichr1+1
      end do

      call GRF_STRING
     &    (xx,yy,height,string(ichr1:41),angle,justify,retX)

      RETURN
      END      ! GRF_NUMBER
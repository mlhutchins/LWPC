      SUBROUTINE GRF_FILL_INDEX
     &          (fill_name,fill_indx)

c***********************************************************************

c  Function:
c     Determines the fill index given the specified fill name

c  Parameters passed:
c     fill_name     [s] fill pattern ( % shading )
c     fill_indx     [i] fill index

c        Index  Name           Pattern
c           0   100%           100  (solid)
c           1    99%            99
c           2    80%            80
c           3    55%            55
c           4    35%            35
c           5    20%            20
c           6    10%            10
c           7     2%             2
c           8     0%             0  (blank)
c           9   vertical       lines
c          10   horizontal     lines
c          11   left diagonal  lines
c          12   right diagonal lines

c               Aliases
c           0   solid
c           8   blank

c  Parameters returned:

c  Common blocks referenced:
c     graphics

c  Functions and subroutines referenced:
c     ichar

c     decode_list_int
c     str_length
c     str_lower
c     str_trim_char

c  Entry:

c  References:

c  Change History:

c*******************!***************************************************

      include      'graphics.cmn'

      character*(*) fill_name
      integer       fill_indx

      character*  8 xfill
      integer       str_length

c     Determine the fill pattern
      xfill=fill_name
      n=STR_LENGTH(xfill)
      call STR_LOWER (xfill,1,n)
      if (n .eq. 0) then
c        Fill not defined; use no fill
         fill_indx=8
         RETURN
      end if
      if (xfill(1:4) .eq. 'fill') then
c        Select fill type from the GRAPHICS.INI list
         read (xfill,'(4x,i2)') nf
         xfill=graphics_FillList(2,nf)
         n=STR_LENGTH(xfill)
         call STR_LOWER (xfill,1,n)
      end if
      if (xfill(n:n) .eq. '%') then
         xfill(n:n)=' '
         n=n-1
      end if

c     Trim leading spaces from the fill name
      call STR_TRIM_CHAR (xfill,' ',n,'LEADING_BLANKS')

c     Determine the fill index
      if (ICHAR(xfill(1:1)) .lt. 48 .or. ICHAR(xfill(1:1)) .gt. 57) then

c        Not a digit; must be a named pattern.
         if (xfill(1:1) .eq. 'b') then
c           Blank
            fill_indx=8
         else
     &   if (xfill(1:1) .eq. 's') then
c           Solid
            fill_indx=0
         else
     &   if (xfill(1:1) .eq. 'v') then
c           Vertical lines
            fill_indx=9
         else
     &   if (xfill(1:1) .eq. 'h') then
c           Horizontal lines
            fill_indx=10
         else
     &   if (xfill(1:1) .eq. 'l') then
c           Left diagonal lines (///)
            fill_indx=11
         else
     &   if (xfill(1:1) .eq. 'r') then
c           Right diagonal lines (\\\)
            fill_indx=12
         else
c           Undefined; use solid
            fill_indx=0
         end if
      else

c        A digit; must be a % shading value.
         call DECODE_LIST_INT (xfill,1,n,ndx)
         if (ndx .eq. 0) then
c           0%
            fill_indx=8
         else
     &   if (ndx .le. 2) then
c           2%
            fill_indx=7
         else
     &   if (ndx .le. 10) then
c           10%
            fill_indx=6
         else
     &   if (ndx .le. 20) then
c           20%
            fill_indx=5
         else
     &   if (ndx .le. 35) then
c           35%
            fill_indx=4
         else
     &   if (ndx .le. 55) then
c           55%
            fill_indx=3
         else
     &   if (ndx .le. 80) then
c           80%
            fill_indx=2
         else
     &   if (ndx .le. 99) then
c           99%
            fill_indx=1
         else
c           100%
            fill_indx=0
         end if
      end if

      RETURN
      END      ! GRF_FILL_INDEX
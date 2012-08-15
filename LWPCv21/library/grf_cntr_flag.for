      LOGICAL FUNCTION GRF_CNTR_FLAG
     &                (cells,nrx,set_cell,i,j,edge)

c***********************************************************************

c  Function:
c     Put a one in the array CELLS at the position corresponding to EDGE
c     in the cell at I,J if SET_CELL is TRUE; otherwise, return TRUE or
c     FALSE if the bit is set or not. The array is dimensioned for two
c     contour lines to allow tracing the upper and lower boundaries of
c     the filled area independently.

c  Parameters passed:
c     parameter    [t,n]         description {t is type, n is dimension}
c     cells        [i,*,2]       work array
c     nrx          [i]           number of elements in the data array
c                                along the X axis
c     set_cell     [l]           flag which determines the action to be
c                                taken (see Function above)
c     i            [i]           index of cell along the X axis
c     j            [i]           index of cell along the Y axis
c     edge         [i]           number of the edge of the cell (they
c                                are numbered counterclockwise starting
c                                with 1 at the bottom)

c  Parameters returned:
c     parameter    [t,n]         description {t is type, n is dimension}
c     grf_cntr_flag[l]           =F: if the contour line has not yet
c                                crossed this edge; =T: if the contour
c                                line has already crossed this edge

c  Common blocks referenced:

c  Functions and subroutines referenced:
c     iand
c     ior

c  References:

c  Change History:
c 2/16/10 MLH changed data statement to proper hexedecimal and shortened
c             lines to 73 characters
c*******************!***************************************************

      IMPLICIT      NONE

      logical       set_cell

      integer       cells(1),
     &              bits_per_word,
     &              bits_to_cell,
     &              bit_position,
     &              edge,
     &              i,
     &              iand,              ! Function
     &              ior,               ! Function
     &              j,
     &              mask(32),
     &              nrx,
     &              result,
     &              words_to_cell
      parameter    (bits_per_word=32)

      data         mask/z'80000000',z'40000000',z'20000000',z'10000000',
     &                  z'08000000',z'04000000',z'02000000',z'01000000',
     &                  z'00800000',z'00400000',z'00200000',z'00100000',
     &                  z'00080000',z'00040000',z'00020000',z'00010000',
     &                  z'00008000',z'00004000',z'00002000',z'00001000',
     &                  z'00000800',z'00000400',z'00000200',z'00000100',
     &                  z'00000080',z'00000040',z'00000020',z'00000010',
     &                  z'00000008',z'00000004',z'00000002',z'00000001'/

c     Number of bits to the specified cell
      bits_to_cell=(i+(j-1)*nrx)*4

c     Number of words to the specified cell
      words_to_cell=bits_to_cell/bits_per_word+1

c     Number of bits from the beginning of the word
c     to the beginning of the cell
      bit_position=bits_to_cell-(words_to_cell-1)*bits_per_word

c     Check if this is a test or setting operation
      if (set_cell) then

c        Set the bit for the specified edge of the cell
         result=IOR (cells(words_to_cell),mask(bit_position+edge))

         cells(words_to_cell)=result

         grf_cntr_flag=.true.
      else

c        Find out if the bit is set
         result=IAND(cells(words_to_cell),mask(bit_position+edge))

         if (result .eq. 0) then
            grf_cntr_flag=.false.
         else
            grf_cntr_flag=.true.
         end if
      end if

      RETURN
      END      ! GRF_CNTR_FLAG
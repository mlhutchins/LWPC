      SUBROUTINE PLT_RX_MODEL
     &          (rx_model,rcomp,
     &           snr,ampn,ampi,ampe,vsdn,mxlat,nrlat,mxlon,nrlon)

c***********************************************************************
c  Subroutine: plt_rx_model
c***********************************************************************
c
c  Program Source:  Naval Ocean Systems Center - Code 882
c
c  Date:
c     23 Jun 1997
c
c  Function:
c     Adjusts the SNR by applying the NRC adjustment as described in the
c     references.
c
c     NOTE: This routine operates only on the vertical component.
c
c  Parameters passed:
c     rx_model       [s]   name of receiver model
c     rcomp          [i]   index of the signal component (1: Z; 2: Y)
c     snr            [r]   signal to noise ratio
c     ampn           [r]   amplitude of atmospheric noise
c     ampi           [r]   amplitude of antenna internal noise
c     ampe           [r]   amplitude of emi
c     vsdn           [r]   V sub d of the noise
c     mxlon,         [i]   dimension of the arrays
c     mxlat
c     nrlon,         [i]   number of elements in the arrays
c     nrlat
c
c  Parameters returned:
c     snr            [r]   adjusted signal to noise ratio
c
c  Common blocks referenced:
c
c  Functions and subroutines referenced:
c     log10
c     sqrt
c
c     lwpc_error
c     str_lower
c
c  References:
c     PSR model:
c        PSR Report 2380, Improved Methods for VLF/LF Coverage Prediction,
c        31 Aug 1993.
c
c     MITRE model:
c        MITRE Report: Submarine EMI VLF Communications Model,
c        C.M.Smith, W.A.Finn, D.W.Sharp, 30 Sep 1996.
c
c  Change History:
c
c*******************!***************************************************

      character*(*) rx_model
      integer       rcomp
      real          snr (mxlon,mxlat  ),
     &              ampn(mxlon,mxlat,2),
     &              vsdn(mxlon,mxlat,2),
     &              ampi,ampe

      character*  4 model
      character*200 error_msg
      real          nrc

      model=rx_model
      call STR_UPPER (model,0,0)
      if (model(:3) .eq. 'PSR') then

c        PSR (Buckner) model
         do nlat=1,nrlat
            do nlon=1,nrlon

               if (vsdn(nlon,nlat,rcomp) .lt. 4.) then

                  nrc=7.
               else
     &         if (vsdn(nlon,nlat,rcomp) .lt. 10.) then

                  nrc=11.7-1.17*vsdn(nlon,nlat,rcomp)
               else

                  nrc=8.25-.825*vsdn(nlon,nlat,rcomp)
               end if
               snr(nlon,nlat)=snr(nlon,nlat)-nrc
            end do
         end do
      else
     &if (model(:4) .eq. 'MITR') then

c        MITRE (Smith) model
         esq=10.**(ampi/10.)+10.**(ampe/10.)

         do nlat=1,nrlat
            do nlon=1,nrlon

               ratio=10.**(ampn(nlon,nlat,rcomp)/10.)/esq
               ampv =10.**(vsdn(nlon,nlat,rcomp)/20.)

c              Adjust Vsubd
               adj=20.*LOG10(1.+ratio*(ampv**2-1.)
     &                       /(ampv+SQRT(ratio))**2)

               nrc=.0667*(10.-adj)

               snr(nlon,nlat)=snr(nlon,nlat)-nrc
            end do
         end do
      else

c        Invalid model specified
         write(error_msg,
     &       '(''[PLT_RX_MODEL]: '',
     &         ''Invalid receiver model:  '',a)') rx_model
         call LWPC_ERROR('ERROR',error_msg)
      end if
      RETURN
      END      ! PLT_RX_MODEL
      SUBROUTINE SW_NEXT
     &          (pflag,nsgmnt0,nsgmnt)

c     Locates the next segment which matches the one just done

c pflag          profile index flag
c nsgmnt0        index of starting segment
c nsgmnt         index of current segment

      parameter     (mxsgmnt=201)

      common/mf_sw_1/dst(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &               azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &               sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &               bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &               num(mxsgmnt),nrsgmnt

      logical        end loop
      integer        pflag


      end loop=.false.
      do while (.not.end loop)
         nsgmnt=nsgmnt+1
         if (nsgmnt .gt. nrsgmnt) then

            end loop=.true.
         else

            if (sgm(nsgmnt) .eq. sgm(nsgmnt0) .and.
     &          num(nsgmnt) .eq. 0) then

               if (pflag .gt. 1) then

                  if (pflag .lt. 5) then

                     if (hpr(nsgmnt) .eq. hpr(nsgmnt0)) end loop=.true.
                  else

                     if (npr(nsgmnt) .eq. npr(nsgmnt0)) end loop=.true.
                  end if
               else

                  end loop=.true.
               end if
            end if
         end if
      end do

      RETURN
      END      ! SW_NEXT
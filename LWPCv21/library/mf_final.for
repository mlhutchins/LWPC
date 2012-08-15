      SUBROUTINE MF_FINAL

c***********************************************************************

c     This routine provides for iterative refinement of eigen values
c     and removes redundant solutions.

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxeigen=50,mxeigenb=25,mxboxes=200)

      integer        rprnt,xprnt
      real           lub
      complex        eigen,eigenb,f,dfdt,delt,udfdt(mxeigenb)

      common/lwpc_mf/ranger(2),rangei(2),atnmax,lub,h,
     &               eigen(mxeigen),nreigen

     &      /mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_eigb/eigenb(mxeigenb),nreigenb
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      data           maxntn/10/


      nofinl=0
      if (nreigenb .eq. 0) RETURN

c     Iterative refinement of solutions
      delmax=adjmsh/float(maxntn)
      if (delmax .lt. 1.1*lub) delmax=1.1*lub

      do j=1,nreigenb
         if (mrprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_FINAL: '',i3)')
     &              j
         nrntn=0
         last=0
         do while (last .eq. 0)
            call MF_FDFDT (eigenb(j),f,dfdt)
            udfdt(j)=dfdt/ABS(dfdt)
            delt=-f/dfdt
            if (mrprnt .ne. 0)
     &         write(lwpcLOG_lun,
     &             '(''MF_FINAL: '',2f7.3,1p4e12.3)')
     &                 eigenb(j),udfdt(j),delt
            if (ABS(delt) .gt. delmax) delt=delt*delmax/ABS(delt)
            eigenb(j)=eigenb(j)+delt
            nrntn=nrntn+1
            if (nrntn .gt. maxntn) then
               nofinl=1
               RETURN
            end if
            if (ABS(delt) .lt. lub) last=1
         end do
         call MF_FDFDT (eigenb(j),f,dfdt)
         udfdt(j)=dfdt/ABS(dfdt)
         delt=-f/dfdt
         if (mrprnt .ne. 0)
     &      write(lwpcLOG_lun,
     &          '(''MF_FINAL: '',2f7.3,1p4e12.3)')
     &              eigenb(j),udfdt(j),delt
         eigenb(j)=eigenb(j)+delt
      end do

      if (nreigenb .gt. 1) then

c        Test for redundant solutions
         atol=0.1*adjmsh
         do j=1,nreigenb-1
            do jj=j+1,nreigenb
               if (ABS(eigenb(j)-eigenb(jj)) .lt. atol .and.
     &             ABS(udfdt(j)-udfdt(jj)) .lt. 1.4) then
                  nofinl=1
                  RETURN
               end if
            end do
         end do
      end if
      RETURN
      END      ! MF_FINAL
      SUBROUTINE MF_SUBDIV

c***********************************************************************

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter     (mxboxes=200)

      integer        rprnt,xprnt

      common/mf_boxs/adjmsh,wdchng,
     &               unitr,nur(mxboxes),boxlft(mxboxes),boxrht(mxboxes),
     &               uniti,nui(mxboxes),boxtop(mxboxes),boxbot(mxboxes),
     &               nrboxs,kb
     &      /mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_prnt/lprnt,lgprnt,mprnt,mrprnt,msprnt,mzprnt,rprnt,xprnt

      data           minbox/2/


      nodivd=0

      if (nur(kb) .le. minbox .and. nui(kb) .le. minbox) then
c         write(lwpcLOG_lun,
c     &       '(''MF_SUBDIV: Box too small'')')
c     MLH Error reporting removed for integration into MATLAB
         nodivd=1
         RETURN
      end if

      if (nrboxs .eq. mxboxes) then
c         write(lwpcLOG_lun,
c     &       '(''MF_SUBDIV: Too many boxes'')')
c     MLH Error reporting removed for integration into MATLAB
         nodivd=2
         RETURN
      end if

      nrboxs=nrboxs+1

      do k=nrboxs,kb+1,-1
         nur   (k)=nur   (k-1)
         nui   (k)=nui   (k-1)
         boxlft(k)=boxlft(k-1)
         boxrht(k)=boxrht(k-1)
         boxtop(k)=boxtop(k-1)
         boxbot(k)=boxbot(k-1)
      end do

      if (nui(kb) .le. nur(kb)) then
         nusav=nur(kb)
         nur   (kb  )=(nusav-1)/2+1
         nur   (kb+1)= nusav-nur(kb)
         boxlft(kb  )=boxrht(kb)-nur(kb)*unitr
         boxrht(kb+1)=boxlft(kb)
      else
         nusav=nui(kb)
         nui   (kb  )=(nusav-1)/2+1
         nui   (kb+1)= nusav-nui(kb)
         boxbot(kb  )=boxtop(kb)-nui(kb)*uniti
         boxtop(kb+1)=boxbot(kb)
      end if
      RETURN
      END      ! MF_SUBDIV
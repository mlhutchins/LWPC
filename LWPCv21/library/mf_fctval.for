      SUBROUTINE MF_FCTVAL (thetaz,f)

c***********************************************************************

c     Routine for computing F-function values wrt THETA at arbitrary
c     values of THETA.  Called on principally by routine MF_FZEROS.
c     This is the modified F-function which has no poles and which has
c     no zeros at THETA=(90,0).

c  Change History:
c     26 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c*******************!***************************************************

      implicit complex (a-h,o-z)

c     LWPC parameters
      include      'lwpc_lun.cmn'

      real           omega,wavenr
      complex        num11,num22

      common/mf_flag/iexact,iovflo,kexact,lowg,nodivd,noevct,
     &               nofinl,nointg,nomesh,notlin
     &      /mf_mode/theta,c,s,csq,ssq,omega,wavenr,ideriv
     &      /mf_rbrs/num11,num22,den11,den22,dnum11,dnum22,dden11,
     &               dden22,f1,f2,hg
     &      /mf_rntg/r11,r21,r12,r22,dr11,dr21,dr12,dr22,drdh(8),
     &               roe(8),rnum11,rnum21,rnum12,rnum22,rden


      theta=thetaz
cxx      write(lwpcLOG_lun,
cxx     &    '(''MF_FCTVAL: theta, noevct'',2f10.3,i3)')
cxx     &        theta,noevct
      ideriv=0
      call mf_r bars
cxx      write(lwpcLOG_lun,
cxx     &    '(''MF_FCTVAL: num'',1p8e12.3)')
cxx     &        num11,num22,den11,den22
      if (noevct .eq. 0) then
         call mf_int roe
         f=(rden*num11-rnum11*den11)*(rden*num22-rnum22*den22)
     &     -rnum12*rnum21*den11*den22
cxx         write(lwpcLOG_lun,
cxx     &       '(''MF_FCTVAL: rnum'',1p10e12.3)')
cxx     &           rnum11,rnum22,rnum12,rnum21,rden
      else
         call mf_lagrng
         f=(num11-r11*den11)*(num22-r22*den22)
     &     -r12*r21*den11*den22
cxx         write(lwpcLOG_lun,
cxx     &       '(''MF_FCTVAL: rnum'',1p10e12.3)')
cxx     &           r11,r22,r12,r21
      end if
      RETURN
      END      ! MF_FCTVAL
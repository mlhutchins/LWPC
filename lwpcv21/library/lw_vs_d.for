      SUBROUTINE LW_VS_D
     &          (lu_mds,lu_lwf,print_lwf,print_mc,print_wf,
     &           begin_file_mds,end_file_mds,begin_file_lwf,
     &           nrcmp,ralt,power,
     &           mxlwf,nrlwf,incl,heading,talt,
     &           dmin,dmax,dinc,davg,
     &           mxpts,nrpts,dst,amp,phs)

c***********************************************************************

c     Gets fields as a function of distance

c***********************************************************************

c     INPUT PARAMETERS:

c     lu_mds        logical unit for path parameters
c     lu_lwf        logical unit for mode sum output

c     print_lwf     Indicates levels of print from ModeSum
c                   =0: minimum print
c                   =1: adds fields vs. distance
c                   =2: adds mode parameters vs. distance

c     print_mc      Indicates levels of print from ModeConversion
c                   =0: minimum print
c                   =1: adds conversion coefficients
c                   =2: adds integrals

c     print_wf      Indicates levels of print from WaveFields
c                   =0: minimum print
c                   =1: adds iterations
c                   =2: adds fields vs. height

c     nrcmp         number of components to output
c     ralt          receiver altitude in km
c     power         power in kW

c     mxlwf         dimension of HEADING
c     nrlwf         number    of HEADING

c     incl          antenna  inclination in degrees
c     heading       antenna  heading in degrees
c     talt          antenna  altitude in km

c     dmin          starting distance in km;
c     dmax          ending   distance in km;
c     dinc          increment in km;

c     davg          logical variable to call for running average of
c                   amplitude vs. distance

c     OUTPUT PARAMETERS:

c     dst           distance array; km
c     amp           signal   array; dB/uv/m
c     phs           relative phase; degrees

c  Change History:
c     21 Oct 95     Changed to get the LOG unit from LWPC_LUN.CMN.

c     28 Dec 95     Test running average of the mode sum; terminate
c                   if 5 consecutive points drop below -30 dB; store
c                   the running average instead of the computed fields.

c*******************!***************************************************

c     LWPC parameters
      include      'lwpc_lun.cmn'

      parameter    (mxprm=21,mxps=11,mxsgmnt=201)

      character*  8 archive,prgm_id
      character* 20 xmtr_id,path_id
      character* 40 prfl_id
      character* 80 case_id
      character*120 file_id
      character*200 error_msg
      integer       pflag,pindex
      real     *  4 freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,

     &              rng,xla,xlo,azm,xdp,fld,sgm,eps,ncd,bta,hpr

      common/lwpc_in/
     &              archive,file_id(3),prgm_id,
     &              case_id,prfl_id,xmtr_id,path_id,
     &              freq,tlat,tlon,bearng,rhomx,rlat,rlon,rrho,pflag,
     &              lat,lon,rho,azim,dip,bfield,sigma,epsr,beta,hprime,
     &              hofwr,topht,botht,pindex
     &      /mf_sw_1/
     &              rng(mxsgmnt),xla(mxsgmnt),xlo(mxsgmnt),
     &              azm(mxsgmnt),xdp(mxsgmnt),fld(mxsgmnt),
     &              sgm(mxsgmnt),eps(mxsgmnt),ncd(mxsgmnt),
     &              bta(mxsgmnt),hpr(mxsgmnt),npr(mxsgmnt),
     &              num(mxsgmnt),nrsgmnt

      character*  4 label/'zyx '/
      logical       begin_file_mds,end_file_mds,begin_file_lwf,davg
      integer       print_lwf,print_mc,print_wf,day,year,UT
      real     *  4 power,incl,headng,talt,ralt,
     &              heading(mxlwf),
     &              dst(mxpts),amp(mxpts,3),phs(mxpts,3),aa(3),pp(3),
     &              param(mxprm),sgmnt(mxps,mxsgmnt)

      logical       do_dst
      real          am(0:4,3),av(0:4,3)
      data          ammin/-30./

c     Set up distance array
      nrdst=(dmax-dmin)/dinc+1.
      if (nrdst .gt. mxpts) then
         write(error_msg,
     &       '(''[LW_VS_D]: '',
     &         ''Too many points required:'',i6)') nrdst
         call LWPC_ERROR ('ERROR', error_msg)
      end if
      d=dmin
      do i=1,nrdst
         dst(i)=d
         d=d+dinc
      end do

      do while (.not.end_file_mds)

c        Get mode parameter data and conversion coefficients
         call LW_READ_MDS (lu_mds,begin_file_mds,end_file_mds)

         if (.not.end_file_mds) then

c           Check if this path was set up using a receiver location
            if (rlat .eq. 99.) then

c              No receiver
               nrpts=nrdst
            else

c              Add an extra point for the receiver
               nrpts=nrdst+1
               if (nrpts .gt. mxpts) then
                  write(error_msg,
     &                '(''[LW_VS_D]: '',
     &                  ''Too many points required:'',i6)') nrpts
                  call LWPC_ERROR ('ERROR', error_msg)
               end if
               dst(nrpts)=rrho
            end if

c           Calculate mode sums over all headings
            do nlwf=1,nrlwf
               headng=heading(nlwf)

               ndst=0
               do_dst=.true.
               do while (do_dst)

                  ndst=ndst+1
                  d=dst(ndst)
                  call LW_SUM_MODES
     &                (print_lwf,print_mc,print_wf,
     &                 power,incl,headng,talt,ralt,
     &                 ndst,nrcmp,d,aa,pp)

c                 Store the amplitude and relative phase
                  do nc=1,nrcmp
                     amp(ndst,nc)=aa(nc)
                     phs(ndst,nc)=pp(nc)
                  end do

                  if (davg) then

c                    Set up running average
                     nstore=MOD(ndst-1,5)
                     do nc=1,nrcmp
                        am(nstore,nc)=aa(nc)
                     end do

                     if (ndst .gt. 4) then

                        do nc=1,nrcmp

c                          Calculate running average
                           av(nstore,nc)=(am(0,nc)+am(1,nc)+am(2,nc)
     &                                   +am(3,nc)+am(4,nc))/5.

c                          Store running average
                           amp(ndst-2,nc)=av(nstore,nc)
                        end do
                     end if

c                    Test running average of the vertical component
                     if (ndst .ge. 9 .and.
     &                  (MAX(av(0,1),av(1,1),av(2,1),av(3,1),av(4,1))
     &                        .lt. ammin)) do_dst=.false.
                  end if

                  if (rlat .ne. 99.) then

c                    Test for receiver distance
                     if (rrho .eq. d) then

c                       At the receiver
                        do nc=1,nrcmp
                          amp(nrpts,nc)=aa(nc)
                          phs(nrpts,nc)=pp(nc)
                        end do
                     else
     &               if (rrho .gt. d .and.
     &                   rrho .lt. dst(ndst+1)) then

c                       The receiver is before the next point
                        d=rrho
                        call LW_SUM_MODES
     &                      (print_lwf,print_mc,print_wf,
     &                       power,incl,headng,talt,ralt,
     &                       nrpts,nrcmp,d,aa,pp)

                        do nc=1,nrcmp
                           amp(nrpts,nc)=aa(nc)
                           phs(nrpts,nc)=pp(nc)
                        end do
                     end if
                  end if

c                 Check if path is complete
                  if (ndst .eq. nrdst) do_dst=.false.
               end do

               if (davg) then

c                 Check if the path is complete;
c                 if not, then fill the rest of the array with the last
c                 computed value.
                  if (ndst .lt. nrdst) then
                     ndst1=ndst-1
                     do ndst=ndst1,nrdst
                        do nc=1,nrcmp
                           amp(ndst,nc)=av(nstore,nc)
                           phs(ndst,nc)=0.
                        end do
                     end do

c                    Test for receiver distance
                     if (rlat .ne. 99.) then
                        if (rrho .ge. dst(ndst1+2)) then
                           do nc=1,nrcmp
                              amp(nrpts,nc)=av(nstore,nc)
                              phs(nrpts,nc)=pp(       nc)
                           end do
                        end if
                     end if
                  end if
               end if

               if (print_lwf .gt. 0) then
                  do nc=1,nrcmp
                     write(lwpcLOG_lun,
     &                   '(/1x,a/
     &                     '' freq='',f5.1,'' tlat='',f5.1,
     &                     '' tlon='',f6.1,'' bearng='',f5.1,
     &                     ''  power='',f5.0/1x,a,'' component '',
     &                     '' incl='',f3.0,'' headng='',f4.0,
     &                     '' talt='',f4.1,'' ralt='',f4.1)')
     &                        case_id,freq,tlat,tlon,bearng,power,
     &                        label(nc:nc),incl,headng,talt,ralt
                     write(lwpcLOG_lun,
     &                   '(3(''  dist   amplitude  phase  ''))')
                     if (amp(1,nc) .ge. 100.) amp(1,nc)=99.99
                     nl=nrpts/3+1
                     do i1=1,nl
                        write(lwpcLOG_lun,
     &                      '(3(f7.0,2f10.4))')
     &                       (dst(i),amp(i,nc),phs(i,nc),i=i1,nrpts,nl)
                     end do
                  end do
               end if

c              Save mode sum
               do nsgmnt=1,nrsgmnt
                  sgmnt( 1,nsgmnt)=rng(nsgmnt)
                  sgmnt( 2,nsgmnt)=xla(nsgmnt)
                  sgmnt( 3,nsgmnt)=xlo(nsgmnt)
                  sgmnt( 4,nsgmnt)=azm(nsgmnt)
                  sgmnt( 5,nsgmnt)=xdp(nsgmnt)
                  sgmnt( 6,nsgmnt)=fld(nsgmnt)
                  sgmnt( 7,nsgmnt)=sgm(nsgmnt)
                  sgmnt( 8,nsgmnt)=eps(nsgmnt)
                  sgmnt( 9,nsgmnt)=bta(nsgmnt)
                  sgmnt(10,nsgmnt)=hpr(nsgmnt)
                  sgmnt(11,nsgmnt)=npr(nsgmnt)
               end do
               nrps=11

               param( 1)=power
               param( 2)=0.
               param( 3)=incl
               param( 4)=headng
               param( 5)=talt
               param( 6)=ralt
               nrprm= 6
                              
               if (prfl_id(1:9) .eq. 'LWPM Date') then

c                 The mode parameter data were generated by the LWPM
c                 for a specific date; decode the information and
c                 update the parameter list.
                  read (prfl_id(12:24),'(2(i2,1x),i2,1x,i4)')
     &                  month,day,year,UT
                  param( 7)=month
                  param( 8)=day
                  param( 9)=year
                  param(10)=UT
                  nrprm=10
               end if
                              
               do nc=1,nrcmp
                  call WRITE_LWF
     &                (lu_lwf,print_lwf,
     &                 bearng,rhomx,rlat,rlon,rrho,
     &                 mxps,nrps,mxsgmnt,nrsgmnt,sgmnt,
     &                 mxprm,nrprm,param,nrcmp,nrlwf,
     &                 mxpts,nrpts,dst,amp(1,nc),phs(1,nc),
     &                 begin_file_lwf)
               end do
            end do
         end if
      end do

      RETURN
      END      ! LW_VS_D
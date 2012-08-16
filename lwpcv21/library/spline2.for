      SUBROUTINE SPLINE2
     &          (mxx,nrx,xi,xd,xb,xs,xdp,
     &           mxy,nry,yi,yd,yb,ys,ydp,
     &           fi,pi,fx,fxy,pf,init)

c***********************************************************************

c     Two dimensional interpolation using cubic splines. The procedure
c     is as follows:

c        Use the function Fi(i,j) to compute spline coefficients x'';
c        interpolate onto the y plane at x;
c        call these interpolated values Fx(j).

c        Use Fx(j) to compute spline coefficients y'';
c        interpolate in y to get Fxy at x,y.

c     The derivatives of Fxy: dFdx, dFdy, are returned in PF.
c     The derivatives are calculated analytically.

c***********************************************************************

      implicit real*8 (a-h,o-z)

      logical       init
      integer       xflag,yflag
      real     *  8 xi (mxx),    yi (mxy),
     &              xd (mxx,mxx),yd (mxy,mxy),
     &              xb (mxx),    yb (mxy),
     &              xs (mxx),    ys (mxy),
     &              xdp(mxx,mxy),ydp(mxy),
     &              fi (mxx,mxy),fx (mxy),
     &              pi(2),fxy,pf(2)


      if (init) then

         init=.false.

cxxcDEBUG
cxx         write(4,'(''x''/(8(i4,f8.2)))')
cxx     &        (i,xi(i),i=1,nrx)
cxx         write(4,'(''y''/(8(i4,f8.2)))')
cxx     &        (j,yi(j),j=1,nry)

c        NOTE:  The order of indices of XD and YD is reversed
c        because DLinEq uses the mathematical convention in which
c        the second index increases faster than the first.

         xflag=0
         do l=2,nrx-1
            j=l-1
            do k=2,nrx-1
               i=k-1
               if (k .eq. l) then
                  xd(j,i)=(xi(k+1)-xi(k-1))/3.
               else
     &         if (k .eq. l-1 .or. k .eq. l+1) then
                  xd(j,i)=(xi(k  )-xi(k-1))/6.
               else
                  xd(j,i)=0.
               end if
            end do
         end do

         yflag=0
         do l=2,nry-1
            j=l-1
            do k=2,nry-1
               i=k-1
               if (k .eq. l) then
                  yd(j,i)=(yi(k+1)-yi(k-1))/3.
               else
     &         if (k .eq. l-1 .or. k .eq. l+1) then
                  yd(j,i)=(yi(k  )-yi(k-1))/6.
               else
                  yd(j,i)=0.
               end if
            end do
         end do

c        Calculate the coefficients for interpolation onto the
c        y plane: x''
         do j=1,nry
            do i=2,nrx-1
               xb(i-1)=(fi(i+1,j)-fi(i  ,j))/(xi(i+1)-xi(i  ))
     &                -(fi(i  ,j)-fi(i-1,j))/(xi(i  )-xi(i-1))
            end do
            call DLinEq (xd,xb,xs,nrx-2,mxx,xflag,err)
            xdp(1,j)=0.
            do i=2,nrx-1
               xdp(i,j)=xs(i-1)
            end do
            xdp(nrx,j)=0.
            xflag=1
         end do
      end if

c     Store current position
      px=pi(1)
      py=pi(2)

c     Find indices for current position
      if (xi(1) .lt. xi(2)) then

c        Xi are in ascending order
         if (px .le. xi(2)) then
            i1=1
            i2=2
         else
     &   if (px .gt. xi(nrx-1)) then
            i1=nrx-1
            i2=nrx
         else
            i2=3
            do while (px .gt. xi(i2))
               i2=i2+1
            end do
            i1=i2-1
         end if
      else

c        Xi are in descending order
         if (px .gt. xi(2)) then
            i1=1
            i2=2
         else
     &   if (px .le. xi(nrx-1)) then
            i1=nrx-1
            i2=nrx
         else
            i2=nrx-2
            do while (px .gt. xi(i2))
               i2=i2-1
            end do
            i1=i2+1
         end if
      end if
      hx=xi(i2)-xi(i1)
      tx=hx**2/6.
      dx1=(px-xi(i1))/hx
      dx2=(px-xi(i2))/hx

      if (yi(1) .lt. yi(2)) then

c        Yi are in ascending order
         if (py .le. yi(2)) then
            j1=1
            j2=2
         else
     &   if (py .gt. yi(nry-1)) then
            j1=nry-1
            j2=nry
         else
            j2=3
            do while (py .gt. yi(j2))
               j2=j2+1
            end do
            j1=j2-1
         end if
      else

c        Yi are in descending order
         if (py .gt. yi(2)) then
            j1=1
            j2=2
         else
     &   if (py .le. yi(nry-1)) then
            j1=nry-1
            j2=nry
         else
            j2=nry-2
            do while (py .gt. yi(j2))
               j2=j2-1
            end do
            j1=j2+1
         end if
      end if
      hy=yi(j2)-yi(j1)
      ty=hy**2/6.
      dy1=(py-yi(j1))/hy
      dy2=(py-yi(j2))/hy


c     Interpolate onto the y axis at x
c     to get the distribution of F along the y axis: Fx
      c1=(dx1-dx1**3)*tx
      c2=(dx2-dx2**3)*tx
      do j=1,nry
         fx(j)=fi(i2,j)*dx1-xdp(i2,j)*c1
     &        -fi(i1,j)*dx2+xdp(i1,j)*c2
      end do

c     Calculate the coefficients for interpolation
c     onto the y axis: y''
      do j=2,nry-1
         yb(j-1)=(fx(j+1)-fx(j  ))/(yi(j+1)-yi(j  ))
     &          -(fx(j  )-fx(j-1))/(yi(j  )-yi(j-1))
      end do
      call DLinEq (yd,yb,ys,nry-2,mxy,yflag,err)
      ydp( 1 )=0.
      do j=2,nry-1
         ydp(j)=ys(j-1)
      end do
      ydp(nry)=0.
      yflag=1

c     Interpolate to the point at x,y: Fxy
      c1=(dy1-dy1**3)*ty
      c2=(dy2-dy2**3)*ty
      fxy=fx(j2)*dy1-ydp(j2)*c1
     &   -fx(j1)*dy2+ydp(j1)*c2


c     Derivatives

c     We currently have the distribution of F along the y axis;
c     calculate dF/dy
      dc1dy=(1.-3.*dy1**2)*ty/hy
      dc2dy=(1.-3.*dy2**2)*ty/hy
      dfdy=fx(j2)/hy-ydp(j2)*dc1dy
     &    -fx(j1)/hy+ydp(j1)*dc2dy


c     Go back to the original distribution of F in the x,y plane;
c     use the coefficients for interpolation onto the y axis to
c     get the distribution of dF/dx along the y axis
      dc1dx=(1.-3.*dx1**2)*tx/hx
      dc2dx=(1.-3.*dx2**2)*tx/hx
      do j=1,nry
         fx(j)=fi(i2,j)/hx-xdp(i2,j)*dc1dx
     &        -fi(i1,j)/hx+xdp(i1,j)*dc2dx
      end do

c     Calculate the coefficients for interpolation
c     onto the x axis
      do j=2,nry-1
         yb(j-1)=(fx(j+1)-fx(j  ))/(yi(j+1)-yi(j  ))
     &          -(fx(j  )-fx(j-1))/(yi(j  )-yi(j-1))
      end do
      call DLinEq (yd,yb,ys,nry-2,mxy,yflag,err)
      ydp( 1 )=0.
      do j=2,nry-1
         ydp(j)=ys(j-1)
      end do
      ydp(nry)=0.

c     Use the coefficients for interpolation onto the x axis to
c     get dF/dx at the point x,y
      c1=(dy1-dy1**3)*ty
      c2=(dy2-dy2**3)*ty
      dfdx=fx(j2)*dy1-ydp(j2)*c1
     &    -fx(j1)*dy2+ydp(j1)*c2


c     Store the derivatives
      pf(1)=dfdx
      pf(2)=dfdy

      RETURN
      END      ! SPLINE2
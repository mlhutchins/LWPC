      SUBROUTINE SPLINE3
     &          (mxx,nrx,xi,xd,xb,xs,xdp,
     &           mxy,nry,yi,yd,yb,ys,ydp,
     &           mxz,nrz,zi,zd,zb,zs,zdp,
     &           fi,pi,fx,fxy,fxyz,pf,init)

c***********************************************************************

c     Three dimensional interpolation using cubic splines. The procedure
c     is as follows:

c        Use the function Fi(i,j,k) to compute spline coefficients x'';
c        interpolate onto the y,z plane at x;
c        call these interpolated values Fx(j,k).

c        Use Fx(i,j) to compute spline coefficients y'';
c        interpolate parallel to the z axis at x,y;
c        call these interpolated values Fxy(k).

c        Use Fxy(k) to compute spline coefficients z'';
c        interpolate in z to get Fxyz at x,y,z.

c     The derivatives of Fxyz: dFdx, dFdy, dFdz, are returned in PF.
c     The derivatives are calculated analytically.

c***********************************************************************

      implicit real*8 (a-h,o-z)

      logical       init
      integer       xflag,yflag,zflag
      real     *  8 xi (mxx),        yi (mxy),    zi (mxz),
     &              xd (mxx,mxx),    yd (mxy,mxy),zd (mxz,mxz),
     &              xb (mxx),        yb (mxy),    zb (mxz),
     &              xs (mxx),        ys (mxy),    zs (mxz),
     &              xdp(mxx,mxy,mxz),ydp(mxy,mxz),zdp(mxz),
     &              fi (mxx,mxy,mxz),fx (mxy,mxz),fxy(mxz),
     &              pi(3),fxyz,pf(3)


      if (init) then

         init=.false.

cxxcDEBUG
cxx         write(4,'(''x''/(8(i4,f8.2)))')
cxx     &        (i,xi(i),i=1,nrx)
cxx         write(4,'(''y''/(8(i4,f8.2)))')
cxx     &        (j,yi(j),j=1,nry)
cxx         write(4,'(''z''/(8(i4,f8.2)))')
cxx     &        (k,zi(k),k=1,nrz)

c        NOTE:  The order of indices of XD, YD and ZD is reversed
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

         zflag=0
         do l=2,nrz-1
            j=l-1
            do k=2,nrz-1
               i=k-1
               if (k .eq. l) then
                  zd(j,i)=(zi(k+1)-zi(k-1))/3.
               else
     &         if (k .eq. l-1 .or. k .eq. l+1) then
                  zd(j,i)=(zi(k  )-zi(k-1))/6.
               else
                  zd(j,i)=0.
               end if
            end do
         end do

c        Calculate the coefficients for interpolation onto the
c        y,z plane: x''
         do k=1,nrz
            do j=1,nry
               do i=2,nrx-1
                  xb(i-1)=(fi(i+1,j,k)-fi(i  ,j,k))/(xi(i+1)-xi(i  ))
     &                   -(fi(i  ,j,k)-fi(i-1,j,k))/(xi(i  )-xi(i-1))
               end do
               call DLinEq (xd,xb,xs,nrx-2,mxx,xflag,err)
               xdp(1,j,k)=0.
               do i=2,nrx-1
                  xdp(i,j,k)=xs(i-1)
               end do
               xdp(nrx,j,k)=0.
               xflag=1
            end do
         end do
      end if

c     Store current position
      px=pi(1)
      py=pi(2)
      pz=pi(3)

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

      if (zi(1) .lt. zi(2)) then

c        Zi are in ascending order
         if (pz .le. zi(2)) then
            k1=1
            k2=2
         else
     &   if (pz .gt. zi(nrz-1)) then
            k1=nrz-1
            k2=nrz
         else
            k2=3
            do while (pz .gt. zi(k2))
               k2=k2+1
            end do
            k1=k2-1
         end if
      else

c        Zi are in descending order
         if (pz .gt. zi(2)) then
            k1=1
            k2=2
         else
     &   if (pz .le. zi(nrz-1)) then
            k1=nrz-1
            k2=nrz
         else
            k2=nrz-2
            do while (pz .gt. zi(k2))
               k2=k2-1
            end do
            k1=k2+1
         end if
      end if
      hz=zi(k2)-zi(k1)
      tz=hz**2/6.
      dz1=(pz-zi(k1))/hz
      dz2=(pz-zi(k2))/hz


c     Interpolate onto the y,z plane at x
c     to get the distribution of F over the y,z plane: Fx
      c1=(dx1-dx1**3)*tx
      c2=(dx2-dx2**3)*tx
      do k=1,nrz
         do j=1,nry
            fx(j,k)=fi(i2,j,k)*dx1-xdp(i2,j,k)*c1
     &             -fi(i1,j,k)*dx2+xdp(i1,j,k)*c2
         end do
      end do

c     Calculate the coefficients for interpolation
c     onto the z axis: y''
      do k=1,nrz
         do j=2,nry-1
            yb(j-1)=(fx(j+1,k)-fx(j  ,k))/(yi(j+1)-yi(j  ))
     &             -(fx(j  ,k)-fx(j-1,k))/(yi(j  )-yi(j-1))
         end do
         call DLinEq (yd,yb,ys,nry-2,mxy,yflag,err)
         ydp(1,k)=0.
         do j=2,nry-1
            ydp(j,k)=ys(j-1)
         end do
         ydp(nry,k)=0.
         yflag=1
      end do

c     Interpolate onto the z axis at x,y
c     to get the distribution of F along the z axis: Fxy
      c1=(dy1-dy1**3)*ty
      c2=(dy2-dy2**3)*ty
      do k=1,nrz
         fxy(k)=fx(j2,k)*dy1-ydp(j2,k)*c1
     &         -fx(j1,k)*dy2+ydp(j1,k)*c2
      end do

c     Calculate the coefficients for interpolation to the
c     point at x,y,z: z''
      do k=2,nrz-1
         zb(k-1)=(fxy(k+1)-fxy(k  ))/(zi(k+1)-zi(k  ))
     &          -(fxy(k  )-fxy(k-1))/(zi(k  )-zi(k-1))
      end do
      call DLinEq (zd,zb,zs,nrz-2,mxz,zflag,err)
      zdp(1)=0.
      do k=2,nrz-1
         zdp(k)=zs(k-1)
      end do
      zdp(nrz)=0.
      zflag=1

c     Interpolate to the point at x,y,z: Fxyz
      c1=(dz1-dz1**3)*tz
      c2=(dz2-dz2**3)*tz
      fxyz=fxy(k2)*dz1-zdp(k2)*c1
     &    -fxy(k1)*dz2+zdp(k1)*c2


c     Derivatives

c     We currently have the distribution of F along the z axis;
c     calculate dF/dz
      dc1dz=(1.-3.*dz1**2)*tz/hz
      dc2dz=(1.-3.*dz2**2)*tz/hz
      dfdz=fxy(k2)/hz-zdp(k2)*dc1dz
     &    -fxy(k1)/hz+zdp(k1)*dc2dz


c     Go back to the distribution of F in the y,z plane: Fx;
c     use the coefficients for interpolation onto the z axis to
c     get the distribution of dF/dy along the z axis
      dc1dy=(1.-3.*dy1**2)*ty/hy
      dc2dy=(1.-3.*dy2**2)*ty/hy
      do k=1,nrz
         fxy(k)=fx(j2,k)/hy-ydp(j2,k)*dc1dy
     &         -fx(j1,k)/hy+ydp(j1,k)*dc2dy
      end do

c     Calculate the coefficients for interpolation to the
c     point at x,y,z
      do k=2,nrz-1
         zb(k-1)=(fxy(k+1)-fxy(k  ))/(zi(k+1)-zi(k  ))
     &          -(fxy(k  )-fxy(k-1))/(zi(k  )-zi(k-1))
      end do
      call DLinEq (zd,zb,zs,nrz-2,mxz,zflag,err)
      zdp(1)=0.
      do k=2,nrz-1
         zdp(k)=zs(k-1)
      end do
      zdp(nrz)=0.

c     Use the coefficients for interpolation to the
c     point at x,y,z to interpolate dF/dy
      c1=(dz1-dz1**3)*tz
      c2=(dz2-dz2**3)*tz
      dfdy=fxy(k2)*dz1-zdp(k2)*c1
     &    -fxy(k1)*dz2+zdp(k1)*c2


c     Go back to the original distribution of F in x,y,z;
c     use the coefficients for interpolation onto the y,z plane to
c     get the distribution of dF/dx over the y,z plane
      dc1dx=(1.-3.*dx1**2)*tx/hx
      dc2dx=(1.-3.*dx2**2)*tx/hx
      do k=1,nrz
         do j=1,nry
            fx(j,k)=fi(i2,j,k)/hx-xdp(i2,j,k)*dc1dx
     &             -fi(i1,j,k)/hx+xdp(i1,j,k)*dc2dx
         end do
      end do

c     Calculate the coefficients for interpolation onto the
c     z axis
      do k=1,nrz
         do j=2,nry-1
            yb(j-1)=(fx(j+1,k)-fx(j  ,k))/(yi(j+1)-yi(j  ))
     &             -(fx(j  ,k)-fx(j-1,k))/(yi(j  )-yi(j-1))
         end do
         call DLinEq (yd,yb,ys,nry-2,mxy,yflag,err)
         ydp(1,k)=0.
         do j=2,nry-1
            ydp(j,k)=ys(j-1)
         end do
         ydp(nry,k)=0.
      end do

c     Use the coefficients for interpolation onto the z axis to
c     get the distribution of dF/dx along the z axis
      c1=(dy1-dy1**3)*ty
      c2=(dy2-dy2**3)*ty
      do k=1,nrz
         fxy(k)=fx(j2,k)*dy1-ydp(j2,k)*c1
     &         -fx(j1,k)*dy2+ydp(j1,k)*c2
      end do

c     Calculate the coefficients for interpolation to the
c     point at x,y,z
      do k=2,nrz-1
         zb(k-1)=(fxy(k+1)-fxy(k  ))/(zi(k+1)-zi(k  ))
     &          -(fxy(k  )-fxy(k-1))/(zi(k  )-zi(k-1))
      end do
      call DLinEq (zd,zb,zs,nrz-2,mxz,zflag,err)
      zdp(1)=0.
      do k=2,nrz-1
         zdp(k)=zs(k-1)
      end do
      zdp(nrz)=0.

c     Use the coefficients for interpolation to the
c     point at x,y,z to interpolate dF/dx
      c1=(dz1-dz1**3)*tz
      c2=(dz2-dz2**3)*tz
      dfdx=fxy(k2)*dz1-zdp(k2)*c1
     &    -fxy(k1)*dz2+zdp(k1)*c2


c     Store the derivatives
      pf(1)=dfdx
      pf(2)=dfdy
      pf(3)=dfdz

cxxcDEBUG
cxx      write(4,'(a,2i4,5f8.2)')
cxx     &     'i1,i2,x1,x2,x,dx1,dx2',i1,i2,xi(i1),xi(i2),px,dx1,dx2
cxx      write(4,'(a,2i4,5f8.2)')
cxx     &     'j1,j2,y1,y2,y,dy1,dy2',j1,j2,yi(j1),yi(j2),py,dy1,dy2
cxx      write(4,'(a,2i4,5f8.2)')
cxx     &     'k1,k2,z1,z2,z,dz1,dz2',k1,k2,zi(k1),zi(k2),pz,dz1,dz2

cxx      write(4,'(a,8f8.2)')
cxx     &     'f   ',fi(i1,j1,k1),fi(i2,j1,k1),fi(i1,j2,k1),fi(i2,j2,k1),
cxx     &            fi(i1,j1,k2),fi(i2,j1,k2),fi(i1,j2,k2),fi(i2,j2,k2)
cxx      write(4,'(a,8f8.2)')
cxx     &     'fx  ',fx(j1,k1),fx(j2,k1),
cxx     &            fx(j1,k2),fx(j2,k2)
cxx      write(4,'(a,8f8.2)')
cxx     &     'fxy ',fxy(k1),fxy(k1)
cxx      write(4,'(a,8f8.2)')
cxx     &     'fxyz',fxyz,pf

      RETURN
      END      ! SPLINE3
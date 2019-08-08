!=============================================================================!
!                                fractal                                      !
!=============================================================================!
!              Module defining all fractals to be calcualted                  !
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================! 
module fractal
  implicit none


contains

  function mand(Max_iter,z,c,e) result(k)
    implicit none
    integer,intent(in)::Max_iter 
    complex*16::z,c
    integer :: k
    real :: e

    if (e.EQ.int(e))then 
       do k=0,Max_iter
          z = z**int(e)+c
          if (abs(z).gt.2) then
             exit
          end if
       end do
    else
       do k=0,Max_iter
          z = z**e+c
          if (abs(z).gt.2) then
             exit
          end if
       end do
    end if

  end function mand


  function julia(Max_iter,c,z,e) result(k)
    integer,intent(in)::Max_iter 
    complex*16::z,c
    integer::k
    real :: e
    if (e.EQ.int(e))then
       do k=0,Max_iter
          z = z**int(e)+c
          if (abs(z).gt.2) then
             exit
          end if
       end do
    else 
       do k=0,Max_iter
          z = z**e+c
          if (abs(z).gt.2) then
             exit
          end if
       end do
    end if
  end function julia


function burning(Max_iter,z,c,e) result(k)
    implicit none
    integer,intent(in)::Max_iter 
    complex*16::z,c
    integer :: k
    real :: e

    if (e.EQ.int(e))then
       do k=0,Max_iter
          z = (abs(real(z))+cmplx(0,1)*abs(aimag(z)))**int(e)+c
          if (abs(z).gt.2) then
             exit
          end if
       end do
    else
       do k=0,Max_iter
          z = (abs(real(z))+cmplx(0,1)*abs(aimag(z)))**e+c
          if (abs(z).gt.2) then
             exit
          end if
       end do
    end if

  end function burning
  


  function random_pos()result(z)
    implicit none
    complex*16::z,perim_z
    integer::i,j
    real::z_re,z_im,theta,floater
    character(len=100)::path_string,thing

#ifdef direc
#define thing direc
#endif
    path_string=trim(adjustl(path_string))


    open(unit=35,file=thing//"/../bin/perim.mand")
!    open(unit=35,file="perim.mand")


    call init_random_seed()

    call random_number(floater)
    i=int(floater*1666)
    do j=1,i
       read(35,*)perim_z
    end do

    call init_random_seed()
    call random_number(floater)
    theta=2*3.1415*floater

    call init_random_seed()
    call random_number(floater)

    floater=floater*0.1

    z_re=real(perim_z)+cos(theta)*cmplx(1,0)*floater
    z_im=aimag(perim_z)+sin(theta)*cmplx(0,1)*floater

    z=cmplx(z_re,z_im)

    close(35)
  end function random_pos


  subroutine init_random_seed()
    implicit none
    integer, allocatable :: seed(:)
    integer ::  n, un, istat

    call random_seed(size = n)
    allocate(seed(n))
    open(newunit=un, file="/dev/urandom", access="stream", &
         form="unformatted", action="read", status="old", iostat=istat)
    read(un) seed
    close(un)
    call random_seed(put=seed)
  end subroutine init_random_seed



  function p(z,e) result(z_out)
    implicit none
    complex*16 :: z,z_out
    real :: e
    if (e.eq.int(e))then
       z_out=z**int(e)-1
!       z_out=sin(z)-1
    else
       z_out=z**(e)-1
    endif
  end function p

  function diff(z,e) result(grad)
    implicit none
    real :: e
    complex*16 :: grad,z,dx=1e-10

       grad=(-p(z-dx,e)+p(z+dx,e))/(2*dx)

!       grad=e*z**int(e-1)
  end function diff



  function Newton(Max_iter,z,e) result(theta)
    implicit none
    complex*16 :: z
    real :: theta,e
    integer :: k,Max_iter

    do k=0,max_iter
!       if (abs(p(z,e)/diff(z,e)).lt.real(1e-17))then
!          exit
!       end if
       z=z-p(z,e)/diff(z,e)

    end do
    
   
    theta=atan(aimag(z)/real(z))
    
    

  end function Newton





end module fractal

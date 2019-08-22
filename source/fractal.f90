!---- File documented byA Fortran Documenter, Z.Hawkhead
!=============================================================================!
!                                fractal                                      !
!=============================================================================!
!              Module defining all fractals to be calcualted                  !
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================! 
module fractal
  use colours
  use io
  use iso_fortran_env
  implicit none


contains

  function mand(Max_iter,z,c,e) result(k_real)
    !==============================================================================!
    !                                   M A N D                                    !
    !==============================================================================!
    ! Formula for calculating the Mandelbrot set fractal.                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           Max_iter,          intent :: in                                    !
    !           z,                 intent :: in                                    !
    !           c,                 intent :: in                                    !
    !           e,                 intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           k_real                                                             !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    integer,intent(in)::Max_iter 
    complex(complex_kind)::z,c,z_old=(0,0)
    integer :: k
    real :: e,z_cum=0,k_real

    if (e.EQ.int(e))then 
       do k=1,Max_iter
          z_old=z
          z = z**int(e)+c-exp(z)
          if (triangle)then
             call triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call ave_angle(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    else
       do k=0,Max_iter
          z_old=z
          z = z**e+c
          if (triangle)then
             call triangle_ineq(z,c,z_old,z_cum,k)

          elseif (ave_an)then
             call ave_angle(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    end if

    if (triangle)then
       k_real=z_cum/real(k)

       if (isnan(k_real))k_real=0
       z_cum=0
    elseif(ave_an)then 
       k_real=k_real/real(k)
!      if (isnan(k_real))k_real=0
    else
       call smooth_iter(k,z,k_real)
    end if
  end function mand


  function julia(Max_iter,c,z,e) result(k_real)
    !==============================================================================!
    !                                  J U L I A                                   !
    !==============================================================================!
    ! Formula for calculating one of the Julia set fractals.                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           Max_iter,          intent :: in                                    !
    !           c,                 intent :: in                                    !
    !           z,                 intent :: in                                    !
    !           e,                 intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           k_real                                                             !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(in)::Max_iter 
    complex(complex_kind)::z,c,z_old=(0,0)
    integer :: k
    real :: e,z_cum=0,k_real

    if (e.EQ.int(e))then
       do k=0,Max_iter
          z_old=z
          z = z**int(e)+c

          if (triangle)then
             call triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call ave_angle(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    else 
       do k=0,Max_iter
          z_old=z       
          z = z**e+c
          if (triangle)then
             call triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call ave_angle(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    end if

    if (triangle)then
       k_real=z_cum/real(k)
       if (isnan(k_real))k_real=0
       z_cum=0
    elseif(ave_an)then
       k_real=k_real/real(k)
!       if (isnan(k_real))k_real=0
    else
       call smooth_iter(k,z,k_real)

    end if

  end function julia


  function burning(Max_iter,z,c,e) result(k_real)
    !==============================================================================!
    !                                B U R N I N G                                 !
    !==============================================================================!
    ! Formula for calculating the "Burning Ship" fractal. This is a derivative     !
    ! of the Mandelbrot set.                                                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           Max_iter,          intent :: in                                    !
    !           z,                 intent :: in                                    !
    !           c,                 intent :: in                                    !
    !           e,                 intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           k_real                                                             !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    integer,intent(in)::Max_iter 
    complex(complex_kind)::z,c,z_old=(0,0)
    integer :: k
    real :: e,z_cum=0,k_real

    if (e.EQ.int(e))then
       do k=0,Max_iter

          !          z=          (real(z)+cmplx(0,1)*(aimag(z)))*(real(z)-cmplx(0,1)*(aimag(z)))+c
          z_old=z
          z = (abs(real(z))+cmplx(0,1)*abs(aimag(z)))**int(e)+c
          if (triangle)then
             call triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call ave_angle(z,k_real)
          end if

          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    else
       do k=0,Max_iter
          z_old=z
          z = (abs(real(z))+cmplx(0,1)*abs(aimag(z)))**e+c
          if (triangle)then
             call triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call ave_angle(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    end if
    if (triangle)then
       k_real=z_cum/real(k)
       if (isnan(k_real))k_real=0
       z_cum=0
    elseif(ave_an)then
       k_real=k_real/k
       if (isnan(k_real))k_real=0
    else
       call smooth_iter(k,z,k_real)
    end if
  end function burning



  function random_pos() result(z)
    !==============================================================================!
    !                             R A N D O M _ P O S                              !
    !==============================================================================!
    ! Function to calculate rhe random position used in the Buddahbrot fractal.    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           z                                                                  !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    complex(complex_kind)::z,perim_z
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
    !==============================================================================!
    !                       I N I T _ R A N D O M _ S E E D                        !
    !==============================================================================!
    ! Initialise a random seed for proper pseudo random numbers across MPI         !
    ! processes.                                                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
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
    !==============================================================================!
    !                                      P                                       !
    !==============================================================================!
    ! Function defining the form of the equation used in the Newton-Raphson        !
    ! fractel.                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           z,                 intent :: in                                    !
    !           e,                 intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           z_out                                                              !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    complex(complex_kind) :: z,z_out
    real :: e
    if (e.eq.int(e))then
       z_out=z**int(e)-1
       !       z_out=sin(z)-1
    else
       z_out=z**(e)-1
    endif
  end function p

  function diff(z,e) result(grad)
    !==============================================================================!
    !                                   D I F F                                    !
    !==============================================================================!
    ! Gradient calculating function for the Newton-Raphson fractal.                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           z,                 intent :: in                                    !
    !           e,                 intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           grad                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    real :: e
    complex(complex_kind) :: grad,z,dx=1e-10

    grad=(-p(z-dx,e)+p(z+dx,e))/(2*dx)

    !       grad=e*z**int(e-1)
  end function diff



  function Newton(Max_iter,z,e) result(theta)
    !==============================================================================!
    !                                 N E W T O N                                  !
    !==============================================================================!
    ! Function implementing the Newton-Raphson method for complex numbers.         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           Max_iter,          intent :: in                                    !
    !           z,                 intent :: in                                    !
    !           e,                 intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           theta                                                              !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    complex(complex_kind) :: z
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

!---- File documented by Fortran Documenter, Z.Hawkhead
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
  use trace
  implicit none


contains

  function fractal_mand(Max_iter,z,c,e) result(k_real)
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
    complex(complex_kind)::z,c,z_old=(0,0),dir
    integer :: k
    real :: e,z_cum=0,k_real
    k_real=0
    dir=(0,0)
    if (e.EQ.int(e))then 
       do k=1,Max_iter
          z_old=z
          z = z**int(e)+c

          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if(light)then
             call colour_light(z,k_real,k,dir)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
             call colour_triangle_ineq(z,c,z_old,z_cum,k)

          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)
    end if



  end function fractal_mand


  function fractal_julia(Max_iter,c,z,e) result(k_real)
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
    k_real=0
    !    call trace_entry("JULIA")
    if (e.EQ.int(e))then
       do k=0,Max_iter
          z_old=z
          z = z**int(e)+c

          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)

    end if
    !    call trace_exit("JULIA")
  end function fractal_julia


  function fractal_burning(Max_iter,z,c,e) result(k_real)
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
    k_real=0
    !    call trace_entry("BURNING")
    if (e.EQ.int(e))then
       do k=0,Max_iter

          !          z=          (real(z)+cmplx(0,1)*(aimag(z)))*(real(z)-cmplx(0,1)*(aimag(z)))+c
          z_old=z
          z = (abs(real(z))+cmplx(0,1)*abs(aimag(z)))**int(e)+c
          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)
    end if
    !    call trace_exit("BURINING")
  end function fractal_burning



  function fractal_random_pos() result(z)
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
    !    call trace_entry("RANDOM_POS")

#ifdef direc
#define thing direc
#endif
    path_string=trim(adjustl(path_string))


    open(unit=35,file=thing//"/../bin/perim.mand")
    !    open(unit=35,file="perim.mand")


    call fractal_init_random_seed()

    call random_number(floater)
    i=int(floater*1666)
    do j=1,i
       read(35,*)perim_z
    end do

    call fractal_init_random_seed()
    call random_number(floater)
    theta=2*3.1415*floater

    call fractal_init_random_seed()
    call random_number(floater)

    floater=floater*0.1

    z_re=real(perim_z)+cos(theta)*cmplx(1,0)*floater
    z_im=aimag(perim_z)+sin(theta)*cmplx(0,1)*floater

    z=cmplx(z_re,z_im)

    close(35)
    !    call trace_exit("RANDOM_POS")
  end function fractal_random_pos


  subroutine fractal_init_random_seed()
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
  end subroutine fractal_init_random_seed



  function fractal_p(z,e) result(z_out)
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
    else
       z_out=z**(e)-1
    endif

  end function fractal_p

  function fractal_diff(z,e) result(grad)
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
    grad=e*z**(e-1)
  end function fractal_diff



  function fractal_newton(Max_iter,z,e,nova) result(theta)
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
    complex(complex_kind) :: z,z_old,z_pix
    real :: theta,e
    integer :: k,Max_iter
    logical :: nova
    theta=0
    if(nova)then
       z_pix=z
       if (abs(julia_const).gt.0) z=julia_const
    else
       z_pix=(0,0)
    end if

    do k=0,max_iter
       z_old=z
       z=z-relaxation*fractal_p(z,e)/fractal_diff(z,e)+z_pix


       if(abs(z_old-z).lt.1.0e-6)exit
       if (exponential) call colour_exponential(1/(z-z_old),theta)

    end do

    if(smooth)then
       call colour_smooth_iter(k,z,theta)
    endif

  end function fractal_newton


  function fractal_magnet(Max_iter,z,c,e) result(k_real)
    !==============================================================================!
    !                         F R A C T A L _ M A G N E T                          !
    !==============================================================================!
    ! Formula for calculating the Magnet set fractal.                              !
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
    k_real=0
    if (e.EQ.int(e))then 
       do k=1,Max_iter
          z_old=z
          z = ((z**int(e)+c-1)/(e*z+c-2))**2

          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          elseif(exponential)then
             call colour_exponential(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    else
       do k=0,Max_iter
          z_old=z
          z = ((z**e+c-1)/(e*z+c-2))**2
          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)

          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          elseif(exponential)then
             call colour_exponential(z,k_real)
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
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)
    end if

  end function fractal_magnet


  function fractal_phoenix(Max_iter,c,z,e) result(k_real)
    !==============================================================================!
    !                        F R A C T A L _ P H O E N I X                         ! 
    !==============================================================================!
    ! Formula for calculating one of the Phoenix set fractals.                     !
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
    integer,intent(in)    :: Max_iter 
    complex(complex_kind) :: z,c,z_old=(0,0)
    complex(complex_kind) :: z_2
    complex(complex_kind),dimension(:),allocatable :: history
    integer :: k
    real :: e,z_cum=0,k_real

    allocate(history(0:max_iter))
    k_real=0

    if (e.EQ.int(e))then
       history(0)=z
       history(1)=z**int(e)+real(c)+aimag(c)*z
       do k=2,Max_iter

          z_old=z

          z = history(k-1)**int(e)+real(c)+aimag(c)*history(k-2)
          history(k)=z
          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    else
       history(0)=z
       history(1)=z**e+real(c)+aimag(c)*z
       do k=0,Max_iter
          z_old=z       
          z = history(k-1)**e+real(c)+aimag(c)*history(k-2)
          history(k)=z
          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
       if (isnan(k_real))k_real=0
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)

    end if
    deallocate(history)
  end function fractal_phoenix


  function fractal_rational(Max_iter,c,z,e_1,e_2,lambda) result(k_real)

    integer,intent(in)::Max_iter 
    complex(complex_kind)::z,c,z_old=(0,0)
    integer :: k
    real :: e_1,e_2,z_cum=0,k_real,lambda
    k_real=0
    if (e_1.eq.int(e_1).and.e_2.eq.int(e_2))then
       do k=0,Max_iter
          z_old=z
          z = z**int(e_1)+c+lambda*z**int(e_2)

          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    elseif (e_1.eq.int(e_1).and.e_2.ne.int(e_2))then
       do k=0,Max_iter
          z_old=z
          z = z**int(e_1)+c+lambda*z**e_2

          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do
    elseif (e_1.ne.int(e_1).and.e_2.eq.int(e_2))then
       do k=0,Max_iter
          z_old=z
          z = z**e_1+c+lambda*z**int(e_2)

          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          end if
          if (abs(z).gt.bail_out) then
             exit
          end if
       end do

    else 
       do k=0,Max_iter
          z_old=z       
          z = z**e_1+c+lambda*z**e_2
          if (triangle)then
             call colour_triangle_ineq(z,c,z_old,z_cum,k)
          elseif (ave_an)then
             call colour_ave_angle(z,k_real)
          else if (exponential)then
             call colour_exponential(z,k_real)
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
    else if (exponential)then
       call colour_exponential(z,k_real)
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)

    end if

  end function fractal_rational


end module fractal

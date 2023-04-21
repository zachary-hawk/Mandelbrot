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
  use iso_fortran_env,only :real64
  use trace
  implicit none

  type comb_result
     integer, dimension(:), allocatable :: combs
  end type comb_result

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
    complex(dp)::z,c,z_old=(0,0),dir
    integer :: k,m=0
    real(dp) :: e,z_cum=0,k_real,z_cum_old,d
    !call trace_entry("FRACTAL_MAND")
    k_real=0.0_dp
    dir=(0,0)
    if (e.EQ.int(e))then 
       do k=1,Max_iter
          z_old=z

          z = z**int(e)+c
          if (triangle)then
             z_cum_old=z_cum
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
             z_cum_old=z_cum
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
       call colour_smooth_iter(k,z,d)
       d=d-int(d)
       k_real=real(d,dp)*z_cum+(1.0_dp-real(d,dp))*z_cum_old
       if (isnan(k_real))k_real=0.0_dp
       if (k.gt.max_iter-1)k_real=0.0_dp
       z_cum=0

    elseif(ave_an)then 
       k_real=k_real/real(k)
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)
    end if


    !call trace_exit("FRACTAL_MAND")
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
    complex(dp)::z,c,z_old=(0,0)
    integer :: k
    real(dp):: e,z_cum=0,k_real,d,z_cum_old
    k_real=0.0_dp
    !    call trace_entry("JULIA")
    if (e.EQ.int(e))then
       do k=0,Max_iter
          z_old=z

          z = z**int(e)+c

          if (triangle)then
             z_cum_old=z_cum
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
             z_cum_old=z_cum
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
       call colour_smooth_iter(k,z,d)
       d=d-int(d)
       k_real=d*z_cum+(1.0_dp-d)*z_cum_old
       if (isnan(k_real))k_real=0
       if (k.gt.max_iter-1)k_real=0
       z_cum=cmplx_0

    elseif(ave_an)then
       k_real=k_real/real(k,dp)
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
    complex(dp)::z,c,z_old=(0,0)
    integer :: k
    real(dp):: e,z_cum=0,k_real,d,z_cum_old
    k_real=0.0_dp
    !    call trace_entry("BURNING")
    
    if (e.EQ.int(e))then
       do k=0,Max_iter

          !          z=          (real(z)+cmplx(0,1)*(dimag(z)))*(real(z)-cmplx(0,1)*(dimag(z)))+c
          z_old=z
          z = (abs(real(z,dp))-cmplx_i*abs(dimag(z)))**int(e)+c
          if (triangle)then
             z_cum_old=z_cum
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
          z = (abs(real(z,dp))-cmplx_i*abs(dimag(z)))**e+c
          if (triangle)then
             z_cum_old=z_cum
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
       call colour_smooth_iter(k,z,d)
       d=d-int(d)
       k_real=d*z_cum+(1.0_dp-d)*z_cum_old
       if (isnan(k_real))k_real=0
       if (k.gt.max_iter-1)k_real=0
       z_cum=0
    elseif(ave_an)then
       k_real=k_real/k
       if (isnan(k_real))k_real=0.0_dp
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
    complex(dp)::z,perim_z
    integer::i,j
    real(dp)::z_re,z_im,theta,floater
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

    floater=floater*0.1_dp

    z_re=real(perim_z,dp)+cos(theta)*cmplx_1*floater
    z_im=dimag(perim_z)+sin(theta)*cmplx_i*floater

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



  function fractal_p(z,coeff) result(z_out)
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
    complex(dp) :: z,z_out
    complex(dp),dimension(:) :: coeff 
    integer :: i
    z_out=(0.0_dp,0.0_dp)
    
    do i=1,size(coeff)
       z_out=z_out + coeff(i) * z ** (i-1) 
    end do
!!$    if (e.eq.int(e))then
!!$       z_out=z**int(e)-1
!!$    else
!!$       z_out=z**(e)-1
!!$    endif

  end function fractal_p

  function fractal_diff(z,coeff) result(grad)
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

    complex(dp) :: grad,z
    complex(dp),dimension(:) :: coeff 
    integer :: i
    grad=(0.0_dp,0.0_dp)
    
    do i=2,size(coeff)
       grad=grad + (real(i,dp)-1.0_dp) *  coeff(i) * z ** (i-2) 
    end do
    
    
  end function fractal_diff



  function fractal_newton(Max_iter,z,e,nova,coeff) result(theta)
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
    complex(dp) :: z,z_old,z_pix
    complex(dp),dimension(:) :: coeff
    real(dp),dimension(:),allocatable :: dist
    real(dp):: theta,e
    integer :: k,Max_iter,i
    logical :: nova

    if (.not.allocated(roots))then
       allocate(roots(1:3))
       roots(1)=exp(2.0_dp*pi*cmplx_i)
       roots(2)=exp(2.0_dp*pi*cmplx_i/3.0_dp)
       roots(3)=exp(4.0_dp*pi*cmplx_i/3.0_dp)
    end if
    !call fractal_coeff(coeff)

    theta=0.0_dp
    if(nova)then
       z_pix=z
       if (abs(julia_const).gt.0) z=julia_const
    else
       z_pix=cmplx_0
    end if

    do k=0,max_iter
       z_old=z
       z=z-relaxation*fractal_p(z,coeff)/fractal_diff(z,coeff)+z_pix


       if(abs(z_old-z).lt.1.0e-6_dp)exit
       if (exponential) call colour_exponential(1/(z-z_old),theta)

    end do

    if(smooth)then
       call colour_smooth_iter(k,z,theta)
    elseif(simple_set)then
       allocate(dist(1:size(roots)))
       do i=1, size(roots)
          dist(i) = abs(roots(i)-z)
       end do
       theta=real(minloc(dist,DIM=1),dp)
       
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
    complex(dp)::z,c,z_old=(0.0_dp,0.0_dp)
    integer :: k
    real(dp):: e,z_cum=0.0_dp,k_real,d,z_cum_old
    k_real=0.0_dp
    if (e.EQ.int(e))then 
       do k=1,Max_iter
          z_old=z
          z = ((z**int(e)+c-1.0_dp)/(e*z+c-2.0_dp))**2
          
          if (triangle)then
             z_cum_old=z_cum
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
          z = ((z**e+c-1.0_dp)/(e*z+c-2.0_dp))**2
          if (triangle)then
             z_cum_old=z_cum
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
       call colour_smooth_iter(k,z,d)
       d=d-int(d)
       k_real=d*z_cum+(1.0_dp-real(d,dp))*z_cum_old
       if (isnan(k_real))k_real=0.0_dp
       if (k.gt.max_iter-1)k_real=0.0_dp
       z_cum=cmplx_0

    elseif(ave_an)then 
       k_real=k_real/real(k,dp)
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z+cmplx_i*1e-17_dp,k_real)
    end if
    !    k_real=real(k)
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
    complex(dp) :: z,c,z_old=(0,0)
    complex(dp) :: z_2
    complex(dp),dimension(:),allocatable :: history
    integer :: k
    real(dp):: e,z_cum=0,k_real,d,z_cum_old

    allocate(history(0:max_iter))
    k_real=0.0_dp

    if (e.EQ.int(e))then
       history(0)=z
       history(1)=z**int(e)+real(c,dp)+dimag(c)*z
       do k=2,Max_iter

          z_old=z

          z = history(k-1)**int(e)+real(c,dp)+dimag(c)*history(k-2)
          history(k)=z
          if (triangle)then
             z_cum_old=z_cum
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
       history(1)=z**e+real(c,dp)+dimag(c)*z
       do k=0,Max_iter
          z_old=z       
          z = history(int(z)-1)**e+real(c,dp)+dimag(c)*history(k-2)
          history(k)=z
          if (triangle)then
             z_cum_old=z_cum
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
       call colour_smooth_iter(k,z,d)
       d=d-int(d)
       k_real=d*z_cum+(1.0_dp-d)*z_cum_old
       if (isnan(k_real))k_real=0.0_dp
       if (k.gt.max_iter-1)k_real=0.0_dp
       z_cum=cmplx_0
    elseif(ave_an)then
       k_real=k_real/real(k,dp)
       if (isnan(k_real))k_real=0.0_dp
    else if(simple_set)then
       call colour_set(k,k_real)
    elseif(smooth)then
       call colour_smooth_iter(k,z,k_real)

    end if
    deallocate(history)
  end function fractal_phoenix


  function fractal_rational(Max_iter,c,z,e_1,e_2,lambda) result(k_real)
    !==============================================================================!
    !                       F R A C T A L _ R A T I O N A L                        !
    !==============================================================================!
    ! Function defining the formula for the rational fractal set, used to          !
    ! produce pretty geometrical shapes.                                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           Max_iter,          intent :: in                                    !
    !           c,                 intent :: in                                    !
    !           z,                 intent :: in                                    !
    !           e_1,               intent :: in                                    !
    !           e_2,               intent :: in                                    !
    !           lambda,            intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           k_real                                                             !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!

    integer,intent(in)::Max_iter 
    complex(dp)::z,c,z_old=(0,0)
    integer :: k
    real(dp):: e_1,e_2,z_cum=0,k_real,lambda,d,z_cum_old
    k_real=0.0_dp
    if (e_1.eq.int(e_1).and.e_2.eq.int(e_2))then
       do k=0,Max_iter
          z_old=z
          z = z**int(e_1)+c+lambda*z**int(e_2)

          if (triangle)then
             z_cum_old=z_cum
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
             z_cum_old=z_cum
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
             z_cum_old=z_cum
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
             z_cum_old=z_cum
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
       call colour_smooth_iter(k,z,d)
       d=d-int(d)
       k_real=d*z_cum+(1.0_dp-d)*z_cum_old
       if (isnan(k_real))k_real=0
       if (k.gt.max_iter-1)k_real=0
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





  subroutine fractal_coeff(coeff)
    integer :: n_roots
    complex(dp),allocatable,dimension(:),intent(out)::coeff
    complex(dp),allocatable,dimension(:,:)::prods

    integer :: root
    integer :: choices


    type(comb_result), dimension(:), pointer :: r
    integer :: i, j
    call trace_entry('fractal_coeff')

    n_roots=size(roots)
    allocate(coeff(0:n_roots))
    coeff(:)=(1.0_dp,0.0_dp)

    do root=0,n_roots-1
       call comb(n_roots,n_roots-root,r)
       allocate(prods(0:n_roots-root-1,0:choose(n_roots,n_roots-root)-1))
       do i = 0, choose(n_roots,n_roots-root)-1
          do j=0,n_roots-root-1
             !prods(j,i)=(0.0_dp,0.0_dp)
             prods(j,i) =  -roots(r(i)%combs(j)+1)
           
          end do
          !print*,r(i)%combs(:)+1
       end do
       !prods(:,i)
       !roots(r(i)%combs(j))  
       
       coeff(root)=sum(product(prods,1))
       

       deallocate(prods)
       deallocate(r)
    end do
    !coeff(n_roots)=(1.0_dp,0.0_dp)
    call trace_exit('fractal_coeff')
  end subroutine fractal_coeff


  function choose(n, k, err)
    integer :: choose
    integer, intent(in) :: n, k
    integer, optional, intent(out) :: err

    integer :: imax, i, imin, ie
    call trace_entry('fractal_choose')
    ie = 0
    if ( (n < 0 ) .or. (k < 0 ) ) then
       choose = 0
       ie = 1
    else
       if ( n < k ) then
          choose = 0
       else if ( n == k ) then
          choose = 1
       else
          imax = max(k, n-k)
          imin = min(k, n-k)
          choose = 1
          do i = imax+1, n
             choose = choose * i
          end do
          do i = 2, imin
             choose = choose / i
          end do
       end if
    end if
    if ( present(err) ) err = ie
    call trace_exit('fractal_choose')
  end function choose

  subroutine comb(n, k, co)
    integer, intent(in) :: n, k
    type(comb_result), dimension(:), pointer, intent(out) :: co

    integer :: i, j, s, ix, kx, hm, t
    integer :: err
    call trace_entry('fractal_comb')
    hm = choose(n, k, err)
    if ( err /= 0 ) then
       nullify(co)
       return
    end if

    allocate(co(0:hm-1))
    do i = 0, hm-1
       allocate(co(i)%combs(0:k-1))
    end do
    do i = 0, hm-1
       ix = i; kx = k
       do s = 0, n-1
          if ( kx == 0 ) exit
          t = choose(n-(s+1), kx-1)
          if ( ix < t ) then
             co(i)%combs(kx-1) = s
             kx = kx - 1
          else
             ix = ix - t
          end if
       end do
    end do
    call trace_exit('fractal_comb')
  end subroutine comb

end module fractal

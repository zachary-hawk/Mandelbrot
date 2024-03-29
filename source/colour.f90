!!---- File documented by Fortran Documenter, Z.Hawkhead
module colours
  use IO
  use iso_fortran_env, only: real64
  use trace
  implicit none
  !integer,parameter :: dp=real64

contains 


  subroutine colour_triangle_ineq(z,c,z_old,z_cum,int)
    !==============================================================================!
    !                          T R I A N G L E _ I N E Q                           !
    !==============================================================================!
    ! Subroutine implementing the Triangle Inequality Average colouring scheme     !
    ! for the Mandelbrot set, Julia set and Burining ship fractals. The method is  !
    ! outlined in the thesis at the following link: https://is.gd/tSLabY           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           z,                 intent :: in                                    !
    !           c,                 intent :: in                                    !
    !           z_old,             intent :: in                                    !
    !           z_cum,             intent :: inout                                 !
    !           int,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    complex(dp),intent(in)     :: z,c,z_old
    integer,intent(in)              :: int
    real(dp),intent(inout)              :: z_cum
    real(kind=dp)              :: max,min
    !   call trace_entry("TRIANGLE_INEQ")

    max=abs(abs(z_old**e_default)+abs(c))
    min=abs(abs(z_old**e_default)-abs(c))

    if (max-min.lt.1.0e-3)then
       z_cum=z_cum
    else
       z_cum=(z_cum+((abs(z)-min)/(max-min)))
    end if

    !    call trace_exit("TRIANGLE_INEQ")
  end subroutine colour_triangle_ineq




  subroutine colour_ave_angle(z,theta)
    !==============================================================================!
    !                              A V E _ A N G L E                               !
    !==============================================================================!
    ! Subroutine implementing the average angle colouring scheme for the           !
    ! Mandelbrot set, Julia set and Burning ship fractals. The colour is           !
    ! determined by the average arg(z) of the complex numbers forming the orbit    !
    ! of each point.                                                               !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           z,                 intent :: in                                    !
    !           theta,             intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    complex(dp),intent(in)     :: z
    real(dp),intent(inout)                   :: theta
    !    call trace_entry("AVE_ANGLE")
    if (abs(real(z,dp)).gt.0)then

       theta=theta+atan(aimag(z)/real(z,dp))
    else
       theta=theta
    end if
    !    call trace_exit("AVE_ANGLE")
  end subroutine colour_ave_angle



  subroutine colour_smooth_iter(k,z,colour)
    !==============================================================================!
    !                     C O L O U R _ S M O O T H _ I T E R                      !
    !==============================================================================!
    ! Subroutine defining the colouring algorithm for smoothing.                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           k,                 intent :: in                                    !
    !           z,                 intent :: in                                    !
    !           colour,            intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none
    complex(dp),intent(in) :: z
    integer,intent(in)               :: k
    real(dp),intent(inout)               :: colour
    !call trace_entry("SMOOTH_ITER")
    colour=k+1.0_dp+(1.0_dp/log(e_default))*log(log(bail_out)/log(z))

    if (isnan(colour))colour=0.0_dp
    !colour=k-((log(abs(z))/log(bail_out))/log(e_default))
    !call trace_exit("SMOOTH_ITER")
  end subroutine colour_smooth_iter

  subroutine colour_set(k,colour)
    !==============================================================================!
    !                             C O L O U R _ S E T                              !
    !==============================================================================!
    ! Subroutine defining the colouring algorithm that produces images with no     !
    ! colouring of the points outside the set.                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           k,                 intent :: in                                    !
    !           colour,            intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none

    integer,intent(in)               :: k
    real(dp),intent(inout)               :: colour

    if (k.eq.max_iter+1)then
       colour=1.0_dp
    else
       colour=0.0_dp
    end if

  end subroutine colour_set

  subroutine colour_exponential(z,colour_ref)
    !==============================================================================!
    !                     C O L O U R _ E X P O N E N T I A L                      !
    !==============================================================================!
    ! Subroutine defining the colouring algorithm that implements the              !
    ! exponential smoothing method.                                                !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           z,                 intent :: in                                    !
    !           colour_ref,        intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none
    complex(dp),intent(in)     :: z
    real(dp),intent(inout)                   :: colour_ref

    colour_ref=colour_ref+exp(-abs(z))
    if (isnan(colour_ref)) colour_ref=000
  end subroutine colour_exponential
  
  subroutine colour_light(z,k_real,i,dir)
    !==============================================================================!
    !                           C O L O U R _ L I G H T                            !
    !==============================================================================!
    ! Subroutine defining the colouring algorithm that renders to look 3D with a   !
    ! light source shining on it from a 45 deg angele. Currently not working.      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           z,                 intent :: in                                    !
    !           k_real(dp),            intent :: inout                                 !
    !           i,                 intent :: in                                    !
    !           dir,               intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none
    complex(dp),intent(in)     :: z
    real(dp),intent(inout)                   :: k_real
    integer,intent(in)                   :: i

    complex(dp)                :: v,u
    complex(dp)                :: dc=(1,0)
    complex(dp),intent(inout)  :: dir
    real(dp)                                 :: h=1.5
    k_real=0
    v=exp(cmplx(0,1)*light_angle*2*pi/360)

    if (i.eq.0)dir=dc
    dir=dir*e_default*z**(e_default-1)+dc

    if (abs(z).gt.bail_out)then

       u = z/dir
       u = u/abs(u)

       k_real = real(u)*real(v) + aimag(u)*aimag(v) + h
       k_real = k_real/(1.0_dp+h) 

       if (k_real.lt.0) k_real=0.0_dp
    end if
  end subroutine colour_light

  

end module colours

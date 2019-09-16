!---- File documented by Fortran Documenter, Z.Hawkhead
module colours
  use IO
  use iso_fortran_env
  use trace
  implicit none


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
    complex(complex_kind),intent(in)     :: z,c,z_old
    integer,intent(in)              :: int
    real,intent(inout)              :: z_cum
    real(kind=real128)              :: max,min
!   call trace_entry("TRIANGLE_INEQ")



    max=abs(abs(z_old**e_default)+abs(c))
    min=abs(abs(z_old**e_default)-abs(c))


    if (max-min.lt.1.0e-3)then
       z_cum=z_cum
    else

       z_cum=(z_cum+(abs(z)-min)/(max-min))

    end if

    if (int.gt.max_iter-1)z_cum=0
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
    complex(complex_kind),intent(in)     :: z
    real,intent(inout)                   :: theta
!    call trace_entry("AVE_ANGLE")
    if (abs(real(z)).gt.0)then

       theta=theta+atan(aimag(z)/real(z))
    else
       theta=theta
    end if
!    call trace_exit("AVE_ANGLE")
  end subroutine colour_ave_angle

  

  subroutine colour_smooth_iter(k,z,colour)
    implicit none
    complex(complex_kind),intent(in) :: z
    integer,intent(in)               :: k
    real,intent(inout)               :: colour
!    call trace_entry("SMOOTH_ITER")
    colour=k-((log(abs(z))/log(bail_out))/log(e_default))
    !    if (colour.gt.100)colour=100
!    call trace_exit("SMOOTH_ITER")
  end subroutine colour_smooth_iter

  subroutine colour_set(k,colour)
    implicit none

    integer,intent(in)               :: k
    real,intent(inout)               :: colour

    if (k.eq.max_iter+1)then
       colour=1
    else
       colour=0
    end if

  end subroutine colour_set

  subroutine colour_exponential(z,colour_ref)
    implicit none
    complex(complex_kind),intent(in)     :: z
    real,intent(inout)                   :: colour_ref

    colour_ref=colour_ref+exp(-abs(z))
  end subroutine colour_exponential

  subroutine colour_light(z,k_real,i,dir)
    implicit none
    complex(complex_kind),intent(in)     :: z
    real,intent(inout)                   :: k_real
    integer,intent(in)                   :: i

    complex(complex_kind)                :: v,u
    complex(complex_kind)                :: dc=(1,0)
    complex(complex_kind),intent(inout)  :: dir
    real                                 :: h=1.5
    k_real=0
    v=exp(cmplx(0,1)*light_angle*2*pi/360)

    if (i.eq.0)dir=dc
    dir=dir*e_default*z**(e_default-1)+dc

    if (abs(z).gt.bail_out)then

       u = z/dir
       u = u/abs(u)

       k_real = real(u)*real(v) + aimag(u)*aimag(v) + h
       k_real = k_real/(1+h) 

       if (k_real.lt.0) k_real=0
    end if
  end subroutine colour_light

  
end module colours

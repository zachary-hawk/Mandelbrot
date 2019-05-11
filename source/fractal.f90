module fractal
implicit none


contains

  function mand(Max_iter,z,c,e) result(k)
    implicit none
    integer,intent(in)::Max_iter 
    complex::z,c
    integer::k,e

    do k=0,Max_iter
       z = z**e+c
       if (abs(z).gt.2) then
          exit
       end if
    end do


  end function mand


  function julia(Max_iter,c,z,e) result(k)
    integer,intent(in)::Max_iter 
    complex::z,c
    integer::k,e

    do k=0,Max_iter
       z = z**e+c
       if (abs(z).gt.2) then
          exit
       end if
    end do

  end function julia


end module fractal

module IO
  implicit none
  integer :: N_d=1000
  integer :: Max_iter_d=50
  real :: e_default_d=2.
  integer :: budda_param_d=10000
  double precision :: lower_X_d=-2.0
  double precision :: upper_X_d=0.5
  double precision :: lower_Y_d=-1.25
  double precision :: upper_Y_d=1.25
  logical :: lookfor_parallel_d=.FALSE.
  logical :: lookfor_data_d=.TRUE.
  logical :: lookfor_eff_d=.FALSE.
  logical :: j_for_carrying_d=.FALSE.
  logical :: b_for_carrying_d=.FALSE.
  logical :: lookfor_cont_d=.FALSE.





contains

  subroutine READ_PARAMETERS(N,Max_iter,e_default,budda_param,z_re,z_im,lower_x,lower_y,&
       upper_x,upper_y,lookfor_parallel,lookfor_data,lookfor_eff,j_for_carrying,b_for_carrying&
       ,param,lookfor_cont)
    !------------------------------------------------
    !   Subroutine for reading the input parameters
    !
    !   Grid_size : int
    !   Max_iter : int
    !   plot_parallel : logical
    !   write_data : logical
    !   write_efficiency : logical
    !   calc_type : Mandelbrot, Julia, Buddahbrot
    !   Julia_const : complex
    !   Buddah_const : int
    !   x_low : real
    !   x_up : real
    !   y_low : real
    !   y_up : real 
    !   
    !   
    !
    !------------------------------------------------

    integer :: IOstatus=0,i,counter
    logical :: file_exist
    character(len=20) :: chara,name,val,z_re_char,z_im_char
    character(len=1) :: junk
    real,intent(out) :: z_re,z_im
    complex :: julia_const

    integer,intent(out) :: N
    integer,intent(out) :: Max_iter
    real,intent(out) :: e_default
    integer,intent(out) :: budda_param
    double precision,intent(out) :: lower_X
    double precision,intent(out) :: upper_X
    double precision,intent(out) :: lower_Y
    double precision,intent(out) :: upper_Y
    logical,intent(out) :: lookfor_parallel
    logical,intent(out) :: lookfor_data
    logical,intent(out) :: lookfor_eff
    logical,intent(out) :: j_for_carrying
    logical,intent(out) :: b_for_carrying
    logical,intent(out) :: param
    logical,intent(out) :: lookfor_cont
    N=N_d
    Max_iter=Max_iter_d
    e_default= e_default_d
    budda_param=budda_param_d
    lower_X=lower_X_d
    upper_X=upper_X_d
    lower_Y=lower_Y_d
    upper_Y=upper_Y_d
    lookfor_parallel=lookfor_parallel_d
    lookfor_cont=lookfor_cont_d
    lookfor_data=lookfor_data_d
    lookfor_eff=lookfor_eff_d
    j_for_carrying=j_for_carrying_d
    b_for_carrying=b_for_carrying_d

    inquire(file="param.mand", EXIST=file_exist)
    if (file_exist)then 
       open(unit=24,file="param.mand",status="OLD",access="stream",form="formatted")
       param=.TRUE.
       do while (IOstatus .eq. 0) 

          read(24,*,IOSTAT=IOstatus)name,junk,val!chara

          name=string_tolower(name)
          val=string_tolower(val)
          if (adjustl(name).eq."grid_size")then
             read(val,'(i6)')N
          elseif (name.eq."max_iter")then
             read(val,'(i12)')max_iter
          elseif (name.eq."exponent")then
             read(val,'(f5.2)')e_default
          elseif (name.eq."buddah_const".or.name.eq."Buddah_const")then
             read(val,'(i12)')budda_param
             !b_for_carrying=.TRUE.
          elseif (name.eq."julia_const")then

             read(val,*)julia_const
             z_re=real(julia_const)
             z_im=aimag(julia_const)

          elseif (name.eq."x_low")then
             read(val,'(f15.10)')lower_x
          elseif (name.eq."x_up")then
             read(val,'(f15.10)')upper_x
          elseif (name.eq."y_low")then
             read(val,'(f15.10)')lower_y
          elseif (name.eq."y_up")then
             read(val,'(f15.10)')upper_y
          elseif(name.eq."plot_parallel")then
             if (val.eq."true".or.val.eq."True")then
                lookfor_parallel=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_parallel=.FALSE.
             else
                call warning(name,val)
             end if
          elseif(name.eq."continuation")then
             if (val.eq."true".or.val.eq."True")then
                lookfor_cont=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_cont=.FALSE.
             else
                call warning(name,val)
             end if
          elseif(name.eq."write_data")then
             if(val.eq."true".or.val.eq."True")then
                lookfor_data=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_data=.FALSE.
             else
                call warning(name,val)
             end if
          elseif(name.eq."write_efficiency")then
             if(val.eq."true".or.val.eq."True")then
                lookfor_eff=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_eff=.FALSE.
             else
                call warning(name,val)
             end if
          elseif(name.eq."calc_type".or.name.eq."Calc_type")then
             if(val.eq."buddahbrot".or.val.eq."Buddahbrot")then
                b_for_carrying=.TRUE.
             elseif(val.eq."julia".or.val.eq."Julia")then
                j_for_carrying=.TRUE.
             elseif(val.eq."Mandelbrot".or.val.eq."mandelbrot")then
                b_for_carrying=.FALSE.
                j_for_carrying=.FALSE.
             else
                call Warning(name,val)
             end if
          elseif (name(1:1).eq."#".or.name(1:1).eq."!")then
             cycle

          else
             write(*,*) "Warning: Unknown parameter- ", name

          end if
       end do
    end if
    !call params()
  end subroutine READ_PARAMETERS






  subroutine warning(name,val)
    implicit none
    character(*)::name,val

    write(*,*)"Warning: Unknown argument '",trim(val),"' for parameter: ",trim(name)
49  format(1x,A,A,A,1x,A)
    write(*,*) "Reverting to default"

  end subroutine warning

  subroutine params()
    write(*,*) "PARAMETERS:"
    write(*,*)
73  format(1x,A16,5x,A,5x,A) !Paramter
74  format(1x,A16,5x,A,5x,A) !default statement for character val
75  format(1x,A16,5x,A,5x,i5) !default statement for integer   
76  format(1x,A16,5x,A,5x,f5.2) !default statement for real           
78  format(1x,A16,5x,a,5x,A)


    write(*,73) adjustl("CALC_TYPE"),":","Toggle the desired set"
    write(*,78) "Allowed",":","Mandelbrot, Julia, Buddahbrot"
    write(*,74) "Default",":","Mandelbrot"
    write(*,*)

    write(*,73) adjustl("GRID_SIZE"),":","Specify No. grid points"
    write(*,78) "Allowed",":","any int > 1"
    write(*,75) "Default",":",N_d
    write(*,*)

    write(*,73) adjustl("MAX_ITER"),":","Specify maximum interations"
    write(*,78) "Allowed",":","any int > 1"
    write(*,75) "Default",":", Max_iter_d
    write(*,*) 

    write(*,73) adjustl("EXPONENT"),":","Specify exponent for calculation"
    write(*,78) "Allowed",":","any"
    write(*,76) "Default",":", e_default_d
    write(*,*)

    write(*,73) adjustl("BUDDAH_CONST"),":","Specify No. initial posotions in Buddahbrot calculation"
    write(*,78) "Allowed",":","any int > 0"
    write(*,75) "Default",":",budda_param_d
    write(*,*)

    write(*,73) adjustl("JULIA_CONST"),":","Specify Julia set constant, quotes required"
    write(*,78) "Allowed",":","any complex"
    write(*,74) "Default",":","'(0.0,0.0)'"
    write(*,*)

    write(*,73) adjustl("X_UP"),":","Sepcify maximum x co-ordinate"
    write(*,78) "Allowed",":","any real > X_LOW"
    write(*,76) "Default",":",upper_x_d
    write(*,*)

    write(*,73) adjustl("X_LOW"),":","Specify minimin x co-ordinate"
    write(*,78) "Allowed",":","any real < X_UP"
    write(*,76) "Default",":",lower_x_d
    write(*,*)

    write(*,73) adjustl("Y_UP"),":","Specify maximim y co-ordinate"
    write(*,78) "Allowed",":","any real > Y_LOW"
    write(*,76) "Default",":",upper_y_d
    write(*,*)

    write(*,73) adjustl("Y_LOW"),":","Specify minimum y co-ordinate"
    write(*,78) "Allowed",":","any real < Y_UP"
    write(*,76) "Default",":",lower_Y_d
    write(*,*)

    write(*,73) adjustl("WRITE_DATA"),":","Write calculation data"
    write(*,78) "Allowed",":","TRUE,FALSE"
    write(*,74) "Default",":","TRUE"
    write(*,*)

    write(*,73) adjustl("WRITE_EFFICIENCY"),":","Write efficiency data"
    write(*,78) "Allowed",":","TRUE,FASLE"
    write(*,74) "Default",":","FALSE"
    write(*,*)

    write(*,73) adjustl("PLOT_PARALLEL"),":","Colour Mandelbrot/Julia calculation by processor"
    write(*,78) "Allowed",":","TRUE,FALSE"
    write(*,74) "Default",":","FALSE"

  end subroutine params


  function string_tolower( string ) result (new) 
    character(len=*)           :: string 

    character(len=len(string)) :: new 

    integer                    :: i 
    integer                    :: k 
    integer::length
    length = len(string) 
    new    = string 
    do i = 1,len(string) 
       k = iachar(string(i:i)) 
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then 
          k = k + iachar('a') - iachar('A') 
          new(i:i) = achar(k) 
       endif
    enddo
  end function string_tolower


  subroutine print_dry(file)
    integer :: file
    write(file,*) "                      ***************************************"
    write(file,*) "                      *     Dryrun Complete: Finishing      *"
    write(file,*) "                      ***************************************"
    stop
  end subroutine print_dry



end module IO

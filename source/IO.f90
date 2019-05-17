module IO
  implicit none

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

    integer :: IOstatus,i,counter
    logical :: file_exist
    character(len=20) :: chara,name,val,z_re_char,z_im_char
    character(len=1) :: junk
    real,intent(out) :: z_re,z_im
    complex :: julia_const

    integer,intent(out) :: N
    integer,intent(out) :: Max_iter
    integer,intent(out) :: e_default
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
    N=1000
    Max_iter=50
    e_default=2 
    budda_param=1000
    lower_X=-2.0
    upper_X=0.5
    lower_Y=-1.25
    upper_Y=1.25
    lookfor_parallel=.false.
    lookfor_cont=.false.
    lookfor_data=.false.
    lookfor_data=.true.
    lookfor_eff=.false.
    j_for_carrying=.false.
    b_for_carrying=.false.



    inquire(file="param.mand", EXIST=file_exist)

    if (file_exist)then 
       open(unit=24,file="param.mand")
       param=.TRUE.
       do while (IOstatus .eq. 0) 
          read(24,*,IOSTAT=IOstatus)name,junk,val!chara


          if (adjustl(name).eq."grid_size")then
             read(val,'(i5)')N
          elseif (name.eq."max_iter")then
             read(val,'(i12)')max_iter
          elseif (name.eq."exponent")then
             read(val,'(i12)')e_default
          elseif (name.eq."buddah_const".or.name.eq."Buddah_const")then
             read(val,'(i12)')budda_param
             b_for_carrying=.TRUE.
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
             if (val.eq."true")then
                lookfor_parallel=.TRUE.
             elseif (val.eq."false")then
                lookfor_parallel=.FALSE.
             end if
          elseif(name.eq."continuation")then
             if (val.eq."true")then
                lookfor_cont=.TRUE.
             elseif (val.eq."false")then
                lookfor_cont=.FALSE.
             end if
          elseif(name.eq."write_data")then
             if(val.eq."true")then
                lookfor_data=.TRUE.
             elseif (val.eq."false")then
                lookfor_data=.FALSE.
             end if
          elseif(name.eq."write_efficiency")then
             if(val.eq."true")then
                lookfor_eff=.TRUE.
             elseif (val.eq."false")then
                lookfor_eff=.FALSE.
             end if
          elseif(name.eq."calc_type".or.name.eq."Calc_type")then
             if(val.eq."buddah".or.val.eq."Buddah")then
                b_for_carrying=.TRUE.
             elseif(val.eq."julia".or.val.eq."Julia")then
                j_for_carrying=.TRUE.
             end if

          else
             write(*,*) "Warning: Unknown parameter- ", name
             
          end if
       end do
    end if

  end subroutine READ_PARAMETERS





  end module IO

!---- File documented by Fortran Documenter, Z.Hawkhead
!----file documented by Fortran Documenter, Z.Hawkhead
!=============================================================================!
!                                     IO                                      !                                                                                                            
!=============================================================================!                                                                                                                            
!              Module handining input/output for Mandelbrot                   !                                                                                                                            
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================!
module IO
  use iso_fortran_env
  use comms , only : rank,max_version_length,comms_arch,COMMS_VERSION &
       & ,COMMS_LIBRARY_VERSION,COMMS_FINALISE,comms_abort,comms_barrier
  use trace
  implicit none
!!!!!! DEFINE THE DEFAULTS !!!!!!!!!
  integer          :: N=1000
  integer          :: Max_iter=50
  real             :: relaxation=1
  real             :: e_default=2.
  real             :: e_rational=-2
  real             :: lambda=-0.35
  real             :: bail_out=1.e5
  real             :: light_angle=45
  real,parameter   :: pi=3.141592654             
  integer          :: buddah_param=10000
  integer          :: stdout
  real             :: zoom_factor=1
  logical          :: lookfor_parallel=.FALSE.
  logical          :: lookfor_data=.TRUE.
  logical          :: lookfor_eff=.FALSE.
  logical          :: j_for_carrying=.FALSE.
  logical          :: burn_for_carrying=.FALSE.
  logical          :: b_for_carrying=.FALSE.
  logical          :: lookfor_cont=.FALSE.
  logical          :: newt_for_carrying=.FALSE.
  logical          :: continuation=.FALSE.
  logical          :: file_exist
  logical          :: triangle=.false.
  logical          :: exponential=.false.
  logical          :: simple_set=.false.
  logical          :: ave_an=.false.
  logical          :: smooth=.true.
  logical          :: light=.false.
  logical          :: do_mandelbrot=.true.
  logical          :: do_magnet=.false.
  logical          :: do_phoenix=.false.
  logical          :: do_nova=.false.
  logical          :: do_rational
  logical          :: debug=.false.
  logical          :: zoom=.false.
  complex          :: julia_const=(0.285,0.01)
  character(81)    :: parser_version="Mandelbrot v.3.0, Z.Hawkhead" 
  character(100)   :: info="Parallel code for calculating the Mandelbrot Set"
  character(100)   :: DATE,TIME,compiler,arch_string,version,cpuinfo
  integer,parameter:: complex_kind=compiler_kind
  real(complex_kind):: lower_X=-2.0
  real(complex_kind):: upper_X=0.5
  real(complex_kind):: lower_Y=-1.25
  real(complex_kind):: upper_Y=1.25

  complex(complex_kind):: centre=(0,0)

  !  public :: triangle,ave_ang



contains



  subroutine io_header()
    !==============================================================================!
    !                                 H E A D E R                                  !
    !==============================================================================!
    ! Subroutine used to write the io_header of the main Mandelbrot output file    !
    ! "out.mand".                                                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    integer::file
    integer :: maj_mpi,min_mpi,min_char
    character(len=max_version_length) :: mpi_c_version
    character(len=3) :: MPI_version_num
    call trace_entry("IO_HEADER")



    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
       call COMMS_VERSION(min_mpi,maj_mpi)

       write(mpi_version_num,97)min_mpi,maj_mpi
97     format(i1,"."i1)
       min_char=scan(mpi_c_version,".")
       !print*, mpi_c_version,mpi_version_num
    end if

#ifdef __INTEL_COMPILER
#define compiler "Intel Compiler"

#endif

    if (compiler.eq."Intel Compiler")then
       version=compiler_version()
       version=trim(version(87:97))
    end if

#ifdef __GFORTRAN__
#define compiler "GNU Fortran"
#define version __VERSION__
#endif

#ifdef arch
#define arch_string arch
#endif

#ifdef cpu
#define cpuinfo cpu
#endif

    if (len(arch).eq.0)  print*,"Could not determine system"

    write(stdout,*) "+==================================================================================+"
    write(stdout,*) "| MM    MM   AAA   NN   NN DDDDD   EEEEEEE LL      BBBBB   RRRRRR   OOOOO  TTTTTTT |"
    write(stdout,*) "| MMM  MMM  AAAAA  NNN  NN DD  DD  EE      LL      BB   B  RR   RR OO   OO   TTT   |"
    write(stdout,*) "| MM MM MM AA   AA NN N NN DD   DD EEEEE   LL      BBBBBB  RRRRRR  OO   OO   TTT   |"
    write(stdout,*) "| MM    MM AAAAAAA NN  NNN DD   DD EE      LL      BB   BB RR  RR  OO   OO   TTT   |"
    write(stdout,*) "| MM    MM AA   AA NN   NN DDDDDD  EEEEEEE LLLLLLL BBBBBB  RR   RR  OOOO0    TTT   |"
    write(stdout,*) "|                                                                                  |"
    write(stdout,*) "| ",parser_version,"|"
    write(stdout,*) "+==================================================================================+"
    write(stdout,*)
    write(stdout,*) "Compiled with ",compiler," ",Trim(version), " on ", __DATE__, " at ",__TIME__
    write(stdout,*) "Compiled for CPU: ",trim(cpuinfo)
    write(stdout,*) "Compiled for system: ",trim(arch_string)
    write(stdout,"(1x,A,i3)") "Complex precision: ",8*complex_kind
    write(stdout,*) "Communications architechture: ",trim(comms_arch)
    if (comms_arch.eq."MPI")then
       write(stdout,*) "MPI Version: ",mpi_c_version(1:min_char+1)
    end if
    write(stdout,*)
    call trace_exit("IO_HEADER")
  end subroutine io_header

  subroutine io_file(nprocs,memory_buffer,disk_stor,file_name)
    !==============================================================================!
    !                                   F I L E                                    !
    !==============================================================================!
    ! Subroutine used to write the main body of the Mandelbrot output io_file,     !
    ! "out.mand".                                                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           nprocs,            intent :: in                                    !
    !           memory_buffer,     intent :: in                                    !
    !           disk_stor,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)    
    character*10                    :: b(3)
    integer,intent(in)              :: nprocs
    real,intent(in)                 :: memory_buffer,disk_stor
    character(*),intent(in)         :: file_name

    call trace_entry("IO_FILE")
    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    if(lookfor_data)then
       open(unit=2,file="data.mand",form="UNFORMATTED")
    end if
    open(unit=stdout,file="out.mand",RECL=8192,form="FORMATTED")
    call io_header()

    write(stdout,1000) months(d_t(2)),d_t(3),d_t(1),d_t(5),d_t(6),d_t(7),d_t(8)
1000 format (' Calculation started:  ', A, 1x, i2.2, 1x, i4.4, ' at ',i2.2, ':', i2.2, ':', i2.2 ,".",i3.3)
    write(stdout,*)
    if (file_exist)then
       write(stdout,*) "Reading Parameters file: "//file_name//".mand"
    else
       write(stdout,*) "No Parameters file found, using defaults"
    end if

    if(b_for_carrying)then
       if(continuation)then
          write(stdout,*)"Restarting from previous calculation"
       end if
    end if
    write(stdout,*)
    write(stdout,*)"------------------------------------ Parameters ------------------------------------"
    write(stdout,*)
2001 format(1x,A37,5x,':',A15)   !Format for Characters
2002 format(1x,A37,5x,':',i15)   !Format for integer parameters
2003 format(1x,A37,5x,':',f15.2) !Format for real parameters
2004 format(1x,A37,5x,':',ES15.2)!Format for scientific paramteters
2005 format(1x,A37,5x,':',F8.3,SP,F6.3,"i")
2006 format(23x,A42)

    write(stdout,2006)"----------- Fractal Parameters -----------"
    write(stdout,*)


    if (j_for_carrying)then
       write(stdout,2001) "Calculation Type","Julia"
    else if (b_for_carrying)then
       write(stdout,2001) "Calculation Type","Buddahbrot"
    elseif (burn_for_carrying)then
       write(stdout,2001) "Calculation Type","Burning Ship"
    elseif (do_magnet)then
       write(stdout,2001) "Calculation Type","Magnet"
    elseif (do_phoenix)then
       write(stdout,2001) "Calculation Type","Phoenix"
    elseif (do_nova)then
       write(stdout,2001) "Calculation Type","Nova"
    elseif (do_rational)then
       write(stdout,2001) "Calculation Type","Rational"
    elseif (newt_for_carrying)then
       write(stdout,2001) "Calculation Type","Newton"
    else
       write(stdout,2001) "Calculation Type","Mandelbrot"
    endif
    if (do_nova.or.newt_for_carrying)then
       write(stdout,2003) "Relaxation Const", relaxation
    end if
    if (b_for_carrying) write(stdout,2002) "No. Initial positions:",buddah_param

    if(.not.do_rational)write(stdout,2003)"Exponent",e_default
    if(do_rational)then
       write(stdout,2003)"Exponent 1",e_default
       write(stdout,2003)"Exponent 2",e_rational
       write(stdout,2003)"Lambda",lambda
    end if
    if (j_for_carrying.or.burn_for_carrying.or.do_phoenix.or.do_nova.or.do_rational) &
         write(stdout,2005) "Complex seed",julia_const
    if (.not.newt_for_carrying) write(stdout,2004) "Bail Out",bail_out
    write(stdout,2002) "Maximum Iterations",max_iter
    write(stdout,*)
    write(stdout,2006)"------------ Image Parameters ------------"
    write(stdout,*)	

    write(stdout,2002)"No. Grid points",N
    if (zoom)then
       write(stdout,2003)"Zoom Factor",zoom_factor
       write(stdout,2005)"Zoom Centre",centre
    end if
    write(stdout,2003)"Lower X",lower_x
    write(stdout,2003)"Upper X",upper_x
    write(stdout,2003)"Lower Y",lower_y
    write(stdout,2003)"Upper Y",upper_y

    if (j_for_carrying.or.burn_for_carrying.or.do_mandelbrot.or.do_magnet.or.do_phoenix)then

       write(stdout,*)
       write(stdout,2006)"------------ Colouring Scheme ------------"
       write(stdout,*)

       if (triangle)then
          write(stdout,2001) "Colouring Method","Triangle"
       elseif (ave_an)then 
          write(stdout,2001) "Colouring Method","Average Angle"
       elseif(simple_set)then
          write(stdout,2001) "Colouring Method","Simple Set"
       elseif(exponential)then
          write(stdout,2001) "Colouring Method","Exponential"
       elseif(smooth)then
          write(stdout,2001) "Colouring Method","Normal"
       elseif(light)then
          write(stdout,2001) "Colouring Method","Light"
       end if
       if (nprocs.gt.1)then
          if (lookfor_PARALLEL .and. lookfor_data.and..not.b_for_carrying)then
             write(stdout,2001) "MPI Colouring","True"
          else
             write(stdout,2001) "MPI Colouring","False"
          end if
       end if
    end if

    if (comms_arch.eq."MPI")then
       write(stdout,*)
       write(stdout,2006)"------------ Parallelisation -------------"
       write(stdout,*)

       if (nprocs.gt.1)then
          write(stdout,2001)"Parallelised","True"
          write(stdout,2002)"No. Processes", nprocs
       else
          write(stdout,2001)"Parallelised","False"
       end if
    end if
    write(stdout,*)
    write(stdout,2006)"----------------- Output -----------------"
    write(stdout,*)

    if (lookfor_eff)then
       write(stdout,2001) "Efficiency","True"
    else
       write(stdout,2001) "Efficiency","False"
    end if
    if (lookfor_data)then
       write(stdout,2001) "Write Data","True"
    else
       write(stdout,2001) "Write Data","False"
    end if
    write(stdout,*)
    write(stdout,*)"------------------------------------------------------------------------------------"
    write(stdout,*)
    write(stdout,2003) "Estimated memory usage (MB)",memory_buffer
    write(stdout,2003) "Estimated disk storage (MB)",disk_stor

    write(stdout,*)

    if (do_nova)newt_for_carrying=.true.


    call trace_exit("IO_FILE")
  end subroutine io_file


  subroutine io_read_parameters(file_name)
    !==============================================================================!
    !                        R E A D _ P A R A M E T E R S                         !
    !==============================================================================!
    ! Subroutine for reading free format parameters file for Mandelbrot,           !
    ! contained in "param.mand".                                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(*),intent(in)      :: file_name

    integer                      :: i,counter,j,stat=0,max_stat
    character(len=30)            :: chara,name,val,z_re_char,z_im_char,junk
    character(len=5)             :: out_1,out_2
    logical                      :: change_ux=.FALSE.
    logical                      :: change_lx=.FALSE.
    logical                      :: change_uy=.FALSE.
    logical                      :: change_ly=.FALSE.
    logical                      :: change_centre=.FALSE.
    logical                      :: change_exp=.FALSE.
    logical                      :: change_iter=.FALSE.
    logical                      :: change_julia=.FALSE.
    logical                      :: change_magnet=.FALSE.
    logical                      :: temp_logical
    call trace_entry("IO_READ_PARAMETERS")





    inquire(file=file_name//".mand", EXIST=file_exist)
    if (file_exist)then 
       open(unit=24,file=file_name//".mand",status="OLD",access="stream",form="formatted")

       do while (max_stat .eq. 0) 

          read(24,'(A)',IOSTAT=max_stat)junk
          if (junk(1:1).eq."!")cycle
          do j=1,len_trim(junk)

             if (junk(j:j).eq.':' .or. junk(j:j).eq."=")then
                name=junk(1:j-1)
                val=junk(j+1:len_trim(junk))

                exit
             elseif (j.eq.len_trim(junk))then
                call io_errors("User I/O Error"//trim(junk))                
             endif
          enddo



          name=io_string_to_lower(name)
          val=io_string_to_lower(val)

          name=adjustl(name)
          val=adjustl(val)

          if (adjustl(name).eq."grid_size")then
             call io_scientific_corr(val)
             read(val,*,iostat=stat)N
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
          elseif (name.eq."max_iter")then
             !     call io_scientific_corr(val)
             read(val,*,iostat=stat)max_iter
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             change_iter=.TRUE.
          elseif (name.eq."exponent")then
             call io_split_params(val,out_1,out_2,temp_logical)
             if (temp_logical)then
                call io_int_to_real(out_1)
                call io_int_to_real(out_2)
                read(out_1,*,iostat=stat)e_default
                if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(out_1))
                read(out_2,*,iostat=stat)e_rational
                if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(out_2))
                change_exp=.true.
             else
                call io_int_to_real(val)
                read(val,*,iostat=stat)e_default
                if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
                change_exp=.true.
             end if
          elseif (name.eq."lambda")then
             call io_int_to_real(val)
             !      call io_scientific_corr(val)
             read(val,*,iostat=stat)lambda

             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
          elseif (name.eq."buddah_const".or.name.eq."Buddah_const")then
             read(val,*,iostat=stat)buddah_param
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
          elseif (name.eq."complex_seed")then
             read(val,*,iostat=stat)julia_const
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             change_julia=.TRUE.
          elseif (name.eq."relaxation")then
             call io_int_to_real(val)
             read(val,*,iostat=stat)relaxation
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
          elseif (name.eq."x_low")then
             call io_int_to_real(val)
             read(val,*,iostat=stat)lower_x
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             change_lx=.true.
          elseif (name.eq."x_up")then
             call io_int_to_real(val)
             !       call io_scientific_corr(val)
             read(val,*,iostat=stat)upper_x
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             change_ux=.true.
          elseif (name.eq."y_low")then
             call io_int_to_real(val)
             !       call io_scientific_corr(val)
             read(val,*,iostat=stat)lower_Y
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             change_ly=.true.        
          elseif (name.eq."y_up")then
             call io_int_to_real(val)
             !       call io_scientific_corr(val)
             read(val,*,iostat=stat)upper_Y
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             change_uy=.true.       
          elseif (name.eq."debug")then
             if(val.eq."true")then
                debug=.true.
             elseif(val.eq."false")then
                debug=.false.
             end if
          elseif (name.eq."colouring_method")then
             if (val.eq."triangle".or.val.eq."tia")then 
                triangle=.true.
                smooth=.false.
             elseif (val.eq."angle")then
                ave_an=.true.
                smooth=.false.
             elseif(val.eq."normal")then 
                smooth=.true.
             elseif (val.eq."simple_set")then
                simple_set=.true.
                smooth=.false.
             elseif (val.eq."exponential")then
                exponential=.true.
                smooth=.false.
             elseif (val.eq."light")then
                light=.true.
                smooth=.false.
             else
                call io_errors("Error in I/O read from "//file_name//".mand: "//trim(name))
             end if
          elseif (name.eq."zoom_factor")then
             call io_int_to_real(val)
             call io_scientific_corr(val)
             read(val,*,iostat=stat) zoom_factor
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             zoom=.true.
          elseif (name.eq."zoom_centre")then
             read(val,*,iostat=stat)centre
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
             zoom=.true.
          elseif (name.eq."bail_out")then
             call io_int_to_real(val)
             call io_scientific_corr(val)
             read(val,*,iostat=stat)BAIL_OUT
             if (stat.ne.0)call io_errors("Error in I/O read from "//file_name//".mand: "//trim(val))
          elseif(name.eq."plot_parallel")then
             if (val.eq."true".or.val.eq."True")then
                lookfor_parallel=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_parallel=.FALSE.
             else
                call io_errors("Error in I/O read from "//file_name//".mand: "//trim(name))
             end if
          elseif(name.eq."continuation")then
             if (val.eq."true".or.val.eq."True")then
                continuation=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                continuation=.FALSE.
             else
                call io_errors("Error in I/O read from "//file_name//".mand: "//trim(name))
             end if
          elseif(name.eq."write_data")then
             if(val.eq."true".or.val.eq."True")then
                lookfor_data=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_data=.FALSE.
             else
                 call io_errors("Error in I/O read from "//file_name//".mand: "//trim(name))
             end if
          elseif(name.eq."write_efficiency")then
             if(val.eq."true".or.val.eq."True")then
                lookfor_eff=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_eff=.FALSE.
             else
                call io_errors("Error in I/O read from "//file_name//".mand: "//trim(name))
             end if
          elseif(name.eq."calc_type")then
             if(val.eq."buddahbrot")then
                b_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."julia")then
                j_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."newton")then
                newt_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."magnet")then
                do_magnet=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."burning_ship")then
                burn_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."phoenix")then
                do_phoenix=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."nova")then
                do_nova=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."rational")then
                do_rational=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."mandelbrot")then
                b_for_carrying=.FALSE.
                j_for_carrying=.FALSE.
                newt_for_carrying=.FALSE.
                do_mandelbrot=.true.
             else
                call io_errors("Error in I/O read from "//file_name//".mand: "//trim(name))
             end if
          elseif (name(1:1).eq."#".or.name(1:1).eq."!")then
             cycle

          !else
          !   lookfor_warnings=.TRUE.
          !   write(*,*) "Warning: Unknown parameter- ", name

          end if
       end do
    end if



    !Set the Defaults

    if (j_for_carrying.or.do_phoenix.or.do_rational)then
       if (.not.change_lx)lower_x=-2.0
       if (.not.change_ux)upper_x=2.0
       if (.not.change_ly)lower_y=-2.0
       if (.not.change_uy)upper_y=2.0
    endif
    if (do_phoenix)then
       if (.not.change_julia)julia_const=(0.566666,-0.5)
    endif
    if (do_nova.or.do_rational)then
       if (.not.change_julia)julia_const=(0.0,0.0)
    end if
    if (burn_for_carrying)then
       if (.not.change_lx)lower_x=-2.0
       if (.not.change_ux)upper_x=2.0
       if (.not.change_ly)lower_y=-2.
       if (.not.change_uy)upper_y=0.75
       if(.not.change_julia)julia_const=(0.,0.)
    endif
    if (b_for_carrying)then
       upper_x=0.75
       lower_X=-2.0
       lower_Y=-1.25
       upper_Y=1.25
       if (.not.change_iter)  max_iter=1000
    endif
    if (newt_for_carrying.or.do_nova)then 
       if (.not.change_lx)lower_x=-2.0
       if (.not.change_ux)upper_x=2.0
       if (.not.change_ly)lower_y=-2.0
       if (.not.change_uy)upper_y=2.0
       if (.not.change_exp)e_default=3.0
    endif
    if (do_magnet)then
       if (.not.change_lx)lower_x=-1.0
       if (.not.change_ux)upper_x=3.5
       if (.not.change_ly)lower_y=-2.5
       if (.not.change_uy)upper_y=2.5
    end if

    if(do_nova)newt_for_carrying=.true.
    if (e_default.lt.3.and.newt_for_carrying)&
         call io_errors("Error in I/O, Exponent must not be less then 3.0 for Newton-Raphson calculation")

    if (change_lx.or.change_ly.or.change_ux.or.change_uy)then
       if (zoom)&
            call io_errors("Error in I/O, Cannot change extent in a zoom calculation")
    endif
    call trace_exit("IO_READ_PARAMETERS")

    if (zoom.and.debug) max_iter=max_iter+zoom_factor
    if (zoom_factor.eq.0) call io_errors("Error in I/O, zoom_factor must be non-zero")
  end subroutine IO_READ_PARAMETERS






  subroutine io_warning(name,val,lookfor_warnings)
    !==============================================================================!
    !                                W A R N I N G                                 !
    !==============================================================================!
    ! Subroutine for writing warnings to the main output file "out.mand" when      !
    ! unknown parameters are present in the parameters file.                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           name,              intent :: in                                    !
    !           val,               intent :: in                                    !
    !           lookfor_warnings,  intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!

    character(*)::name,val
    logical,intent(inout) :: lookfor_warnings
    call trace_entry("IO_WARNING")
    lookfor_warnings=.TRUE.
    write(*,*)"Warning: Unknown argument '",trim(val),"' for parameter: ",trim(name)
49  format(1x,A,A,A,1x,A)
    write(*,*) "Reverting to default"
    call trace_exit("IO_WARNING")
  end subroutine io_warning

  subroutine io_params()
    !==============================================================================!
    !                                 P A R A M S                                  !
    !==============================================================================!
    ! Subroutine defining the list of parameters, accessible using "--list" flag   !
    ! on the commandline.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!

    write(*,*) "PARAMETERS:"
    write(*,*)
73  format(1x,A16,5x,A,5x,A) !Paramter
74  format(1x,A16,5x,A,5x,A) !default statement for character val
75  format(1x,A16,5x,A,5x,i5) !default statement for integer   
76  format(1x,A16,5x,A,5x,f5.2) !default statement for real           
78  format(1x,A16,5x,a,5x,A)


    write(*,73) adjustl("CALC_TYPE"),":","Toggle the desired set"
    write(*,78) "Allowed",":","Mandelbrot, Julia, Buddahbrot, Newton, Burning_ship, Phoenix, Nova, Magnet, Rational"
    write(*,74) "Default",":","Mandelbrot"
    write(*,*)

    write(*,73) adjustl("GRID_SIZE"),":","Specify No. grid points"
    write(*,78) "Allowed",":","any int > 1"
    write(*,75) "Default",":",N
    write(*,*)

    write(*,73) adjustl("MAX_ITER"),":","Specify maximum interations"
    write(*,78) "Allowed",":","any int > 1"
    write(*,75) "Default",":", Max_iter
    write(*,*) 

    write(*,73) adjustl("EXPONENT"),":",&
         &"Specify exponent for calculation, a second exponent can be provided for rational maps, comma separated"
    write(*,78) "Allowed",":","any"
    write(*,76) "Default",":", e_default
    write(*,76) "Default second",":",e_rational
    write(*,*)

    write(*,73) adjustl("LAMBDA"),":","Specify the weighting for the rational map second exponent"
    write(*,78) "Allowed",":","any real"
    write(*,76) "Default",":",lambda
    write(*,*)

    write(*,73) adjustl("BUDDAH_CONST"),":","Specify No. initial posotions in Buddahbrot calculation"
    write(*,78) "Allowed",":","any int > 0"
    write(*,75) "Default",":",buddah_param
    write(*,*)

    write(*,73) adjustl("COMPLEX_SEED"),":","Specify complex seed"
    write(*,78) "Allowed",":","any complex"
    write(*,74) "Default",":","(0.285,0.01)"
    write(*,*)

    write(*,73) adjustl("X_UP"),":","Sepcify maximum x co-ordinate"
    write(*,78) "Allowed",":","any real > X_LOW"
    write(*,76) "Default",":",upper_x
    write(*,*)

    write(*,73) adjustl("X_LOW"),":","Specify minimin x co-ordinate"
    write(*,78) "Allowed",":","any real < X_UP"
    write(*,76) "Default",":",lower_x
    write(*,*)

    write(*,73) adjustl("Y_UP"),":","Specify maximim y co-ordinate"
    write(*,78) "Allowed",":","any real > Y_LOW"
    write(*,76) "Default",":",upper_y
    write(*,*)

    write(*,73) adjustl("Y_LOW"),":","Specify minimum y co-ordinate"
    write(*,78) "Allowed",":","any real < Y_UP"
    write(*,76) "Default",":",lower_Y
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
    write(*,*)

    write(*,73) adjustl("BAIL_OUT"),":","Set bail out value for fractal iteration"
    write(*,78) "Allowed",":","any real"
    write(*,74) "Default",":","1.0E5"
    write(*,*)

    write(*,73) adjustl("COLOURING_METHOD"),":","Change colouring method for fractal sets"
    write(*,78) "Allowed",":","NORMAL,TRIANGLE,ANGLE,SIMPLE_SET"
    write(*,74) "Default",":","NORMAL"
    write(*,*)

    write(*,73) adjustl("RELAXATION"),":","Specify the relaxation parameter for the Newton fractal"
    write(*,78) "Allowed",":","any real>0"
    write(*,74) "Default",":","1.0"
    write(*,*)

    write(*,73) adjustl("ZOOM_FACTOR"),":","Magnification for a zoom calculation"
    write(*,78) "Allowed",":","any real>0"
    write(*,74) "Default",":","1.0"
    write(*,*)

    write(*,73) adjustl("ZOOM_CENTRE"),":","Centre of zoom calculation"
    write(*,78) "Allowed",":","any complex"
    write(*,74) "Default",":","(0,0)"

  end subroutine io_params


  function io_string_to_lower( string ) result (new) 
    !==============================================================================!
    !                         S T R I N G _ T O L O W E R                          !
    !==============================================================================!
    ! Functional subroutine used to lower the case of inputted strings to          !
    ! prevent ambiguities in reading parameters.                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           strin                                                              !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(len=*)           :: string 

    character(len=len(string)) :: new 

    integer                    :: i 
    integer                    :: k 
    integer::length
    call trace_entry("IO_STRING_TO_LOWER")
    length = len(string) 
    new    = string 
    do i = 1,len(string) 
       k = iachar(string(i:i)) 
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then 
          k = k + iachar('a') - iachar('A') 
          new(i:i) = achar(k) 
       endif
    enddo
    call trace_exit("IO_STRING_TO_LOWER")
  end function io_string_to_lower


  subroutine io_print_dry(on_root)
    !==============================================================================!
    !                              P R I N T _ D R Y                               !
    !==============================================================================!
    ! Subroutine used to terminiate a calculation after initialisation. Used to    !
    ! check input parameters, accessible using "--dryrun" on the commandline.      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           on_root,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    logical :: on_root
    call trace_entry("IO_PRINT_DRY")
    if (on_root)then
       write(stdout,*) "                        ***************************************"
       write(stdout,*) "                        *     Dryrun Complete: Finishing      *"
       write(stdout,*) "                        ***************************************"
    endif
    call COMMS_FINALISE()
    call trace_exit("IO_PRINT_DRY")
    call trace_finalise(debug,rank)
    stop

  end subroutine io_print_dry



  subroutine io_print_help()
    !==============================================================================!
    !                             P R I N T _ H E L P                              !
    !==============================================================================!
    ! Subroutine to print the help information for Mandelbrot, accessible using    !
    ! "--help" on the commandline.                                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    write(*,*) trim(parser_version)
    write(*,*) trim(info)
    write(*,*) '   -v, --version     Print version information and exit'
    write(*,*) '   -h, --help        Print usage information and exit'
    write(*,*) '   -l, --list        Print list of allowed parameters'
    write(*,*) '   -c, --clear       Remove previous output files'
  end subroutine io_print_help

  subroutine io_errors(message)
    !==============================================================================!
    !                                 E R R O R S                                  !
    !==============================================================================!
    ! Subroutine used to call errors and terminate the program when fatal errors   !
    ! are detected in the input parameters.                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           message,           intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    implicit none
    character(*)::message
    !if (rank.eq.0)then
       write(*,*) "Called I/O Error"
       open(20,file="err.mand",status="unknown")
       write(20,*) "Error: ",trim(message)

       call trace_stack(20)
       print*,rank
       call comms_barrier()
       print*,rank
       close(20)

       stop
    !end if
  end subroutine io_errors


  subroutine io_int_to_real(int_real)
    !==============================================================================!
    !                            I N T _ T O _ R E A L                             !
    !==============================================================================!
    ! Functional subroutine for converting characters to integers when read        !
    ! natively as reals in the parameters file.                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           int_real,          intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(*),intent(inout) :: int_real
    integer                    :: j
    logical                    :: decimal=.false.
    call trace_entry("IO_INT_TO_REAL")

    do j=0,len_trim(int_real)
       if (int_real(j:j).eq.".")decimal=.true.
    end do

    if (.not.decimal)int_real=trim(int_real)//"."
    decimal=.false.
    call trace_exit("IO_INT_TO_REAL")
  end subroutine io_int_to_real


  subroutine io_split_params(in_char,out_char1,out_char2,found)
    !==============================================================================!
    !                        I O _ S P L I T _ P A R A M S                         !
    !==============================================================================!
    ! Low level subroutine for parsing parameters, splits parameters from the      !
    ! keyword.                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           in_char,           intent :: in                                    !
    !           out_char1,         intent :: inout                                 !
    !           out_char2,         intent :: inout                                 !
    !           found,             intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none
    character(*),intent(in)       :: in_char
    character(5),intent(inout)    :: out_char1,out_char2
    logical,     intent(inout)    :: found

    integer                       :: i
    call trace_entry("IO_SPLIT_PARAMS")
    do i=1,len(in_char)
       if (in_char(i:i).eq.",")then
          out_char1=in_char(1:i-1)
          out_char2=in_char(i+1:len(in_char))
          found=.true.
          exit
       end if
    end do

    call trace_exit("IO_SPLIT_PARAMS")

  end subroutine io_split_params


  subroutine io_zoom(zoom_check)
    !==============================================================================!
    !                                I O _ Z O O M                                 !
    !==============================================================================!
    ! Subroutine for handling the parsing of the zoom functionality, defines the   !
    ! aspect of the image based on the default aspect, the zoom multiplier and     !
    ! the centre of the image.                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           zoom_check,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none
    logical            :: zoom_check
    real(complex_kind) :: width,height
    integer            :: max_steps=10000

    call trace_entry("IO_ZOOM")
    width=upper_x-lower_x
    height=upper_y-lower_y

    if (zoom_check) then
       width=width*exp(-100*zoom_factor/(max_steps-1))
       !width=width/zoom_factor
       height=height*exp(-100*zoom_factor/(max_steps-1))
       !height=height/zoom_factor
       !print*,1/exp(-100*zoom_factor/(max_steps-1))


       lower_x=real(centre,complex_kind)-width/2
       upper_x=real(centre,complex_kind)+width/2
       lower_y=aimag(centre)-height/2
       upper_y=aimag(centre)+height/2
    end if

    call trace_exit("IO_ZOOM")
  end subroutine io_zoom


  subroutine io_scientific_corr(char)
    !==============================================================================!
    !                     I O _ S C I E N T I F I C _ C O R R                      !
    !==============================================================================!
    ! Subroitine for parsing parameters given in the exponential format, adds a    !
    ! '.' if it is absent in the declaration.                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           char,              intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  01/10/2019                                            !
    !==============================================================================!
    implicit none
    character(*),intent(inout)    :: char

    logical                       :: found_e
    integer                       :: i
    character(len=20)             :: temp,temp2

    call trace_entry("IO_SCIENTIFIC_CORR")

    do i=1,len_trim(char)
       if (char(i:i).eq."e".or.char(i:i).eq."E")then
          found_e=.true.
          exit
       end if
    end do

    if (found_e)then
       temp=char(1:i-1)
       call io_int_to_real(temp)

       temp2=trim(temp)//trim(char(i:len_trim(char)))
    else
       return
    end if


    char=temp2
    if (char(len_trim(char):len_trim(char)).eq.".")then
       char=char(1:len_trim(char)-1)
    endif
    call trace_exit("IO_SCIENTIFIC_CORR")
  end subroutine io_scientific_corr


end module IO


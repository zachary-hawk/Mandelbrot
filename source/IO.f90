!---- File documented by Fortran Documenter, Z.Hawkhead
!=============================================================================!
!                                     IO                                      !                                                                                                            
!=============================================================================!                                                                                                                            
!              Module handining input/output for Mandelbrot                   !                                                                                                                            
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================!
module IO
  use iso_fortran_env
  use comms
  use trace
  implicit none
!!!!!! DEFINE THE DEFAULTS !!!!!!!!!
  integer          :: N=1000
  integer          :: Max_iter=50
  real             :: e_default=2.
  real             :: bail_out=1.e5
  integer          :: buddah_param=10000
  integer          :: stdout
  real(real32)     :: lower_X=-2.0
  real(real32)     :: upper_X=0.5
  real(real32)     :: lower_Y=-1.25
  real(real32)     :: upper_Y=1.25
  logical          :: lookfor_parallel=.FALSE.
  logical          :: lookfor_data=.TRUE.
  logical          :: lookfor_eff=.FALSE.
  logical          :: j_for_carrying=.FALSE.
  logical          :: burn_for_carrying=.FALSE.
  logical          :: b_for_carrying=.FALSE.
  logical          :: lookfor_cont=.FALSE.
  logical          :: lookfor_warnings=.FALSE.
  logical          :: newt_for_carrying=.FALSE.
  logical          :: continuation=.FALSE.
  logical          :: file_exist
  logical          :: triangle=.false.
  logical          :: ave_an=.false.
  logical          :: do_mandelbrot=.true.
  logical          :: debug=.false.
  complex          :: julia_const=(0.285,0.01)
  character(81)    :: parser_version="Mandelbrot v.3.0, Z.Hawkhead" 
  character(100)   :: info="Parallel code for calculating the Mandelbrot Set"
  character(100)   :: DATE,TIME,compiler,arch_string,version,cpuinfo
  integer,parameter:: complex_kind=real32


  !  public :: triangle,ave_ang



contains



  subroutine header()
    !==============================================================================!
    !                                 H E A D E R                                  !
    !==============================================================================!
    ! Subroutine used to write the header of the main Mandelbrot output file       !
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
    call trace_entry("HEADER")
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
    call trace_exit("HEADER")
  end subroutine header

  subroutine File(nprocs,memory_buffer,disk_stor)
    !==============================================================================!
    !                                   F I L E                                    !
    !==============================================================================!
    ! Subroutine used to write the main body of the Mandelbrot output file,        !
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
    call trace_entry("FILE")
    call date_and_time(b(1), b(2), b(3), d_t)
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

    if(lookfor_data)then
       open(unit=2,file="data.mand",form="UNFORMATTED")
    end if
    open(unit=stdout,file="out.mand",RECL=8192,form="FORMATTED")
    call header()

    write(stdout,1000) months(d_t(2)),d_t(3),d_t(1),d_t(5),d_t(6),d_t(7),d_t(8)
1000 format (' Calculation started:  ', A, 1x, i2.2, 1x, i4.4, ' at ',i2.2, ':', i2.2, ':', i2.2 ,".",i3.3)
    write(stdout,*)
    if (file_exist)then
       write(stdout,*) "Reading Parameters file"
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
    elseif (newt_for_carrying)then
       write(stdout,2001) "Calculation Type","Newton"
    elseif (burn_for_carrying)then
       write(stdout,2001) "Calculation Type","Burning Ship"
    else
       write(stdout,2001) "Calculation Type","Mandelbrot"
    endif

    if (b_for_carrying) write(stdout,2002) "No. Initial positions:",buddah_param

    write(stdout,2003)"Exponent",e_default
    if (j_for_carrying.or.burn_for_carrying)  write(stdout,2005) "Complex seed",julia_const
    if (.not.newt_for_carrying) write(stdout,2004) "Bail Out",bail_out
    write(stdout,2002) "Maximum Iterations",max_iter
    write(stdout,*)
    write(stdout,2006)"------------ Image Parameters ------------"
    write(stdout,*)	



    write(stdout,2002)"No. Grid points",N
    write(stdout,2003)"Lower X",lower_x
    write(stdout,2003)"Upper X",upper_x
    write(stdout,2003)"Lower Y",lower_y
    write(stdout,2003)"Upper Y",upper_y

    if (j_for_carrying.or.burn_for_carrying.or.do_mandelbrot)then

       write(stdout,*)
       write(stdout,2006)"------------ Colouring Scheme ------------"
       write(stdout,*)

       if (triangle)then
          write(stdout,2001) "Colouring Method","Triangle"
       elseif (ave_an)then 
          write(stdout,2001) "Colouring Method","Average Angle"
       else
          write(stdout,2001) "Colouring Method","Normal"
       end if
       if (nprocs.gt.1)then
          if (lookfor_PARALLEL .and. lookfor_data .and.b_for_carrying)then
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
    if(lookfor_warnings)then
       write(stdout,*) "** Warnings Present, Check param.mand"
       write(stdout,*)
    end if


    call trace_exit("FILE")
  end subroutine File


  subroutine READ_PARAMETERS()
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

    integer                      :: IOstatus=0,i,counter,j
    character(len=30)            :: chara,name,val,z_re_char,z_im_char,junk

    logical                      :: change_ux=.FALSE.
    logical                      :: change_lx=.FALSE.
    logical                      :: change_uy=.FALSE.
    logical                      :: change_ly=.FALSE.
    logical                      :: change_exp=.FALSE.
    logical                      :: change_iter=.FALSE.
    logical                      :: change_julia=.FALSE.

    call trace_entry("READ_PARAMETERS")

    inquire(file="param.mand", EXIST=file_exist)
    if (file_exist)then 
       open(unit=24,file="param.mand",status="OLD",access="stream",form="formatted")

       do while (IOstatus .eq. 0) 

          read(24,'(A)',IOSTAT=IOstatus)junk

          do j=1,len_trim(junk)

             if (junk(j:j).eq.':' .or. junk(j:j).eq."=")then
                name=junk(1:j-1)
                val=junk(j+1:len_trim(junk))

                exit
             elseif (j.eq.len_trim(junk))then
                !                print*, name, j, "no colon found"
                call Errors("User I/O Error")                
             endif
          enddo



          name=string_tolower(name)
          val=string_tolower(val)

          name=adjustl(name)
          val=adjustl(val)

          if (adjustl(name).eq."grid_size")then
             read(val,'(i6)')N
          elseif (name.eq."max_iter")then
             read(val,'(i12)')max_iter
             change_iter=.TRUE.
          elseif (name.eq."exponent")then
             call int_to_real(val)
             read(val,'(f5.2)')e_default
             change_exp=.true.
          elseif (name.eq."buddah_const".or.name.eq."Buddah_const")then
             read(val,'(i12)')buddah_param
          elseif (name.eq."julia_const")then
             read(val,*)julia_const
             change_julia=.TRUE.
          elseif (name.eq."x_low")then
             call int_to_real(val)
             read(val,'(f15.10)')lower_x
             change_lx=.true.
          elseif (name.eq."x_up")then
             call int_to_real(val)
             read(val,'(f15.10)')upper_x
             change_ux=.true.
          elseif (name.eq."y_low")then
             call int_to_real(val)
             read(val,'(f15.10)')lower_Y
             change_ly=.true.        
          elseif (name.eq."y_up")then
             call int_to_real(val)
             read(val,'(f15.10)')upper_Y
             change_uy=.true.       
          elseif (name.eq."debug")then
             if(val.eq."true")then
                debug=.true.
             elseif(val.eq."false")then
                debug=.false.
             end if
          elseif (name.eq."colouring_method")then
             if (val.eq."triangle")then 
                triangle=.true.
             elseif (val.eq."angle")then
                ave_an=.true.
             elseif(val.eq."normal")then 
                ave_an=.false.
             else
                call warning(name,val,lookfor_warnings)

             end if
          elseif (name.eq."bail_out")then
             do i=1,len_trim(val)
                if (val(1:1).eq."e")then 
                   read(val,'(E11.4)')bail_out
                   exit
                elseif(i.eq.len_trim(val))then
                   call int_to_real(val)
                   read(val,'(f15.10)')bail_out

                else
                   cycle
                end if
             end do

          elseif(name.eq."plot_parallel")then
             if (val.eq."true".or.val.eq."True")then
                lookfor_parallel=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_parallel=.FALSE.
             else
                call warning(name,val,lookfor_warnings)
             end if
          elseif(name.eq."continuation")then
             if (val.eq."true".or.val.eq."True")then
                continuation=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                continuation=.FALSE.
             else
                call warning(name,val,lookfor_warnings)
             end if
          elseif(name.eq."write_data")then
             if(val.eq."true".or.val.eq."True")then
                lookfor_data=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_data=.FALSE.
             else
                call warning(name,val,lookfor_warnings)
             end if
          elseif(name.eq."write_efficiency")then
             if(val.eq."true".or.val.eq."True")then
                lookfor_eff=.TRUE.
             elseif (val.eq."false".or.val.eq."False")then
                lookfor_eff=.FALSE.
             else
                call warning(name,val,lookfor_warnings)
             end if
          elseif(name.eq."calc_type".or.name.eq."Calc_type")then
             if(val.eq."buddahbrot".or.val.eq."Buddahbrot")then
                b_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."julia".or.val.eq."Julia")then
                j_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."newton".or.val.eq."Newton")then
                newt_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."burning_ship")then
                burn_for_carrying=.TRUE.
                do_mandelbrot=.false.
             elseif(val.eq."Mandelbrot".or.val.eq."mandelbrot")then
                b_for_carrying=.FALSE.
                j_for_carrying=.FALSE.
                newt_for_carrying=.FALSE.
                do_mandelbrot=.true.
             else
                call Warning(name,val,lookfor_warnings)
             end if
          elseif (name(1:1).eq."#".or.name(1:1).eq."!")then
             cycle

          else
             lookfor_warnings=.TRUE.
             write(*,*) "Warning: Unknown parameter- ", name

          end if
       end do
    end if



    !Set the Defaults

    if (j_for_carrying)then
       if (.not.change_lx)lower_x=-2.0
       if (.not.change_ux)upper_x=2.0
       if (.not.change_ly)lower_y=-2.0
       if (.not.change_uy)upper_y=2.0
    elseif (burn_for_carrying)then
       if (.not.change_lx)lower_x=-2.0
       if (.not.change_ux)upper_x=2.0
       if (.not.change_ly)lower_y=-2.
       if (.not.change_uy)upper_y=0.75
       if(.not.change_julia)julia_const=(0.,0.)
    elseif (b_for_carrying)then
       upper_x=0.75
       lower_X=-2.0
       lower_Y=-1.25
       upper_Y=1.25
       if (.not.change_iter)  max_iter=1000
    elseif (newt_for_carrying)then 
       if (.not.change_lx)lower_x=-2.0
       if (.not.change_ux)upper_x=2.0
       if (.not.change_ly)lower_y=-2.0
       if (.not.change_uy)upper_y=2.0
       if (.not.change_exp)e_default=3.0
    end if
    call trace_exit("READ_PARAMETERS")
  end subroutine READ_PARAMETERS






  subroutine warning(name,val,lookfor_warnings)
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
    call trace_entry("WARNING")
    lookfor_warnings=.TRUE.
    write(*,*)"Warning: Unknown argument '",trim(val),"' for parameter: ",trim(name)
49  format(1x,A,A,A,1x,A)
    write(*,*) "Reverting to default"
    call trace_exit("WARNING")
  end subroutine warning

  subroutine params()
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
    write(*,78) "Allowed",":","Mandelbrot, Julia, Buddahbrot, Newton, Burning_ship"
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

    write(*,73) adjustl("EXPONENT"),":","Specify exponent for calculation"
    write(*,78) "Allowed",":","any"
    write(*,76) "Default",":", e_default
    write(*,*)

    write(*,73) adjustl("BUDDAH_CONST"),":","Specify No. initial posotions in Buddahbrot calculation"
    write(*,78) "Allowed",":","any int > 0"
    write(*,75) "Default",":",buddah_param
    write(*,*)

    write(*,73) adjustl("JULIA_CONST"),":","Specify Julia set constant"
    write(*,78) "Allowed",":","any complex"
    write(*,74) "Default",":","(0.0,0.0)"
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
    write(*,78) "Allowed",":","NORMAL,TRIANGLE,ANGLE"
    write(*,74) "Default",":","NORMAL"


  end subroutine params


  function string_tolower( string ) result (new) 
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
    call trace_entry("STRING_TOLOWER")
    length = len(string) 
    new    = string 
    do i = 1,len(string) 
       k = iachar(string(i:i)) 
       if ( k >= iachar('A') .and. k <= iachar('Z') ) then 
          k = k + iachar('a') - iachar('A') 
          new(i:i) = achar(k) 
       endif
    enddo
    call trace_exit("STRING_TOLOWER")
  end function string_tolower


  subroutine print_dry(on_root)
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
    if (on_root)then
       write(stdout,*) "                        ***************************************"
       write(stdout,*) "                        *     Dryrun Complete: Finishing      *"
       write(stdout,*) "                        ***************************************"
    endif
    call COMMS_FINALISE()
    stop
  end subroutine print_dry



  subroutine print_help()
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
  end subroutine print_help

  subroutine errors(message)
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


    open(20,file="err.mand",status="unknown",access="append")
    write(20,*) "Error: ",trim(message)
    close(20)
    stop

  end subroutine errors


  subroutine int_to_real(int_real)
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
    call trace_entry("INT_TO_REAL")
    do j=0,len_trim(int_real)
       if (int_real(j:j).eq.".")decimal=.true.
    end do

    if (.not.decimal)int_real=trim(int_real)//"."
    decimal=.false.
    call trace_exit("INT_TO_REAL")
  end subroutine int_to_real
end module IO


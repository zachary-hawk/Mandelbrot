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
  implicit none
!!!!!! DEFINE THE DEFAULTS !!!!!!!!!
  integer        :: N=1000
  integer        :: Max_iter=50
  real           :: e_default=2.
  integer        :: budda_param=10000
  integer        :: stdout
  real(real32)   :: lower_X=-2.0
  real(real32)   :: upper_X=0.5
  real(real32)   :: lower_Y=-1.25
  real(real32)   :: upper_Y=1.25
  logical        :: lookfor_parallel=.FALSE.
  logical        :: lookfor_data=.TRUE.
  logical        :: lookfor_eff=.FALSE.
  logical        :: j_for_carrying=.FALSE.
  logical        :: burn_for_carrying=.FALSE.
  logical        :: b_for_carrying=.FALSE.
  logical        :: lookfor_cont=.FALSE.
  logical        :: lookfor_warnings=.FALSE.
  logical        :: newt_for_carrying=.FALSE.
  logical        :: continuation=.FALSE.
  logical        :: file_exist
  complex        :: julia_const=(0.285,0.01)
  character(81)  :: parser_version="Mandelbrot v.3.0, Z.Hawkhead" 
  character(100) :: info="Parallel code for calculating the Mandelbrot Set"
  character(100) :: DATE,TIME,compiler,arch_string,version,cpuinfo





contains

  subroutine header()
    implicit none
    integer::file
    integer :: maj_mpi,min_mpi,min_char
    character(len=max_version_length) :: mpi_c_version
    character(len=3) :: MPI_version_num

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

    write(file,*) "+==================================================================================+"
    write(file,*) "| MM    MM   AAA   NN   NN DDDDD   EEEEEEE LL      BBBBB   RRRRRR   OOOOO  TTTTTTT |"
    write(file,*) "| MMM  MMM  AAAAA  NNN  NN DD  DD  EE      LL      BB   B  RR   RR OO   OO   TTT   |"
    write(file,*) "| MM MM MM AA   AA NN N NN DD   DD EEEEE   LL      BBBBBB  RRRRRR  OO   OO   TTT   |"
    write(file,*) "| MM    MM AAAAAAA NN  NNN DD   DD EE      LL      BB   BB RR  RR  OO   OO   TTT   |"
    write(file,*) "| MM    MM AA   AA NN   NN DDDDDD  EEEEEEE LLLLLLL BBBBBB  RR   RR  OOOO0    TTT   |"
    write(file,*) "|                                                                                  |"
    write(file,*) "| ",parser_version,"|"
    write(file,*) "+==================================================================================+"
    write(file,*)
    write(file,*) "Compiled with ",compiler," ",Trim(version), " on ", __DATE__, " at ",__TIME__
    write(file,*) "Compiled for CPU: ",trim(cpuinfo)
    write(file,*) "Compiled for system: ",trim(arch_string)
    write(file,*)
    write(file,*) "Communications architechture: ",trim(comms_arch)
    if (comms_arch.eq."MPI")then
       write(file,*) "MPI Version: ",mpi_c_version(1:min_char+1)
    end if
    write(file,*)
  end subroutine header


  subroutine File(dryrun,nprocs,memory_buffer)
    character(len=3),dimension(12)  :: months
    integer                         :: d_t(8)    
    character*10                    :: b(3)
    logical,intent(in)              :: dryrun
    integer,intent(in)              :: nprocs
    real,intent(in)                 :: memory_buffer
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
2005 format(1x,A37,5x,':',F8.3,SP,F6.3,"i")

    if (j_for_carrying)then
       write(stdout,*)""
       write(stdout,2001) "Calculation Type","Julia"
    else if (b_for_carrying)then

       write(stdout,*)""
       write(stdout,2001) "Calculation Type","Buddahbrot"
    elseif (newt_for_carrying)then
       write(stdout,*)""
       write(stdout,2001) "Calculation Type","Newton"
    elseif (burn_for_carrying)then
       write(stdout,2001) "Calculation Type","Burning Ship"
    else
       write(stdout,*)
       write(stdout,2001) "Calculation Type","Mandelbrot"
    endif

    if (b_for_carrying) write(stdout,2002) "No. Initial positions:",budda_param



    write(stdout,2002) "No. Grid points",N
    write(stdout,2002) "No. Iterations",max_iter

    write(stdout,2003)"Exponent:",e_default
    if (j_for_carrying)  write(stdout,2005) "Value of c",julia_const

    write(stdout,2003)"Lower X",lower_x
    write(stdout,2003)"Upper X",upper_x
    write(stdout,2003)"Lower Y",lower_y
    write(stdout,2003)"Upper Y",upper_y

    if (nprocs.gt.1)then
       write(stdout,2001)"Parallelised","True"
       write(stdout,2002)"No. Processes", nprocs
       if (lookfor_PARALLEL .and. lookfor_data .and.b_for_carrying) then
          write(stdout,2001) "Colouring by Processor","True"
       else
          write(stdout,2001) "Colouring by Processor","False"
       end if
    else
       write(stdout,2001)"Parallelised","False"
    end if



    if (lookfor_eff)then
       write(stdout,2001) "Writing efficiency data","True"
    else
       write(stdout,2001) "Writing efficiency data","False"
    end if
    if (lookfor_data)then
       write(stdout,2001) "Writing data","True"
    else
       write(stdout,2001) "Writing data","False"
    end if
    write(stdout,*)
    write(stdout,*)"------------------------------------------------------------------------------------"
    write(stdout,*)
    write(stdout,2003) "Estimated memory usage (MB)",memory_buffer

    write(stdout,*)
    if(lookfor_warnings)then
       write(stdout,*) "** Warnings Present, Check param.mand"
       write(stdout,*)
    end if
    if (dryrun)then
       call print_dry()
    end if
    write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"
    write(stdout,*)"                           Starting Calculation                             <-- TIME"
    write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"


  end subroutine File










  subroutine READ_PARAMETERS()

    integer                      :: IOstatus=0,i,counter
    character(len=20)            :: chara,name,val,z_re_char,z_im_char
    character(len=1)             :: junk
    logical                      :: change_ux=.FALSE.
    logical                      :: change_lx=.FALSE.
    logical                      :: change_uy=.FALSE.
    logical                      :: change_ly=.FALSE.
    logical                      :: change_exp=.FALSE.
    logical                      :: change_iter=.FALSE.




    inquire(file="param.mand", EXIST=file_exist)
    if (file_exist)then 
       open(unit=24,file="param.mand",status="OLD",access="stream",form="formatted")

       do while (IOstatus .eq. 0) 

          read(24,*,IOSTAT=IOstatus)name,junk,val!chara

          name=string_tolower(name)
          val=string_tolower(val)
          if (adjustl(name).eq."grid_size")then
             read(val,'(i6)')N
          elseif (name.eq."max_iter")then
             read(val,'(i12)')max_iter
             change_iter=.TRUE.
          elseif (name.eq."exponent")then
             read(val,'(f5.2)')e_default
             change_exp=.true.
          elseif (name.eq."buddah_const".or.name.eq."Buddah_const")then
             read(val,'(i12)')budda_param
             b_for_carrying=.TRUE.
          elseif (name.eq."julia_const")then
             read(val,*)julia_const
          elseif (name.eq."x_low")then
             read(val,'(f15.10)')lower_x
             change_lx=.true.
          elseif (name.eq."x_up")then
             read(val,'(f15.10)')upper_x
             change_ux=.true.
          elseif (name.eq."y_low")then
             read(val,'(f15.10)')lower_Y
             change_ly=.true.        
          elseif (name.eq."y_up")then
             read(val,'(f15.10)')upper_Y
             change_uy=.true.         
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
             elseif(val.eq."julia".or.val.eq."Julia")then
                j_for_carrying=.TRUE.
             elseif(val.eq."newton".or.val.eq."Newton")then
                newt_for_carrying=.TRUE.
             elseif(val.eq."burning_ship")then
                burn_for_carrying=.TRUE.


             elseif(val.eq."Mandelbrot".or.val.eq."mandelbrot")then
                b_for_carrying=.FALSE.
                j_for_carrying=.FALSE.
                newt_for_carrying=.FALSE.

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
  end subroutine READ_PARAMETERS






  subroutine warning(name,val,lookfor_warnings)

    character(*)::name,val
    logical,intent(inout) :: lookfor_warnings
    lookfor_warnings=.TRUE.
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
    write(*,75) "Default",":",budda_param
    write(*,*)

    write(*,73) adjustl("JULIA_CONST"),":","Specify Julia set constant, quotes required"
    write(*,78) "Allowed",":","any complex"
    write(*,74) "Default",":","'(0.0,0.0)'"
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


  subroutine print_dry()

    write(stdout,*) "                      ***************************************"
    write(stdout,*) "                      *     Dryrun Complete: Finishing      *"
    write(stdout,*) "                      ***************************************"

    stop
  end subroutine print_dry



  subroutine print_help()
    write(*,*) trim(parser_version)
    write(*,*) trim(info)
    write(*,*) '   -v, --version     Print version information and exit'
    write(*,*) '   -h, --help        Print usage information and exit'
    write(*,*) '   -l, --list        Print list of allowed parameters'
    write(*,*) '   -c, --clear       Remove previous output files'
  end subroutine print_help

  subroutine errors(message)
    implicit none
    character(*)::message

       open(20,file="err.mand",status="unknown",access="append")
       write(*,*) "Error"
       write(20,*) "Error: ",trim(message)
       close(20)
       stop

  end subroutine errors


end module IO

!------------------------------!
! Writing Header for Mandelbrot!
! Author: Z.Hawkhead           !
!------------------------------!
module files
use comms
implicit none


contains
  subroutine header(file,parser_version,arch_string)
    use ISO_FORTRAN_ENV
    implicit none
    integer::file
    character(81)::parser_version
    character(100)::DATE,TIME,compiler,arch_string,version,comms,cpuinfo
    integer :: maj_mpi,min_mpi
    character(len=max_version_length) :: mpi_c_version


    if (comms_arch.eq."MPI")then
       call COMMS_LIBRARY_VERSION(mpi_c_version)
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



    if (len(arch).eq.0)then 
       print*,"Could not determine system"
    end if


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
       write(file,*) "MPI Version: ",mpi_c_version(1:15)
    end if
    write(file,*)
  end subroutine header

end module files

!--------------------------------!
! Author Z.Hawkhead              !
!--------------------------------!
program Mandelbrot
  !use MPI 
  use files
  use fractal
  use ISO_FORTRAN_ENV
  implicit none
  include 'mpif.h'
  integer :: N=1000,Max_iter=50 ,i,j,k,l=0,m, colour_ref,colour_ref_buff,i_buff,j_buff, rank_buff,comp_count,z_counter=21
  real,allocatable,dimension(:) :: Buffer_mpi,rank_0_buff
  real,allocatable,dimension(:,:):: set
  complex :: c,z,c_buff=(0,0) ! the complex number to be used
  double precision :: lower_X=-2.0,upper_X=0.5,lower_Y=-1.25,upper_Y=1.25,dx,dy
  double precision ::start, finish ,inp_st, inp_fn,int_time,par_start,par_end,par_time=0.0,tot_par_time,total_proc_times
  integer:: ierr, nprocs, rank, rem 
  integer, dimension(MPI_STATUS_SIZE):: status1
  integer::narg,cptArg !#of arg & counter of arg
  character(len=20)::name !Arg name
  logical::lookfor_N=.FALSE.,lookfor_MAX_ITER=.FALSE.,lookfor_PARALLEL=.FALSE.,lookfor_lx=.FALSE.,lookfor_data=.TRUE.
  logical::lookfor_ux=.FALSE.,lookfor_ly=.FALSE.,lookfor_uy=.FALSE.,lookfor_eff=.FALSE.,lookfor_c=.FALSE.,lookfor_j=.FALSE.
  !logical::lookfor_imz=.FALSE.,lookfor_rez=.FALSE.
  character::clear_check
  integer :: d_t(8)
  character*10 :: b(3)
  character(len=100)::version, compiler,arch_string, DATE,TIME
  character(len=81)::parser_version="Mandelbrot v.2.0, Z.Hawkhead"
  character(len=100)::info="Parallel code for calculating the Mandelbrot Set"
  character(len=15)::N_character,max_char
  real::eff,z_im=0.,z_re=0.
  character(20)::z_char="[0.0:0.0]",z_re_char,z_im_char
  character(len=3),dimension(12)::months


  months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']


  ! PreProcessor set up
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



  call date_and_time(b(1), b(2), b(3), d_t)


  !SET UP MPI ENVIRONMENT
  CALL MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)             
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)   



  ! Set up Commandline Parser

  !Check for arguments 
  narg=command_argument_count()

  !Loop over the arguments
  if(narg>0)then
     !loop across options
     do cptArg=1,narg
        call get_command_argument(cptArg,name)

        select case(adjustl(name))
        case("--help","-h")
           call print_help()
           stop
        case("-v","--version")
           write(*,*) trim(Parser_version)
           write(*,*) trim(info)
           stop
        case("-e","--efficency")
           lookfor_eff=.TRUE.  
        case("-n","-N")
           lookfor_N=.TRUE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           !           lookfor_j=.FALSE.
        case("-i")
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.TRUE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           !          lookfor_j=.FALSE.
        case("-p","--parallel")
           lookfor_PARALLEL=.TRUE.
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           !         lookfor_j=.FALSE.
        case("-lx")
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.TRUE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           !        lookfor_j=.FALSE.
        case("-ux")
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.TRUE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           !       lookfor_j=.FALSE.
        case("-uy")
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.TRUE.
           lookfor_j=.FALSE.
        case("-ly")
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.TRUE.
           lookfor_uy=.FALSE.
           !      lookfor_j=.FALSE.
        case("-d","--data") !Added in v1.0.1 
           lookfor_data=.FALSE.
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           lookfor_PARALLEL=.FALSE.
           !     lookfor_j=.FALSE.
        case("-c","--clear") !Added in v1.0.1
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           lookfor_c=.TRUE.
           lookfor_j=.FALSE.
        case("-j","--julia") !Added in v1.0.1                                                                      
           lookfor_N=.FALSE.
           lookfor_MAX_ITER=.FALSE.
           lookfor_lx=.FALSE.
           lookfor_ux=.FALSE.
           lookfor_ly=.FALSE.
           lookfor_uy=.FALSE.
           lookfor_c=.FALSE.
           lookfor_j=.TRUE.
           !Default case, handels inputs
           lower_x=-2.
           lower_y=-2.
           upper_x=2.
           upper_y=2.
        case default
           if (lookfor_N) then
              name=adjustl(name)
              read(name,'(i5)') N
           else if (lookfor_MAX_ITER) then
              name=adjustl(name)
              read(name,'(i5)') Max_iter
           else if (lookfor_ux)then
              name=adjustl(name)
              read(name,'(f15.10)') upper_x
           else if (lookfor_ly)then
              name=adjustl(name)
              read(name,'(f15.10)') lower_Y
           else if (lookfor_uy)then
              name=adjustl(name)
              read(name,'(f15.10)') upper_Y           
           else if (lookfor_lx)then
              name=adjustl(name)
              read(name,'(f15.10)') lower_X
           else if (lookfor_j)then
              name=adjustl(name)
              read(name,*)z_char


           else
              write(*,34)"Undefined Flag:", name
34            format(1x,A,1x,A)
              call print_help()
              stop
           end if

        end select
     end do
  end if



  !  if (rank.eq.0)


  ! IDENTIFYING Z

  do i=1,len(z_char)
     if (z_char(i:i).eq.":")then
        z_counter=i
        exit
     end if
     if (i.eq.len(z_char))then
           call errors(rank,"Julia constant improperly formatted")
     end if
  end do

  z_re_char=z_char(2:z_counter-1)
  z_im_char=z_char(i+1:len(trim(z_char))-1)



  read(z_re_char,*)z_re
  read(z_im_char,*)z_im

  !  if (lookfor_j.eqv..TRUE. .and. lookfor_lx.eqv..FALSE.)then 
  !     lower_x=-2.
  !  end if
  !  if (lookfor_j.eqv..TRUE..and. lookfor_ux.eqv..FALSE.)then
  !     upper_x=2.
  !  end if
  !  if (lookfor_j.eqv..TRUE..and. lookfor_uy.eqv..FALSE.)then
  !     upper_y=2.
  !  end if
  !  if (lookfor_j.eqv..TRUE. .and. lookfor_ly.eqv..FALSE.)then
  !     lower_y=-2.
  !  end if

  ! Check for clear command
  if(lookfor_c) then
     write(*,*) "Previous results may be deleted. Proceed anyway? y/n"
     read(*,*) clear_check
     if (clear_check.eq."y" .or. clear_check.eq."Y")then
        call system("rm out.mand data.mand efficiency.mand err.mand &>/dev/null")
        stop
     else if (clear_check.eq."n" .or. clear_check.eq."N") then
        stop
     end if
  end if


  !ERROR HANDLING



  if (upper_X-lower_X .lt. 0 .or.upper_Y-lower_Y .lt. 0) then 
     call errors(rank,"Extent definition ambiguous"  )
  else if (N.lt.0)then

     call errors(rank,"Grid size must be positve integer")
  else if (Max_iter.lt.0)then

     call errors(rank," Max No. iterations must be positive integer")

  else if (N.lt.nprocs**2)then
     call errors(rank,"Too few grid points for parallism ")
  end if

  !Ensure commensurate with cores 
  do while (mod(N,nprocs).gt.0)
     N=N+1
  end do

  ! Allocating arrays
  if (rank.eq.0) then
     allocate(set(1:N,1:N))
     allocate(rank_0_buff(1:N))
  end if

  allocate(Buffer_mpi(1:N))

  ! BROADCAST INPUT TO SLAVES
  call MPI_BCAST(N,1,MPI_INT,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(dx,1,MPI_FLOAT,0,MPI_COMM_WORLD,ierr) 
  call MPI_BCAST(dy,1,MPI_FLOAT,0,MPI_COMM_WORLD,ierr) 
  call MPI_BCAST(Max_iter,1,MPI_INT,0,MPI_COMM_WORLD,ierr)





  if (rank.eq.0)then

     !write(*,*) compiler_version()

     !Initialise Files
     if(lookfor_data)then     
        open(unit=2,file="data.mand",form="UNFORMATTED")
     end if
     open(unit=1,file="out.mand",RECL=8192)
     call header(1,parser_version,arch_string)

     write(1,1000) months(d_t(2)),d_t(3),d_t(1),d_t(5),d_t(6),d_t(7),d_t(8)
1000 format (' Calculation started:  ', A, 1x, i2.2, 1x, i4.4, ' at ',i2.2, ':', i2.2, ':', i2.2 ,".",i3.3)
     write(1,*)
     if (lookfor_j)then
        write(1,2001) "Calculation Type:","Julia"
     else
        write(1,2002) "Calculation Type:","Mandelbrot"
     endif
2001 format(1x,A,35x,A)
2002 format(1x,A,30x,A) 
     write(N_character,'(i6)')N    
     write(1,1) "No. Grid points:",trim(N_character)
1    format(1x,A,35x,A,6x) !             ",i6," Grid points")

     write(max_char,'(i6)')Max_iter
     write(1,2) "No. Iteration:",trim(max_char)
2    format(1x,A,37x,A)
     if (lookfor_j)then
        if (z_im.gt.0)then
           write(1,77)"Value of c:",z_re,"+",z_im,"i"
        else
           write(1,78)"Value of c:",z_re,z_im,"i"
        end if
77      format(1x,A,34x,f5.3,A,f5.3,A)
78      format(1x,A,34x,f5.3,f5.3,A)

     end if
     write(1,*)    
     write(1,75) lower_X,upper_X
     write(1,76) lower_Y,upper_Y
75   format(" Extent used: X:                                ",f10.2,f10.2)
76   format("              Y:                                ",f10.2,f10.2)
     write(1,*)
     if (nprocs.gt.1)then

        write(1,*) "Calculated in Parallel"
        write(1,400) nprocs
400     format(" No. Processes:                                         ",i2)
        write(1,*)


        if (lookfor_PARALLEL .and. lookfor_data) then 
           write(1,*) "Colouring by processor:                                On"
        else 
           write(1,*) "Colouring by Processor:                               Off"
        end if
     else
        write(1,*) "Calculated in Serial"
     end if

     if (lookfor_eff)then
        write(1,*) "Writing efficiency data:                               On"
     else 
        write(1,*) "Writing efficiency data:                              Off"
     end if
     if (lookfor_data)then
        write(1,*) "Writing data:                                          On"
     else 
        write(1,*) "Writing data:                                         Off"
     end if
     write(1,*)
     write(1,*) "Starting Calculation:"
     write(1,*)"************************************************************************************"

  end if





  !Do the Calculations


  start =MPI_WTIME()

  dx = (upper_X-lower_X)/N
  dy = (upper_Y-lower_Y)/N


  do i=1+rank*N/nprocs,(rank+1)*N/nprocs

     do j=1,N

        c = cmplx(lower_X+i*dx,lower_Y+j*dy)
        z=cmplx(z_re,z_im)
        !do k=0,Max_iter
        ! c = z+c**2
        !if (abs(c).gt.2) then
        ! exit         
        !end if
        !end do


        if (lookfor_j)then
           k=julia(Max_iter,z,c)
        else
           k=mand(Max_iter,z,c)
        end if


        if (lookfor_PARALLEL) then
           colour_ref =k -Max_iter-1+k + Max_iter*rank
           Buffer_mpi(j)=colour_ref
        else 
           colour_ref =k -Max_iter-1+k
           Buffer_mpi(j)=colour_ref
        end if

     end do

     if (rank.eq.0) then 
        set(i,1:N)=Buffer_mpi
        do m=1,nprocs-1
           l=l+1

           par_start=MPI_WTIME() ! Time the parallel stuff
           CALL MPI_RECV(i_buff,1,MPI_INT,m,1,MPI_COMM_WORLD,status1,ierr)
           CALL MPI_RECV(rank_0_buff,N+1,MPI_INT,m,1,MPI_COMM_WORLD,status1,ierr)
           par_end=MPI_WTIME()
           par_time=par_time+par_end-par_start


           set(i_buff,1:N)=rank_0_buff
           ! Timing the steps           
           do comp_count=10,100,10
              if (real(l)/real(N).gt.real(comp_count)/100-1./(real(N)*nprocs) .and. &
                   real(l)/real(N).lt.real(comp_count)/100+1./(real(N)*nprocs))then 
                 int_time=MPI_WTIME()              
                 write(1,200) comp_count,int_time-start
200              format(" Calculation ",i2,"% complete:                      ",F10.5," s")

              end if
           end do
        end do
     else
        par_start=MPI_WTIME()
        CALL MPI_SEND(i,1,MPI_INT,0,1,MPI_COMM_WORLD,status1,ierr)
        CALL MPI_SEND(Buffer_mpi,N+1,MPI_INT,0,1,MPI_COMM_WORLD,status1,ierr)
        par_end=MPI_WTIME()
        par_time=par_time+par_end-par_start
     end if
  end do

  finish = MPI_WTIME()
  !par_start=MPI_WTIME()
  call MPI_REDUCE(start,inp_st,1,MPI_DOUBLE,MPI_MIN,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(finish,inp_fn,1,MPI_DOUBLE,MPI_MAX,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(finish-start,total_proc_times,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  !par_end=MPI_WTIME()
  !par_time=par_time+par_end-par_start

  !  print*,total_proc_times

  CALL MPI_REDUCE(par_time,tot_par_time,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)


  if (rank.eq.0)then
     !Finish Files and close
     write(1,*)"************************************************************************************"
     Write(1,1001) inp_fn-inp_st
1001 format(" Total Calculation Time:                       ",F11.5," s")   
     write(1,*)"************************************************************************************"  
     write(1,*)
     eff=100.-par_time*100./total_proc_times
     eff=100.-eff/95.
     if (nprocs.gt.1)then
        write(1,1005) "Parallel Effciency:",eff,"%"
1005    format(1x,A,32x,f6.2,1x,A)
     end if

     if (lookfor_data)then
        write(2)set
        close(2)
     end if


     !**********FOLLOWING LINES FOR PARALLEL EFFICIENCY TESTING*************
     if (lookfor_eff)then
        open(unit=3,file="efficiency.mand",form="formatted",status="unknown",access="append")
        write(3,*)nprocs,inp_fn-inp_st,"#N=",N
        close(3)
        write(1,*) "Efficiency data writen to file 'efficiency.mand'"
     end if
     if (lookfor_data)then
        write(1,*)'Calculation written to file "data.mand"'
     end if
  end if
  close(1)
  call MPI_FINALIZE(ierr)

contains 
  subroutine print_help()
    write(*,*) trim(parser_version)
    write(*,*) trim(info)
    write(*,*) '-v, --version     print version information and exit'
    write(*,*) '-h, --help        print usage information and exit'
    write(*,*) '-j, --julia       Calculate Julia set with variable c: Format [re:im]'
    write(*,*) '-n, -N            Set size of grid'
    write(*,*) '-i                Set maximum iteration'
    write(*,*) '-p                Toggle parallel image generation'
    write(*,*) '-e, --efficiency  Write data to file "Mandelbrot.dat" for efficiceny testing'
    write(*,*) '-d, --data        Surpress data output'
    write(*,*) '-c, --clear       Remove previous output files'
    write(*,*) '-lx               Set lower limit in x direction'
    write(*,*) '-ux               Set upper limit in x direction'
    write(*,*) '-ly               Set lower limit in y direction'
    write(*,*) '-uy               Set upper limit in y direction'
  end subroutine print_help

  subroutine errors(rank,message)
    implicit none
    integer:: rank
    character(*)::message

    if (rank.eq.0)then
       open(20,file="err.mand",status="unknown",access="append")
       write(*,*) "Error"
       write(20,*) "Error: ",trim(message)
       close(20)
       stop
    end if
  end subroutine errors
end program

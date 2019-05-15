!--------------------------------!
! Author Z.Hawkhead              !
! Master for MPI                 !
!--------------------------------!
program Mandelbrot
  !use MPI 
  use files
  use fractal
  use ISO_FORTRAN_ENV
  implicit none
  include 'mpif.h'
  integer :: N=1000,Max_iter=50 ,i,j,k,l=0,m, colour_ref,colour_ref_buff,i_buff,j_buff, rank_buff,comp_count,z_counter=21
  integer :: e_default=2,k_buff,m_buff
  real,allocatable,dimension(:) :: Buffer_mpi,rank_0_buff
  real,allocatable,dimension(:,:):: set,buddah_set,buddah_buff
  complex :: c,z,c_buff=(0,0) ! the complex number to be used
  double precision :: lower_X=-2.0,upper_X=0.5,lower_Y=-1.25,upper_Y=1.25,dx,dy
  double precision ::start, finish ,inp_st, inp_fn,int_time,par_start,par_end,par_time=0.0,tot_par_time,total_proc_times
  integer:: ierr, nprocs, rank, rem 
  integer, dimension(MPI_STATUS_SIZE):: status1
  integer::narg,cptArg,buddah_counter=0,buddah_buff_counter !#of arg & counter of arg
  character(len=20)::name !Arg name
  logical::lookfor_N=.FALSE.,lookfor_MAX_ITER=.FALSE.,lookfor_PARALLEL=.FALSE.,lookfor_lx=.FALSE.,lookfor_data=.TRUE.
  logical::lookfor_ux=.FALSE.,lookfor_ly=.FALSE.,lookfor_uy=.FALSE.,lookfor_eff=.FALSE.,lookfor_c=.FALSE.,lookfor_j=.FALSE.
  logical::lookfor_m=.FALSE.,J_FOR_CARRYING=.FALSE.,lookfor_buddah=.FALSE.,B_FOR_CARRYING=.FALSE.
  integer:: d_t(8),budda_param
  character*10 :: b(3),clear_check
  character(len=100)::version, compiler,arch_string,path_string, DATE,TIME,comms="MPI"
  character(len=81)::parser_version="Mandelbrot v.2.1, Z.Hawkhead"
  character(len=100)::info="Parallel code for calculating the Mandelbrot Set"
  character(len=15)::N_character,max_char,buddah_char
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
        case("-i")
           lookfor_MAX_ITER=.TRUE.
        case("-p","--parallel")
           lookfor_PARALLEL=.TRUE.
        case("-lx")
           lookfor_lx=.TRUE.
        case("-ux")
           lookfor_ux=.TRUE.
        case("-uy")
           lookfor_uy=.TRUE.
        case("-ly")
           lookfor_ly=.TRUE.
        case("-d","--data") !Added in v1.0.1 
           lookfor_data=.FALSE.
        case("-c","--clear") !Added in v1.0.1
           lookfor_c=.TRUE.
        case("-m") !Added in v1.0.1
           lookfor_m=.TRUE.
           lower_x=-1.5
           lower_y=-1.5
           upper_x=1.5
           upper_y=1.5
        case("-j","--julia") !Added in v1.0.1                                                                      
           lookfor_j=.TRUE.
           J_FOR_CARRYING=.TRUE.
           lower_x=-2.
           lower_y=-2.
           upper_x=2.
           upper_y=2.
        case("-b","--buddah")
           lookfor_buddah=.TRUE.
           B_FOR_CARRYING=.TRUE.
           !Default case, handel inputs
        case default
           if (lookfor_N) then
              name=adjustl(name)
              read(name,'(i5)') N
              lookfor_N=.FALSE.
           else if (lookfor_MAX_ITER) then
              name=adjustl(name)
              read(name,'(i12)') Max_iter
              lookfor_MAX_ITER=.FALSE.
           else if (lookfor_ux)then
              name=adjustl(name)
              read(name,'(f15.10)') upper_x
              lookfor_ux=.FALSE.
           else if (lookfor_ly)then
              name=adjustl(name)
              read(name,'(f15.10)') lower_Y
              lookfor_ly=.FALSE.
           else if (lookfor_uy)then
              name=adjustl(name)
              read(name,'(f15.10)') upper_Y
              lookfor_uy=.FALSE.
           else if (lookfor_lx)then
              name=adjustl(name)
              read(name,'(f15.10)') lower_X
              lookfor_lx=.FALSE.
           else if (lookfor_j)then
              name=adjustl(name)
              read(name,*)z_char
              lookfor_j=.FALSE.
           else if (lookfor_m)then
              name=adjustl(name)
              read(name,'(i6)')e_default
              lookfor_m=.FALSE.
           else if (lookfor_buddah)then
              name=adjustl(name)
              read(name,'(i12)') budda_param
              lookfor_buddah=.FALSE.
           else
              write(*,33)"Undefined Flag:", name
33            format(1x,A,1x,A)
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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FILE WRITTING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (rank.eq.0)then

     !write(*,*) compiler_version()

     !Initialise Files
     if(lookfor_data)then     
        open(unit=2,file="data.mand",form="UNFORMATTED")
     end if
     open(unit=1,file="out.mand",RECL=8192)
     call header(1,parser_version,arch_string,comms)

     write(1,1000) months(d_t(2)),d_t(3),d_t(1),d_t(5),d_t(6),d_t(7),d_t(8)
1000 format (' Calculation started:  ', A, 1x, i2.2, 1x, i4.4, ' at ',i2.2, ':', i2.2, ':', i2.2 ,".",i3.3)
     write(1,*)
     if (j_for_carrying)then
        write(1,2001) "Calculation Type:","Julia"
     else if (b_for_carrying)then
        write(1,2002) "Calculation Type:","Buddahbrot"
     else
        write(1,2002) "Calculation Type:","Mandelbrot"
     endif
     write(1,*)
2001 format(1x,A,35x,A)
2002 format(1x,A,30x,A)
     write(1,91)"Exponent:",e_default
91   format(1x,A,42x,i6)
     if (b_for_carrying)then
        write(buddah_char,'(i12)')budda_param
        write(1,3) "No. Initial positions:",trim(buddah_char)
3       format(1x,A,23x,A,6x)
     end if
     write(N_character,'(i6)')N    
     write(1,1) "No. Grid points:",trim(N_character)
1    format(1x,A,35x,A,6x) !             ",i6," Grid points")x

     write(max_char,'(i12)')Max_iter
     write(1,2) "No. Iteration:",trim(max_char)
2    format(1x,A,31x,A)
     if (j_for_carrying)then
        if (z_im.gt.0)then
           write(1,77)"Value of c:",z_re,"+",z_im,"i"
        else
           write(1,78)"Value of c:",z_re,z_im,"i"
        end if
77      format(1x,A,34x,f5.3,A,f5.3,A)
78      format(1x,A,34x,f5.3,f6.3,A)

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
     if (nprocs.gt.1 .and. b_for_carrying.eq..false.)then
        write(1,*)"************************************************************************************"
     end if
  end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !Do the Calculations


  start =MPI_WTIME()

  dx = (upper_X-lower_X)/N
  dy = (upper_Y-lower_Y)/N

  !This bit for the Buddahbrot

  if (b_for_carrying)then

     !if (rank.eq.0) then
     allocate(buddah_set(1:N,1:N))
     buddah_set=0
     !end if
     !call MPI_BCAST(set,N**2,MPI_INT,0,MPI_COMM_WORLD,ierr)

     do i=1+rank*budda_param/nprocs,(rank+1)*budda_param/nprocs

        z=cmplx(0,0)

        !print*,"first z",z
        !call init_random_seed()
        !call random_number(z_re)
        !call random_number(z_im)
        !z_re=z_re*(upper_x-lower_x)+lower_x
        !z_im=z_im*(upper_y-lower_y)+lower_y


        !c=cmplx(z_re,z_im)
        c=random_pos()

        if (mand(Max_iter,z,c,e_default).gt.Max_iter)then
           cycle

        end if
        buddah_counter=buddah_counter+1
        z=cmplx(0,0)
        do j=1,Max_iter
           z=z**e_default+c
           if (abs(z).gt.4)then
              exit
           end if


           k=int(N*(real(z)-lower_x)/(upper_x-lower_x))
           m=int(N*(aimag(z)-lower_y)/(upper_y-lower_y))

           if (m.lt.N .and. m.gt.0)then
              if (k.lt.N.and.k.gt.0)then
                 !set(k,m)=set(k,m)+10
                 buddah_set(m,k)=buddah_set(m,k)+N
              end if
           end if

        end do
     end do
     !     print*, buddah_set,rank


     if (rank.gt.0)then



        CALL MPI_SEND(buddah_set,N**2,MPI_FLOAT,0,rank,MPI_COMM_WORLD,status1,ierr)


     end if

     if (rank.eq.0)then

        allocate(buddah_buff(1:N,1:N))

        set=set+buddah_set

        do j=1,nprocs-1


           CALL MPI_RECV(buddah_buff,N**2,MPI_FLOAT,j,j,MPI_COMM_WORLD,status1,ierr)
           !        print*, "rec"
           set=set+buddah_buff
        end do
     end if
     !     call MPI_REDUCE(buddah_set,set,N**2,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD)
     ! print*,"after"


     !    if (rank.eq.0)then
     !      set=buddah_set
     !  end if



     !and the Mandelbrot and Julia 

  else

     do i=1+rank*N/nprocs,(rank+1)*N/nprocs

        do j=1,N

           c = cmplx(lower_X+i*dx,lower_Y+j*dy)
           z=cmplx(z_re,z_im)

           if (J_FOR_CARRYING)then
              k=julia(Max_iter,z,c,e_default)
           else
              k=mand(Max_iter,z,c,e_default)
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
200                 format(" Calculation ",i2,"% complete:                      ",F10.5," s")

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
  end if
  finish = MPI_WTIME()
  !par_start=MPI_WTIME()
  call MPI_REDUCE(start,inp_st,1,MPI_DOUBLE,MPI_MIN,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(finish,inp_fn,1,MPI_DOUBLE,MPI_MAX,0,MPI_COMM_WORLD,ierr)
  CALL MPI_REDUCE(finish-start,total_proc_times,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  if (b_for_carrying)then
     CALL MPI_REDUCE(buddah_counter,buddah_buff_counter,1,MPI_INT,MPI_SUM,MPI_COMM_WORLD,ierr)

  end if

  !par_end=MPI_WTIME()
  !par_time=par_time+par_end-par_start

  !  print*,total_proc_times

  CALL MPI_REDUCE(par_time,tot_par_time,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)




  !plotting perimeter points

  !  open(unit=12,file="perim.mand")
  ! do i=2,N-1
  !   do j=2,N-1
  !
  !       if (set(i,j).gt.max_iter)then
  !
  !          if (set(i,j-1).lt.max_iter .or. set(i,j+1).lt.max_iter)then
  !
  !             write(12,*) cmplx(lower_X+i*dx,lower_Y+j*dy)
  !
  !          end if
  !      end if
  !    end do
  ! end do
  !close(12)


  if (rank.eq.0)then
     !Finish Files and close
     write(1,*)"************************************************************************************"
     Write(1,1001) inp_fn-inp_st
1001 format(" Total Calculation Time:                       ",F11.5," s")   
     write(1,*)"************************************************************************************"  
     write(1,*)
     eff=100.-par_time*100./total_proc_times
     eff=100.-eff/95.
     if (nprocs.gt.1 .and. b_for_carrying.eq..false.)then
34      write(1,1005) "Parallel Effciency:",eff,"%"
1005    format(1x,A,32x,f6.2,1x,A)
     end if
     if (b_for_carrying)then 
        write(1,304) "Buddahbrot Efficiency:",100.*real(buddah_buff_counter)/real(budda_param),"%"
304     format(1x,A,29x,f6.2,1x,A)
     end if
     if (lookfor_data)then
        write(2)set
        close(2)
     end if


     !**********FOLLOWING LINES FOR PARALLEL EFFICIENCY TESTING*************
     if (lookfor_eff)then
        open(unit=3,file="efficiency.mand",form="formatted",status="unknown",access="append")
        write(3,*)nprocs,inp_fn-inp_st,"#N=",N," MPI"
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
    write(*,*) '   -v, --version     print version information and exit'
    write(*,*) '   -h, --help        print usage information and exit'
    write(*,*) '   -j, --julia       Calculate Julia set with variable c: Format [re:im]'
    write(*,*) '   -b                Calculate Buddahbrot set, No. initial positions required'
    write(*,*) '   -n, -N            Set size of grid'
    write(*,*) '   -i                Set maximum iteration'
    write(*,*) '   -m                Exponent for generalised calculations'
    write(*,*) '   -p                Toggle parallel image generation'
    write(*,*) '   -e, --efficiency  Write data to file "Mandelbrot.dat" for efficiceny testing'
    write(*,*) '   -d, --data        Surpress data output'
    write(*,*) '   -c, --clear       Remove previous output files'
    write(*,*) '   -lx               Set lower limit in x direction'
    write(*,*) '   -ux               Set upper limit in x direction'
    write(*,*) '   -ly               Set lower limit in y direction'
    write(*,*) '   -uy               Set upper limit in y direction'
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

end program Mandelbrot

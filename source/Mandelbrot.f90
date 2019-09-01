!=============================================================================!
!                           M A N D E L B R O T                               !
!=============================================================================!
!          The main code for Mandelbrot and fractal generation                !
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================!
program Mandelbrot
  use fractal
  use io
  use comms
  use trace
  use ISO_FORTRAN_ENV
  implicit none

  integer                         :: i,j,l=0,m,i_buff,j_buff, rank_buff,comp_count,z_counter=21
  integer                         :: buddah_iter
  real(real32),allocatable        :: Buffer_mpi(:),rank_0_buff(:)
  real(real32),allocatable        :: set(:,:),buddah_set(:,:),buddah_buff(:,:)
  complex(complex_kind)           :: c,z,c_buff=(0,0) ! the complex number to be used
  double precision                :: dx,dy, init_time,init_buff,fini_time,fini_buff
  double precision                :: start, finish ,inp_st, inp_fn,int_time,par_start,par_end
  double precision                :: par_time=0.0,tot_par_time,total_proc_times
  double precision                :: after_calc,after_calc_buff
  integer                         :: nprocs, rank, rem 
  integer                         :: narg,cptArg,buddah_counter=0,buddah_buff_counter !#of arg & counter of arg
  character(len=20)               :: name !Arg name
  logical                         :: args_bool=.FALSE.,dryrun=.false.,lookfor_c=.false.,on_root
  character*10                    :: clear_check
  real                            :: eff,z_im=0.,z_re=0.
  integer                         :: data_size
  real                            :: tolerance,frac,memory_size=0,memory_buffer=0,theta
  real                            :: disk_stor=0,k,colour_ref,colour_ref_buff,comms_time_buff

  call trace_init()



  !SET UP MPI ENVIRONMENT
  CALL COMMS_INIT()

  call COMMS_RANK(rank)             
  call COMMS_SIZE(nprocs)   
  start=COMMS_WTIME()
  if(rank.eq.0)then
     on_root=.true.
  else
     on_root=.false.
  end if




!!! Define the parameters from the param.mand

  call READ_PARAMETERS()
  !Ensure commensurate with cores
  if (.not.b_for_carrying)then 
     do while (mod(N,nprocs).gt.0)
        N=N+1
     end do
  end if

  ! Set up Commandline Parser

  !Check for arguments 

  narg=command_argument_count()

  !Loop over the arguments
  if(narg>0)then
     !loop across options
     args_bool=.TRUE.
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
        case("-c","--clear") !Added in v1.0.1
           lookfor_c=.TRUE.
        case("-l","--list")
           write(*,*) trim(Parser_version)
           write(*,*) trim(info)
           call params()
           stop
        case("-d","--dryrun")
           dryrun=.TRUE.

        case default
           write(*,33)"Undefined Flag:", name
33         format(1x,A,1x,A)
           call print_help()
           stop
        end select
     end do
  end if



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
     if (on_root) call errors("Extent definition ambiguous"  )
  else if (N.lt.0)then
     if (on_root) call errors("Grid size must be positve integer")
  else if (Max_iter.lt.0)then
     if (on_root) call errors(" Max No. iterations must be positive integer")
  end if


  ! Allocating arrays
  if (rank.eq.0) then
     allocate(set(1:N,1:N))
     if (lookfor_data) disk_stor=sizeof(set)*1e-6
     allocate(rank_0_buff(1:N))
     if (b_for_carrying) then
        allocate(buddah_buff(1:N,1:N))
     end if
     memory_size=memory_size+sizeof(set)*1e-6
     memory_size=memory_size+sizeof(rank_0_buff)*1e-6
  end if
  if(b_for_carrying)then
     allocate(buddah_set(1:N,1:N))
     memory_size=memory_size+sizeof(buddah_set)*1e-6
  end if
  allocate(Buffer_mpi(1:N))
  memory_size=memory_size+sizeof(buffer_mpi)*1e-6



  !Check the memory size
  call COMMS_REDUCE_REAL(memory_size,memory_buffer,1,"MPI_SUM")


  ! BROADCAST INPUT TO SLAVES
  call COMMS_BCAST_INT(N,1)
  call COMMS_BCAST_DOUBLE(dx,1)
  call COMMS_BCAST_DOUBLE(dy,1)
  call COMMS_BCAST_INT(Max_iter,1)

  ! Check for Buddahbrot continuation
  if (rank.eq.0)then
     if (b_for_carrying)then
        if (continuation)then
           inquire(FILE="data.mand",EXIST=lookfor_cont)
           if (lookfor_cont)then
              open(unit=99,file="data.mand",status="unknown",access="stream",form="Unformatted")
              inquire(unit=99,size=data_size)
              if (data_size /= ((N**2)*4+8))then
                 if (on_root) call errors("Incompatible grid size for continuation")
              else
                 read(99)set
                 set=cshift(set,1)
              end if
           end if
        end if
     end if
  end if




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FILE WRITTING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (on_root) call File(nprocs,memory_buffer,disk_stor)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !Check for dryrun
  if(dryrun) call print_dry(on_root)

  if (on_root)then
     write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"
     write(stdout,*)"                           Starting Calculation                             <-- TIME"
     write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"
  endif

  !Do the Calculations

  init_time=COMMS_WTIME()


  call trace_entry("MAIN_CALC")
  dx = (upper_X-lower_X)/N
  dy = (upper_Y-lower_Y)/N

  !This bit for the Buddahbrot

  if (b_for_carrying)then

     buddah_set=0
     tolerance=1/(real(buddah_param)/real(nprocs))


     do buddah_iter=1,int(real(buddah_param)/(real(nprocs)))

        int_time=COMMS_WTIME()

        frac=real(buddah_iter)/real(buddah_param/nprocs)
        do comp_count=10,90,10
           if (frac.gt.real(comp_count)/100..or.&
                frac.eq.real(comp_count)/100.)then
              if (frac-real(comp_count)/100..lt.tolerance/2)then
                 call COMMS_REDUCE_DOUBLE(int_time,par_time,1,"MPI_MAX")
                 if (rank.eq.0) write(stdout,200) "Calculation",comp_count,par_time-start
              end if
           endif
        end do

        z=cmplx(0,0)
        c=random_pos()

        if (mand(max_iter,z,c,2.).gt.max_iter)then
           cycle
        end if

        buddah_counter=buddah_counter+1
        z=cmplx(0,0)
        do j=1,Max_iter
           z=z**2+c
!           if(debug)print*, abs(z)
           if (abs(z).gt.bail_out) exit

           k=int(N*(real(z)-lower_x)/(upper_x-lower_x))
           m=int(N*(aimag(z)-lower_y)/(upper_y-lower_y))

           if (m.lt.N .and. m.gt.0)then
              if (k.lt.N.and.k.gt.0)then
                 if (buddah_set(m,k).eq.0)then
                 end if
                 buddah_set(m,k)=buddah_set(m,k)+10!/(10+buddah_set(m,k))
                 !               end if
              end if
           end if
        end do
     end do

     if (rank.gt.0)then
        CALL COMMS_SEND_REAL_ARRAY2D(buddah_set,N,N,0,rank)
     end if
     if (rank.eq.0)then
        set=set+buddah_set
        do j=1,nprocs-1           
           CALL COMMS_RECV_REAL_ARRAY2D(buddah_buff,N,N,j,j)
           set=set+buddah_buff
        end do
        set=set/maxval(set)
        deallocate(buddah_buff)
     end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !and the Mandelbrot and Julia and newt

  else

     do i=1+rank*N/nprocs,(rank+1)*N/nprocs

        do j=1,N

           c = cmplx(lower_X+i*dx,lower_Y+j*dy)

           z=cmplx(z_re,z_im)

           if (J_FOR_CARRYING)then
              z=julia_const
              k=julia(Max_iter,z,c,e_default)

           elseif(newt_for_carrying)then

              theta=Newton(max_iter,c,e_default)

           elseif(burn_for_carrying)then
              z=julia_const
              k=burning(Max_iter,z,c,e_default)
           else
              z=(0,0)

              k=mand(Max_iter,z,c,e_default)

           end if


           if (lookfor_PARALLEL) then
              if(newt_for_carrying)then
                 Buffer_mpi(j)=theta+rank
              else

                 colour_ref =k -Max_iter-1+k + Max_iter*rank
                 Buffer_mpi(j)=colour_ref
              end if
           else 
              if (newt_for_carrying)then
                 Buffer_mpi(j)=theta
              else
                 colour_ref =k! -Max_iter-1+k
                 Buffer_mpi(j)=colour_ref
              end if
           end if

        end do

        if (on_root) then 

           set(i,1:N)=Buffer_mpi
           do m=1,nprocs-1
              l=l+1
              par_start=COMMS_WTIME() ! Time the parallel stuff

              CALL COMMS_RECV_INT(i_buff,1,m,1)
              CALL COMMS_RECV_REAL_ARRAY(rank_0_buff,N+1,m,1)
              par_end=COMMS_WTIME()
              par_time=par_time+par_end-par_start
              set(i_buff,1:N)=rank_0_buff
           end do
200        format(1x,A24,1x,i2,"% complete",5x,':',F13.5," s",18x,"<-- TIME")
        else
           par_start=COMMS_WTIME()

           CALL COMMS_SEND_INT(i,1,0,1)

           CALL COMMS_SEND_REAL_ARRAY(Buffer_mpi,N+1,0,1)
           par_end=COMMS_WTIME()
           par_time=par_time+par_end-par_start
        end if


        !Timing the steps           
        int_time=COMMS_WTIME()

        tolerance=1/(real(N)/real(nprocs))
        frac=real(i-rank*real(N)/real(nprocs))/(real(N)/real(nprocs))
        do comp_count=10,90,10
           if (frac.gt.real(comp_count)/100..or.&
                frac.eq.real(comp_count)/100.)then
              if (frac-real(comp_count)/100..lt.tolerance/2)then
                 call COMMS_REDUCE_DOUBLE(int_time,fini_buff,1,"MPI_MAX")
                 if (on_root)then
                    write(stdout,200) "Calculation",comp_count,fini_buff-start
                 end if
              end if
           endif
        end do



     end do
  end if
  call trace_exit("MAIN_CALC")
  after_calc=COMMS_WTIME()

  !plotting perimeter points

  ! open(unit=12,file="perim.mand")
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



  if (lookfor_data.and.on_root)then
     !if(debug)write(stdout,*)set
     write(2)set
     close(2)
  end if

  finish = COMMS_WTIME()
  call COMMS_REDUCE_DOUBLE(start,inp_st,1,"MPI_MIN")
  CALL COMMS_REDUCE_DOUBLE(finish,inp_fn,1,"MPI_MAX")
  CALL COMMS_REDUCE_DOUBLE(finish-start,total_proc_times,1,"MPI_SUM")
  CALL COMMS_REDUCE_DOUBLE(init_time,init_buff,1,"MPI_MAX")
  if (b_for_carrying)then
     CALL COMMS_REDUCE_INT(buddah_counter,buddah_buff_counter,1,"MPI_SUM")
  end if
  CALL COMMS_REDUCE_DOUBLE(par_time,tot_par_time,1,"MPI_SUM")
  CALL COMMS_REDUCE_DOUBLE(after_calc,after_calc_buff,1,"MPI_MAX")

  !FINALISE THE TRACE
  call trace_finalise(rank,debug)


  CALL COMMS_REDUCE_REAL(comms_time,comms_time_buff,1,"MPI_SUM")
  comms_time=comms_time_buff/nprocs


  if (on_root)then



     !Finish Files and close
     write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"

     write(stdout,1001)"Initialisation Time", init_buff-inp_st
     write(stdout,1001)"Finalisation Time", inp_fn-after_calc_buff
     write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"
     write(stdout,1001)"Total Calculation Time", inp_fn-inp_st

1001 format(1x,A37,5x,':',F13.5," s",18x,"<-- TIME")   
     write(stdout,*)"--------------------------------------------------------------------------- <-- TIME"
     write(stdout,*)
     if (nprocs.gt.1) write(stdout,304) "Parallel Efficiency:", 100*(1-comms_time/inp_fn-inp_st),"%"
     if (b_for_carrying)then 
        write(stdout,304) "Buddahbrot Efficiency:",100.*real(buddah_buff_counter)/real(buddah_param),"%"
304     format(1x,A,12x,f6.2,1x,A)
     end if


     !**********FOLLOWING LINES FOR PARALLEL EFFICIENCY TESTING*************
     if (lookfor_eff)then
        open(unit=3,file="efficiency.mand",form="formatted",status="unknown",access="append")
        write(3,*)nprocs,inp_fn-inp_st,"#N=",N,comms_arch
        close(3)
        write(stdout,*) "Efficiency data writen to file 'efficiency.mand'"
     end if
     if (lookfor_data)then
        write(stdout,*)'Calculation written to file "data.mand"'
     end if



  end if
  close(stdout)


  CALL COMMS_FINALISE()


end program Mandelbrot

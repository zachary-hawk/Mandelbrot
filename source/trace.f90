!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!---- File documented by Fortran Documenter, Z.Hawkhead
!----file documented by Fortran Documenter, Z.Hawkhead
!=============================================================================!
!                                 T R A C E                                   !
!=============================================================================!
!                 Module for profiling the Mandelbrot code                    !
!-----------------------------------------------------------------------------!
!                           author: Z. Hawkhead                               !
!=============================================================================!
module trace
  implicit none
  real,dimension(:),allocatable             :: entry_time_array
  real,dimension(:),allocatable             :: exit_time_array
  character(50),dimension(:),allocatable    :: entry_array
  character(50),dimension(:),allocatable    :: exit_array
  real,dimension(:),allocatable             :: temp_real_array
  character(50),dimension(:),allocatable    :: temp_char_array
  character(50),dimension(:),allocatable    :: unique_array
  real                                      :: comms_start_time
  real                                      :: io_start_time
  real                                      :: comms_end_time
  real                                      :: io_end_time
  real                                      :: comms_time
  real                                      :: io_time
  integer                                   :: no_subs

contains

  subroutine trace_init()
    !==============================================================================!
    !                             T R A C E _ I N I T                              !
    !==============================================================================!
    ! Initialisation for the profiling tools in the trace module. Allocates the    !
    ! arrays for the trace log such that they are ready for appending.             !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    allocate(entry_array(1))
    allocate(entry_time_array(1))
    allocate(exit_array(1))
    allocate(exit_time_array(1))
  end subroutine trace_init


  subroutine trace_entry(sub_name)
    !==============================================================================!
    !                            T R A C E _ E N T R Y                             !
    !==============================================================================!
    ! Subroutine to be called at the start of every other subroutine. Adds the     !
    ! name of the subroutine to the log and initialises a timer for the            !
    ! subroutine of interest.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           sub_name,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    character(*), intent(in) :: sub_name
    real                      :: time

    call CPU_TIME(time)

    ! check for comms
    if (index(sub_name,"COMMS").gt.0) comms_start_time=comms_start_time+time
    if (index(sub_name,"IO_").gt.0) io_start_time=io_start_time+time
    !set the things to the last array element

    entry_array(size(entry_array))=sub_name
    entry_time_array(size(entry_time_array))=time

    !increase the size of the array

    allocate(temp_real_array(1:size(entry_time_array)+1))
    temp_real_array(1:size(entry_time_array))=entry_time_array
    call  move_alloc(temp_real_array,entry_time_array)

    allocate(temp_char_array(1:size(entry_array)+1))
    temp_char_array(1:size(entry_array))=entry_array
    call  move_alloc(temp_char_array,entry_array)


    if (allocated(temp_real_array)) deallocate(temp_real_array)
    if (allocated(temp_char_array)) deallocate(temp_char_array)

  end subroutine trace_entry



  subroutine trace_exit(sub_name)
    !==============================================================================!
    !                             T R A C E _ E X I T                              !
    !==============================================================================!
    ! Subroutine to be called at the exit of every other subroutine. Adds the      !
    ! name of the subroutine to the log and initialises a timer for the exit       !
    ! time.                                                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           sub_name,          intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    character(*), intent(in)      :: sub_name
    real                          :: time

    call CPU_TIME(time)
    ! check for comms
    if (index(sub_name,"COMMS").gt.0) comms_end_time=comms_end_time+time
    if (index(sub_name,"IO_").gt.0) io_end_time=io_end_time+time

    !set the things to the last array element

    exit_array(size(exit_array))=sub_name
    exit_time_array(size(exit_time_array))=time

    !increase the size of the array

    allocate(temp_real_array(1:size(exit_time_array)+1))
    temp_real_array(1:size(exit_time_array))=exit_time_array
    call  move_alloc(temp_real_array,exit_time_array)

    allocate(temp_char_array(1:size(exit_array)+1))
    temp_char_array(1:size(exit_array))=exit_array
    call  move_alloc(temp_char_array,exit_array)


    if (allocated(temp_real_array)) deallocate(temp_real_array)
    if (allocated(temp_char_array)) deallocate(temp_char_array)

  end subroutine trace_exit



  subroutine trace_finalise(rank,debug)
    !==============================================================================!
    !                         T R A C E _ F I N A L I S E                          !
    !==============================================================================!
    ! Subroutine that finalises the trace module, no further profiling can         !
    ! happen once this module is called.                                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !           debug,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    integer                          :: i,k
    logical,intent(in)               :: debug
    real,dimension(:),allocatable    :: start_time_sum
    real,dimension(:),allocatable    :: end_time_sum
    real,dimension(:),allocatable    :: sub_times
    integer,dimension(:),allocatable :: call_count
    character(len=50),dimension(:),allocatable :: unique_subs_trimmed
    integer,intent(inout)            :: rank
    ! TRIM DOWN THE ARRAYS

    print*,entry_array
    print*,exit_array


    allocate(temp_real_array(1:size(entry_time_array)-1))
    temp_real_array(1:size(entry_time_array))=entry_time_array
    call  move_alloc(temp_real_array,entry_time_array)

    allocate(temp_char_array(1:size(entry_array)-1))
    temp_char_array(1:size(entry_array))=entry_array
    call  move_alloc(temp_char_array,entry_array)


    allocate(temp_real_array(1:size(exit_time_array)-1))
    temp_real_array(1:size(exit_time_array))=exit_time_array
    call  move_alloc(temp_real_array,exit_time_array)

    allocate(temp_char_array(1:size(exit_array)-1))
    temp_char_array(1:size(exit_array))=exit_array
    call  move_alloc(temp_char_array,exit_array)

    ! DEFINE THE COMM TIME- ACCESSIBLE GLOBALLY
    comms_time=abs(comms_end_time-comms_start_time)
    io_time=abs(io_end_time-io_start_time)



    if (allocated(temp_real_array)) deallocate(temp_real_array)
    if (allocated(temp_char_array)) deallocate(temp_char_array)


    ! HANDLE ALL OF THE UNIQUE CHECKING AND SUMMING UP THE SHIT


    ! set up the array to fill with the unique subroutines, null packs the end 
    allocate(Unique_array(1:size(entry_array)))


    ! HERE IS THE CALL THAT DOES ALL OF THE FUN STUFF, SORTS SUBROUTINES AND RETURNS AN ARRAY OF UNIQUE ARRAYS
    call trace_unique(entry_array,unique_array,no_subs)


    ! ALLOCATE THE TIMING ARRAYS

    allocate(start_time_sum(1:no_subs),end_time_sum(1:no_subs))
    allocate(sub_times(1:no_subs))
    allocate(call_count(1:no_subs))
    sub_times=0
    call_count=0
    start_time_sum=0
    end_time_sum=0
    

    ! ALLOCATE THE UNIQUE SUBS ARRAY SO THERE ARE NO EMPTY CRAP AT THE END

    allocate(unique_subs_trimmed(1:no_subs))
    unique_subs_trimmed=unique_array(1:no_subs)



    do i=1,size(unique_subs_trimmed)
       do k=1,size(entry_array)

          if (entry_array(k).eq.unique_subs_trimmed(i))then
             start_time_sum(i)=start_time_sum(i)+entry_time_array(k)
             call_count(i)=call_count(i)+1
          end if
          if (exit_array(k).eq.unique_subs_trimmed(i))then
             end_time_sum(i)=end_time_sum(i)+exit_time_array(k)
          end if
       end do
    end do

    sub_times=abs(end_time_sum-start_time_sum)

    call trace_sort(sub_times,unique_subs_trimmed,call_count)



    if(debug)call trace_IO(rank,unique_subs_trimmed,sub_times,call_count)

    deallocate(unique_subs_trimmed)


    deallocate(call_count)
    deallocate(sub_times)
    deallocate(start_time_sum,end_time_sum)    

  end subroutine trace_finalise



  subroutine trace_unique(unsorted_array,out_array,k)
    !==============================================================================!
    !                           T R A C E _ U N I Q U E                            !
    !==============================================================================!
    ! Subroutine to extract unique entries in the trace log and reduce them to a   !
    ! new array.                                                                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           unsorted_array,    intent :: in                                    !
    !           out_array,         intent :: inout                                 !
    !           k,                 intent :: out                                   !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    character(len=50),intent(in),dimension(:)    :: unsorted_array         ! The input
    character(len=50),dimension(:),allocatable   :: res  ! The output
    character(len=50),dimension(:),intent(inout) :: out_array
    integer,intent(out)                          :: k                   ! The number of unique elements
    integer :: i, j

    allocate(res(size(unsorted_array)))
    k = 1
    res(1) = unsorted_array(1)
    outer: do i=2,size(unsorted_array)
       do j=1,k
          if (res(j) == unsorted_array(i)) then
             ! Found a match so start looking again
             cycle outer
          end if
       end do
       ! No match found so add it to the output
       k = k + 1
       res(k) = unsorted_array(i)
    end do outer
    out_array(1:size(res))=res

  end subroutine trace_unique


  subroutine trace_sort(array_to_sort,subs_array,count_array)
    !==============================================================================!
    !                             T R A C E _ S O R T                              !
    !==============================================================================!
    ! Subroutine that implements the Bubble Sort algorithm for sorting the total   !
    ! time spent in each subroutine into descending order. Also acts on the list   !
    ! of subroutine names and the call log so as to keep the orders consistent.    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           array_to_sort,     intent :: inout                                 !
    !           subs_array,        intent :: inout                                 !
    !           count_array,       intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    real,dimension(:),intent(inout)              :: array_to_sort
    character(len=50),dimension(:),intent(inout) :: subs_array
    character(len=50),dimension(:),allocatable   :: temp_array_char
    real,dimension(:),allocatable                :: temp_array
    integer,dimension(:),intent(inout)           :: count_array
    integer,dimension(:),allocatable             :: temp_count
    integer                         :: i,j,k

    allocate(temp_array(1:size(array_to_sort)))
    allocate(temp_array_char(1:size(array_to_sort)))



    do j=1,size(array_to_sort)-1
       do i=1,size(array_to_sort)-j
          temp_array=array_to_sort
          temp_array_char=subs_array
          temp_count=count_array
          if (array_to_sort(i).lt.array_to_sort(i+1))then
             temp_array(i)=temp_array(i+1)
             temp_array(i+1)=array_to_sort(i)
             temp_array_char(i)=temp_array_char(i+1)
             temp_array_char(i+1)=subs_array(i)
             temp_count(i)=temp_count(i+1)
             temp_count(i+1)=count_array(i)

             array_to_sort=temp_array
             subs_array=temp_array_char
             count_array=temp_count

          else
             cycle
          end if
       end do
    end do

  end subroutine trace_sort


  subroutine trace_IO(rank,subs_list,time_list,call_list)
    !==============================================================================!
    !                               T R A C E _ I O                                !
    !==============================================================================!
    ! Subroutine for opening and writing the profiling files, one per MPI          !
    ! instance.                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !           subs_list,         intent :: in                                    !
    !           time_list,         intent :: in                                    !
    !           call_list,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  26/08/2019                                            !
    !==============================================================================!
    implicit none
    integer,intent(inout)                 :: rank
    character(50),dimension(:),intent(in) :: subs_list
    real,dimension(:),intent(in)          :: time_list
    integer,dimension(:),intent(in)       :: call_list
    integer                               :: file_id
    character(3)                          :: rank_char
    integer                               :: i
    file_id=rank*294+89

    write(rank_char,'(i3)')rank

    do i=1,3
       if (rank_char(i:i).eq." ") rank_char(i:i)="0"
    end do

    open(unit=file_id,file="profile."//rank_char//".mand",RECL=8192,form="FORMATTED",status="UNKNOWN")

    write(file_id,*) "+==================================================================================+"
    write(file_id,*) "| MM    MM   AAA   NN   NN DDDDD   EEEEEEE LL      BBBBB   RRRRRR   OOOOO  TTTTTTT |"
    write(file_id,*) "| MMM  MMM  AAAAA  NNN  NN DD  DD  EE      LL      BB   B  RR   RR OO   OO   TTT   |"
    write(file_id,*) "| MM MM MM AA   AA NN N NN DD   DD EEEEE   LL      BBBBBB  RRRRRR  OO   OO   TTT   |"
    write(file_id,*) "| MM    MM AAAAAAA NN  NNN DD   DD EE      LL      BB   BB RR  RR  OO   OO   TTT   |"
    write(file_id,*) "| MM    MM AA   AA NN   NN DDDDDD  EEEEEEE LLLLLLL BBBBBB  RR   RR  OOOO0    TTT   |"
    write(file_id,*) "+==================================================================================+"
    write(file_id,*) "|                                                                                  |"
    write(file_id,9)  rank_char

9   FORMAT(1x,"|",30x,"P R O F I L E :",2x,A,32x,"|")

    write(file_id,*) "|                                                                                  |"
    write(file_id,*) "+==================================================================================+"
    write(file_id,*) "|       Subroutine:                 Call Count:                  Time:             |"
    write(file_id,*) "+==================================================================================+"
    write(file_id,*) "|                                                                                  |"
19  format(1x,"|",3x,A25,10x,i5,19x,f10.4,1x,"s",8x,"|")
    do i=1,size(subs_list)
       write(file_id,19) adjustl(subs_list(i)),call_list(i),time_list(i)

    end do
    write(file_id,*) "|                                                                                  |"
    write(file_id,*) "+----------------------------------------------------------------------------------+"
!    write(file_id,*) "|                                                                                  |"
    write(file_id,'(1x,A,3x,A,3x,i3,48x,a)') "|","No. Subroutines Profiled:",size(subs_list),"|"
    write(file_id,'(1x,A,3x,A,4x,f10.5,1x,a,43x,a)') "|","Time Spent in COMMS:",comms_time,"s","|"
    write(file_id,'(1x,A,3x,A,4x,f10.5,1x,a,43x,a)') "|","Time Spent in IO   :",io_time,"s","|"    
!    write(file_id,*) "|                                                                                  |"
    write(file_id,*) "+==================================================================================+"
    close(file_id)
  end subroutine trace_IO



  subroutine trace_parents()
!==============================================================================!
!                          T R A C E _ P A R E N T S                           !
!==============================================================================!
! Subroutine for identifying the parents of a different subroutine currently   !
! being traced. Puts the subroutines in order and counts the number of         !
! parents.                                                                     !
!------------------------------------------------------------------------------!
! Arguments:                                                                   !
!           None                                                               !
!------------------------------------------------------------------------------!
! Author:   Z. Hawkhead  01/10/2019                                            !
!==============================================================================!
    implicit none


!    there is something here 
    

    

  end subroutine trace_parents
    

end module trace

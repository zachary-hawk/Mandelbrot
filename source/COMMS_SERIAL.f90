!=============================================================================! 
!                                  COMMS                                      !
!=============================================================================!
!              Module handining comminications: SERIAL                        !
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================! 
module COMMS

  implicit none

  integer :: ierr
  integer ::  max_version_length=1
  character(6) :: comms_arch="SERIAL"
contains


  subroutine COMMS_LIBRARY_VERSION(MPI_version)
    character(len=max_version_length),intent(inout) :: MPI_version
    integer ::length
  end subroutine COMMS_LIBRARY_VERSION

  subroutine COMMS_ABORT(message)
    character(*) :: message

  end subroutine COMMS_ABORT

  subroutine COMMS_VERSION(maj_mpi,min_mpi)
    integer, intent(inout):: maj_mpi,min_mpi

    call MPI_GET_VERSION(maj_MPI,min_MPI,ierr)


  end subroutine COMMS_VERSION


  subroutine COMMS_INIT()
    integer :: ierr

  end subroutine COMMS_INIT

  subroutine COMMS_FINALISE()
    integer :: ierr

  end subroutine COMMS_FINALISE


  !Rank and size

  subroutine COMMS_RANK(rank)
    integer,intent(inout) :: rank


  end subroutine COMMS_RANK

  subroutine COMMS_SIZE(nprocs)
    integer,intent(inout) :: nprocs
    nprocs=1
  end subroutine COMMS_SIZE


  !Send Routines
  subroutine COMMS_SEND_INT(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    integer :: send_buff
    !    print*,send_buff
    print*, "send_int success"

  end subroutine COMMS_SEND_INT

  subroutine COMMS_SEND_REAL(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    real :: send_buff

  end subroutine COMMS_SEND_REAL

  subroutine COMMS_SEND_DOUBLE(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    double precision :: send_buff

  end subroutine COMMS_SEND_DOUBLE
  !SEND_Arrays
  subroutine COMMS_SEND_INT_ARRAY(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    integer,dimension(count) :: send_buff

  end subroutine COMMS_SEND_INT_ARRAY

  subroutine COMMS_SEND_REAL_ARRAY(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    real,dimension(count) :: send_buff



  end subroutine COMMS_SEND_REAL_ARRAY

  subroutine COMMS_SEND_DOUBLE_ARRAY(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    double precision,dimension(count) :: send_buff

  end subroutine COMMS_SEND_DOUBLE_ARRAY


  !2D array
  subroutine COMMS_SEND_REAL_ARRAY2D(send_buff,count1,count2,dest_rank,tag)
    integer:: count1,count2,dest_rank,tag
    real,dimension(count1,count2) :: send_buff



  end subroutine COMMS_SEND_REAL_ARRAY2D




  !Recv Routines

  subroutine COMMS_RECV_INT(recv_buff,count,source,tag)
    integer:: count,source,tag
    integer, intent(inout) :: recv_buff
    !   print*, "message in routine"

    !    print*, "after",recv_buff
  end subroutine COMMS_RECV_INT

  subroutine COMMS_RECV_REAL(recv_buff,count,source,tag)
    integer:: count,source,tag
    real, intent(inout) :: recv_buff

    !   print*, "Recv sent to routine"

    !  print*, "Recv success from rank",source 

  end subroutine COMMS_RECV_REAL

  subroutine COMMS_RECV_DOUBLE(recv_buff,count,source,tag)
    integer:: count,source,tag
    double precision ,intent(inout) :: recv_buff

  end subroutine COMMS_RECV_DOUBLE
  !RECV ROUTINES ARRAY
  subroutine COMMS_RECV_INT_ARRAY(recv_buff,count,source,tag)
    integer:: count,source,tag
    integer,dimension(count),intent(inout) :: recv_buff

  end subroutine COMMS_RECV_INT_ARRAY

  subroutine COMMS_RECV_REAL_ARRAY(recv_buff,count,source,tag)
    integer :: count,source,tag
    real,dimension(count),intent(inout) :: recv_buff

  end subroutine COMMS_RECV_REAL_ARRAY

  subroutine COMMS_RECV_DOUBLE_ARRAY(recv_buff,count,source,tag)
    integer :: count,source,tag
    double precision,dimension(count),intent(inout) :: recv_buff

  end subroutine COMMS_RECV_DOUBLE_ARRAY

  !2D array
  subroutine COMMS_RECV_REAL_ARRAY2D(recv_buff,count1,count2,source,tag)
    integer :: count1,count2,source,tag
    real,dimension(count1,count2),intent(inout) :: recv_buff

  end subroutine COMMS_RECV_REAL_ARRAY2D






  !Reduce Routines

  subroutine COMMS_REDUCE_INT(send_buff,recv_buff,count,OP)
    integer:: count
    integer,intent(inout) :: recv_buff
    integer :: send_buff
    character(*) :: OP
    recv_buff=send_buff
    if (trim(OP).eq."MPI_MAX")then

    elseif (trim(OP).eq."MPI_MIN")then

    elseif (trim(OP).eq."MPI_SUM")then

    end if
  end subroutine COMMS_REDUCE_INT

  subroutine COMMS_REDUCE_REAL(send_buff,recv_buff,count,OP)
    integer:: count
    real,intent(inout) :: recv_buff
    real :: send_buff
    character(*) :: OP
    recv_buff=send_buff
    if (trim(OP).eq."MPI_MAX")then

    elseif (trim(OP).eq."MPI_MIN")then

    elseif (trim(OP).eq."MPI_SUM")then

    end if
  end subroutine COMMS_REDUCE_REAL

  subroutine COMMS_REDUCE_DOUBLE(send_buff,recv_buff,count,OP)
    integer:: count
    double precision,intent(inout) :: recv_buff
    double precision :: send_buff
    character(*) :: OP
    recv_buff=send_buff
    if (trim(OP).eq."MPI_MAX")then

    elseif (trim(OP).eq."MPI_MIN")then

    elseif (trim(OP).eq."MPI_SUM")then

    end if
  end subroutine COMMS_REDUCE_DOUBLE

  ! ARRAY

  subroutine COMMS_REDUCE_INT_ARRAY(send_buff,recv_buff,count,OP)
    integer:: count
    integer,dimension(count),intent(inout) :: recv_buff
    integer,dimension(count) :: send_buff
    character(*) :: OP

    if (trim(OP).eq."MPI_MAX")then

    elseif (trim(OP).eq."MPI_MIN")then

    elseif (trim(OP).eq."MPI_SUM")then

    end if
  end subroutine COMMS_REDUCE_INT_ARRAY

  subroutine COMMS_REDUCE_REAL_ARRAY(send_buff,recv_buff,count,OP)
    integer:: count
    real,dimension(count),intent(inout) :: recv_buff
    real,dimension(count) :: send_buff
    character(*) :: OP
    recv_buff=send_buff
    if (trim(OP).eq."MPI_MAX")then

    elseif (trim(OP).eq."MPI_MIN")then

    elseif (trim(OP).eq."MPI_SUM")then

    end if
  end subroutine COMMS_REDUCE_REAL_ARRAY

  subroutine COMMS_REDUCE_DOUBLE_ARRAY(send_buff,recv_buff,count,OP)
    integer:: count
    double precision,dimension(count),intent(inout) :: recv_buff
    double precision,dimension(count) :: send_buff
    character(*) :: OP
    recv_buff=send_buff
    if (trim(OP).eq."MPI_MAX")then

    elseif (trim(OP).eq."MPI_MIN")then

    elseif (trim(OP).eq."MPI_SUM")then

    end if
  end subroutine COMMS_REDUCE_DOUBLE_ARRAY







  !BCAST routines
  subroutine COMMS_BCAST_INT(start_buff,count)
    integer :: count
    integer :: start_buff

  end subroutine COMMS_BCAST_INT

  subroutine COMMS_BCAST_REAL(start_buff,count)
    integer :: count
    real :: start_buff

  end subroutine COMMS_BCAST_REAL

  subroutine COMMS_BCAST_DOUBLE(start_buff,count)
    integer :: count
    double precision :: start_buff

  end subroutine COMMS_BCAST_DOUBLE
  !ARRAY
  subroutine COMMS_BCAST_INT_ARRAY(start_buff,count)
    integer :: count
    integer,dimension(count) :: start_buff

  end subroutine COMMS_BCAST_INT_ARRAY

  subroutine COMMS_BCAST_REAL_ARRAY(start_buff,count)
    integer :: count
    real,dimension(count) :: start_buff

  end subroutine COMMS_BCAST_REAL_ARRAY

  subroutine COMMS_BCAST_DOUBLE_ARRAY(start_buff,count)
    integer :: count
    double precision,dimension(count) :: start_buff

  end subroutine COMMS_BCAST_DOUBLE_ARRAY




  function COMMS_WTIME() result(time)
    double precision :: time
    call CPU_TIME(time)

  end function COMMS_WTIME







end module COMMS

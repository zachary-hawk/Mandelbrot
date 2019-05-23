module COMMS_MPI

  implicit none
  include 'mpif.h'
  integer :: ierr
contains

  subroutine COMMS_INIT()
    integer :: ierr
    call MPI_INIT(ierr)
  end subroutine COMMS_INIT

  subroutine COMMS_FINALISE()
    integer :: ierr
    call MPI_FINALIZE(ierr)
  end subroutine COMMS_FINALISE


  !Rank and size

  subroutine COMMS_RANK(rank)
    integer,intent(inout) :: rank
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

  end subroutine COMMS_RANK

  subroutine COMMS_SIZE(nprocs)
    integer,intent(inout) :: nprocs
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
  end subroutine COMMS_SIZE


  !Send Routines
  subroutine COMMS_SEND_INT(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    integer :: send_buff
    print*,send_buff
    call MPI_SEND(send_buff,count,MPI_INT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    
  end subroutine COMMS_SEND_INT

  subroutine COMMS_SEND_REAL(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    real :: send_buff
    call MPI_SEND(send_buff,count,MPI_FLOAT,dest_rank,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_SEND_REAL

  subroutine COMMS_SEND_DOUBLE(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    double precision :: send_buff
    call MPI_SEND(send_buff,count,MPI_DOUBLE,dest_rank,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_SEND_DOUBLE
  !SEND_Arrays
  subroutine COMMS_SEND_INT_ARRAY(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    integer,dimension(count) :: send_buff
    call MPI_SEND(send_buff,count,MPI_INT,dest_rank,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_SEND_INT_ARRAY

  subroutine COMMS_SEND_REAL_ARRAY(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    real,dimension(count) :: send_buff
    
    call MPI_SEND(send_buff,count,MPI_FLOAT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    
  end subroutine COMMS_SEND_REAL_ARRAY

  subroutine COMMS_SEND_DOUBLE_ARRAY(send_buff,count,dest_rank,tag)
    integer:: count,dest_rank,tag
    double precision,dimension(count) :: send_buff
    call MPI_SEND(send_buff,count,MPI_DOUBLE,dest_rank,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_SEND_DOUBLE_ARRAY







  !Recv Routines

  subroutine COMMS_RECV_INT(recv_buff,count,source,tag)
    integer:: count,source,tag
    integer, intent(inout) :: recv_buff
    print*, "message in routine"
    call MPI_RECV(recv_buff,count,MPI_INT,source,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_RECV_INT

  subroutine COMMS_RECV_REAL(recv_buff,count,source,tag)
    integer:: count,source,tag
    real, intent(inout) :: recv_buff
    call MPI_RECV(recv_buff,count,MPI_REAL,source,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_RECV_REAL

  subroutine COMMS_RECV_DOUBLE(recv_buff,count,source,tag)
    integer:: count,source,tag
    double precision ,intent(inout) :: recv_buff
    call MPI_RECV(recv_buff,count,MPI_DOUBLE,source,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_RECV_DOUBLE
  !RECV ROUTINES ARRAY
  subroutine COMMS_RECV_INT_ARRAY(recv_buff,count,source,tag)
    integer:: count,source,tag
    integer,dimension(count),intent(inout) :: recv_buff
    call MPI_RECV(recv_buff,count*MPI_INT,source,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_RECV_INT_ARRAY

  subroutine COMMS_RECV_REAL_ARRAY(recv_buff,count,source,tag)
    integer :: count,source,tag
    real,dimension(count),intent(inout) :: recv_buff
    call MPI_RECV(recv_buff,count*MPI_REAL,source,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_RECV_REAL_ARRAY

  subroutine COMMS_RECV_DOUBLE_ARRAY(recv_buff,count,source,tag)
    integer :: count,source,tag
    double precision,dimension(count),intent(inout) :: recv_buff
    call MPI_RECV(recv_buff,count*MPI_DOUBLE,source,tag,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_RECV_DOUBLE_ARRAY








  !Reduce Routines

  subroutine COMMS_REDUCE_INT(send_buff,recv_buff,count,OP)
    integer:: count
    integer,intent(inout) :: recv_buff
    integer :: send_buff
    character(*) :: OP

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MIN,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if
  end subroutine COMMS_REDUCE_INT

  subroutine COMMS_REDUCE_REAL(send_buff,recv_buff,count,OP)
    integer:: count
    real,intent(inout) :: recv_buff
    real :: send_buff
    character(*) :: OP

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MIN,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if
  end subroutine COMMS_REDUCE_REAL

  subroutine COMMS_REDUCE_DOUBLE(send_buff,recv_buff,count,OP)
    integer:: count
    double precision,intent(inout) :: recv_buff
    double precision :: send_buff
    character(*) :: OP

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MIN,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if
  end subroutine COMMS_REDUCE_DOUBLE

  ! ARRAY

  subroutine COMMS_REDUCE_INT_ARRAY(send_buff,recv_buff,count,OP)
    integer:: count
    integer,dimension(count),intent(inout) :: recv_buff
    integer,dimension(count) :: send_buff
    character(*) :: OP
    
    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MIN,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if
  end subroutine COMMS_REDUCE_INT_ARRAY

  subroutine COMMS_REDUCE_REAL_ARRAY(send_buff,recv_buff,count,OP)
    integer:: count
    real,dimension(count),intent(inout) :: recv_buff
    real,dimension(count) :: send_buff
    character(*) :: OP

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MIN,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if
  end subroutine COMMS_REDUCE_REAL_ARRAY

  subroutine COMMS_REDUCE_DOUBLE_ARRAY(send_buff,recv_buff,count,OP)
    integer:: count
    double precision,dimension(count),intent(inout) :: recv_buff
    double precision,dimension(count) :: send_buff
    character(*) :: OP

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MIN,0,MPI_COMM_WORLD,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if
  end subroutine COMMS_REDUCE_DOUBLE_ARRAY







  !BCAST routines
  subroutine COMMS_BCAST_INT(start_buff,count)
    integer :: count
    integer :: start_buff
    call MPI_BCAST(start_buff, count,MPI_INT,0,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_BCAST_INT

  subroutine COMMS_BCAST_REAL(start_buff,count)
    integer :: count
    real :: start_buff
    call MPI_BCAST(start_buff, count,MPI_FLOAT,0,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_BCAST_REAL

  subroutine COMMS_BCAST_DOUBLE(start_buff,count)
    integer :: count
    double precision :: start_buff
    call MPI_BCAST(start_buff, count,MPI_DOUBLE,0,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_BCAST_DOUBLE
  !ARRAY
  subroutine COMMS_BCAST_INT_ARRAY(start_buff,count)
    integer :: count
    integer,dimension(count) :: start_buff
    call MPI_BCAST(start_buff, count,MPI_INT,0,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_BCAST_INT_ARRAY

  subroutine COMMS_BCAST_REAL_ARRAY(start_buff,count)
    integer :: count
    real,dimension(count) :: start_buff
    call MPI_BCAST(start_buff, count,MPI_FLOAT,0,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_BCAST_REAL_ARRAY

  subroutine COMMS_BCAST_DOUBLE_ARRAY(start_buff,count)
    integer :: count
    double precision,dimension(count) :: start_buff
    call MPI_BCAST(start_buff, count,MPI_DOUBLE,0,MPI_COMM_WORLD,ierr)
  end subroutine COMMS_BCAST_DOUBLE_ARRAY




  function COMMS_WTIME() result(time)
    double precision :: time
    time = MPI_WTIME()

  end function COMMS_WTIME







end module COMMS_MPI

!---- File documented by ForOAAtran Documenter, Z.Hawkhead
!=============================================================================!
!                                  COMMS                                      !
!=============================================================================!
!              Module handining comminications: MPI                           !
!-----------------------------------------------------------------------------!
!                        author: Z. Hawkhead                                  !
!=============================================================================! 
module COMMS
  !use mpi
  use iso_fortran_env , only : real64
  use trace
  implicit none
  include 'mpif.h'
  integer                              :: ierr
  integer,parameter                    :: max_version_length=MPI_MAX_LIBRARY_VERSION_STRING
  integer, dimension(MPI_STATUS_SIZE)  :: status1
  !integer,parameter,private :: dp=real64
  ! Some of the stuff i'll need, gloabal
  integer,public,save                  :: rank
  integer,public,save                  :: nprocs
  logical,public,save                  :: on_root_node
  
  character(3)                         :: comms_arch="MPI"
  integer,public,save,dimension(:,:),allocatable :: comms_scheme_array
  logical,public :: u_scheme=.false.



  interface comms_send
     module procedure COMMS_SEND_INT
     !module procedure COMMS_SEND_REAL
     module procedure COMMS_SEND_DOUBLE
     module procedure COMMS_SEND_INT_ARRAY
     !module procedure COMMS_SEND_REAL_ARRAY
     module procedure COMMS_SEND_DOUBLE_ARRAY
     module procedure COMMS_SEND_REAL_ARRAY2D
  end interface 

  interface comms_recv
     module procedure COMMS_RECV_INT
     !module procedure COMMS_RECV_REAL
     module procedure COMMS_RECV_DOUBLE
     module procedure COMMS_RECV_INT_ARRAY
     !module procedure COMMS_RECV_REAL_ARRAY
     module procedure COMMS_RECV_DOUBLE_ARRAY
     module procedure COMMS_RECV_REAL_ARRAY2D
  end interface 


  interface comms_reduce
     module procedure COMMS_REDUCE_INT
     module procedure COMMS_REDUCE_REAL
     module procedure COMMS_REDUCE_DOUBLE
     module procedure COMMS_REDUCE_INT_ARRAY
     module procedure COMMS_REDUCE_REAL_ARRAY
     !module procedure COMMS_REDUCE_DOUBLE_ARRAY
     module procedure COMMS_REDUCE_LOG
  end interface comms_reduce
  
  interface comms_bcast
     module procedure COMMS_BCAST_INT
     !module procedure COMMS_BCAST_REAL
     module procedure COMMS_BCAST_DOUBLE
     module procedure COMMS_BCAST_INT_ARRAY
     !module procedure COMMS_BCAST_REAL_ARRAY
     module procedure COMMS_BCAST_DOUBLE_ARRAY
  end interface 

  

  !-------------------------------------------------------!
  !              P U B L I C  R O U T I N E S             !
  !-------------------------------------------------------! 

!  public comms_recv
!  public comms_send
!  public comms_reduce
!  public comms_bcast
!  public COMMS_WTIME
!  public COMMS_INIT
!  public COMMS_FINALISE
!  public COMMS_ABORT

contains

  subroutine COMMS_BARRIER()
    !==============================================================================!
    !                          C O M M S _ B A R R I E R                           !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_BARRIER command, holds each process until     !
    ! each process reaches the same place.                                         !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    call trace_entry("COMMS_BARRIER")

    call MPI_BARRIER(MPI_COMM_WORLD,1)
    call trace_exit("COMMS_BARRIER")
     
  end subroutine COMMS_BARRIER


  subroutine COMMS_ABORT(error_code)
    !==============================================================================!
    !                            C O M M S _ A B O R T                             !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_ABORT command. Calls an abort to all          !
    ! processes to kill trailing operations when an exception is met.              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           error_code,        intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: error_code
    call trace_entry("COMMS_ABORT")
    call MPI_ABORT(MPI_COMM_WORLD,error_code,ierr)
    call trace_exit("COMMS_ABORT")
     
  end subroutine COMMS_ABORT




  subroutine COMMS_VERSION(maj_mpi,min_mpi)
    !==============================================================================!
    !                          C O M M S _ V E R S I O N                           !
    !==============================================================================!
    ! Subroutine wrapper for MPI_GET_VERSION command,  s the version          !
    ! information of the installed MPI libraries.                                  !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           maj_mpi,           intent :: inout                                 !
    !           min_mpi,           intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer, intent(inout):: maj_mpi,min_mpi
    call trace_entry("COMMS_VERSION")
    call MPI_GET_VERSION(maj_MPI,min_MPI,ierr)
    call trace_exit("COMMS_VERSION")
      
  end subroutine COMMS_VERSION



  subroutine COMMS_LIBRARY_VERSION(MPI_version)
    !==============================================================================!
    !                  C O M M S _ L I B R A R Y _ V E R S I O N                   !
    !==============================================================================!
    ! Subroutine wrapper for MPI_LIBRARIES_VERSION command.                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           MPI_version,       intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character(len=max_version_length),intent(inout) :: MPI_version
    integer ::length

    CALL trace_entry("COMMS_LIBRARY_VERSION")
    call MPI_GET_LIBRARY_VERSION(MPI_version,length,ierr)
    call trace_exit("COMMS_LIBRARY_VERSION")
     
  end subroutine COMMS_LIBRARY_VERSION
  

  subroutine COMMS_PROC_NAME()
    !==============================================================================!
    !                        C O M M S _ P R O C _ N A M E                         !
    !==============================================================================!
    ! Subroutine wrapper for MPI_GET_PROC_NAME command, gets the id of the         !
    ! processor being queried.                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    character*(MPI_MAX_PROCESSOR_NAME) ::proc_name
    integer :: proc_name_len
    call trace_entry("COMMS_PROC_NAME")
    call MPI_GET_PROCESSOR_NAME(proc_name,proc_name_len,ierr)
    call trace_EXIT("COMMS_PROC_NAME")
     
  end subroutine COMMS_PROC_NAME


  subroutine COMMS_INIT()
    !==============================================================================!
    !                             C O M M S _ I N I T                              !
    !==============================================================================!
    ! Subroutine wrapper for MPI_INIT, initialises the MPI instances at the        !
    ! start of a program.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: ierr
    call trace_entry("COMMS_INIT")

    call MPI_INIT(ierr)
    call COMMS_RANK(rank)
    call COMMS_SIZE(nprocs)
    if (rank.eq.0)then
       on_root_node=.true.
    else
       on_root_node=.false.
    end if
    call trace_exit("COMMS_INIT")
     
  end subroutine COMMS_INIT

  subroutine COMMS_FINALISE()
    !==============================================================================!
    !                         C O M M S _ F I N A L I S E                          !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_FINALIZE command, finalises the MPI           !
    ! envrionment and closes. No MPI commands should be called after               !
    ! COMMS_FINALISE.                                                              !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: ierr
    call trace_entry("COMMS_FINALISE")
    call MPI_FINALIZE(ierr)
    call trace_exit("COMMS_FINALISE")
     
  end subroutine COMMS_FINALISE


  !Rank and size

  subroutine COMMS_RANK(rank)
    !==============================================================================!
    !                             C O M M S _ R A N K                              !
    !==============================================================================!
    ! Subroutine wrapper for the MPI_RANK command, allocates the rank to each      !
    ! process.                                                                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           rank,              intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(inout) :: rank
    call trace_entry("COMMS_RANK")
    call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
    call trace_exit("COMMS_RANK")
     
  end subroutine COMMS_RANK

  subroutine COMMS_SIZE(nprocs)
    !==============================================================================!
    !                             C O M M S _ S I Z E                              !
    !==============================================================================!
    ! Subrouitine wrapper for the MPI_SIZE command, initialises the number of      !
    ! processes to spawn.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           nprocs,            intent :: inout                                 !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer,intent(inout) :: nprocs
    call trace_entry("COMMS_SIZE")
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
    call trace_exit("COMMS_SIZE")
     
  end subroutine COMMS_SIZE


  !Send Routines
  subroutine COMMS_SEND_INT(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                         C O M M S _ S E N D _ I N T                          !
    !==============================================================================!
    ! Subroutine wrapper for sending 1D data of type INT.                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    integer :: send_buff
    !    print*,send_buff
    call trace_entry("COMMS_SEND_INT")
    call MPI_SEND(send_buff,count,MPI_INT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_INT")
     
  end subroutine COMMS_SEND_INT

  subroutine COMMS_SEND_REAL(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                        C O M M S _ S E N D _ R E A L                         !
    !==============================================================================!
    ! Subroutine wrapper for sending 1D data of type REAL                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp):: send_buff
    call trace_entry("COMMS_SEND_REAL")
    call MPI_SEND(send_buff,count,MPI_FLOAT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_REAL")
     
  end subroutine COMMS_SEND_REAL

  subroutine COMMS_SEND_DOUBLE(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                      C O M M S _ S E N D _ D O U B L E                       !
    !==============================================================================!
    ! Subroutine wrapper for sending 1D data of type DOUBLE                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp):: send_buff
    call trace_entry("COMMS_SEND_DOUBLE")
    call MPI_SEND(send_buff,count,MPI_DOUBLE,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_DOUBLE")
     
  end subroutine COMMS_SEND_DOUBLE


  
  subroutine COMMS_SEND_INT_ARRAY(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                   C O M M S _ S E N D _ I N T _ A R R A Y                    !
    !==============================================================================!
    ! Subroutine wrapper for sending array data of type INT.                       !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    integer,dimension(1:count) :: send_buff
    call trace_entry("COMMS_SEND_INT_ARRAY")
    call MPI_SEND(send_buff,count,MPI_INT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_INT_ARRAY")
     
  end subroutine COMMS_SEND_INT_ARRAY

  subroutine COMMS_SEND_REAL_ARRAY(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                  C O M M S _ S E N D _ R E A L _ A R R A Y                   !
    !==============================================================================!
    ! Subroutine wrapper for sending array data of type REAL.                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp),dimension(1:count) :: send_buff
    call trace_entry("COMMS_SEND_REAL_ARRAY")
    call MPI_SEND(send_buff,count,MPI_FLOAT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_REAL_ARRAY")
     
  end subroutine COMMS_SEND_REAL_ARRAY

  subroutine COMMS_SEND_DOUBLE_ARRAY(send_buff,count,dest_rank,tag)
    !==============================================================================!
    !                C O M M S _ S E N D _ D O U B L E _ A R R A Y                 !
    !==============================================================================!
    ! Subroutine wrapper for sending array data of type DOUBLE.                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count,             intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,dest_rank,tag
    real(dp),dimension(1:count) :: send_buff
    call trace_entry("COMMS_SEND_DOUBLE_ARRAY")
    call MPI_SEND(send_buff,count,MPI_DOUBLE,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_DOUBLE_ARRAY")
     
  end subroutine COMMS_SEND_DOUBLE_ARRAY


  !2D array
  subroutine COMMS_SEND_REAL_ARRAY2D(send_buff,count1,count2,dest_rank,tag)
    !==============================================================================!
    !                C O M M S _ S E N D _ R E A L _ A R R A Y 2 D                 !
    !==============================================================================!
    ! Subroutine wrapper for sending 2D array data of type REAL.                   !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           count1,            intent :: in                                    !
    !           count2,            intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count1,count2,dest_rank,tag
    real(dp),dimension(count1,count2) :: send_buff
    call trace_entry("COMMS_SEND_REAL_ARRAY2D")
    call MPI_SEND(send_buff,count1*count2,MPI_FLOAT,dest_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_EXIT("COMMS_SEND_REAL_ARRAY2D")
     
  end subroutine COMMS_SEND_REAL_ARRAY2D

  subroutine COMMS_SEND_RECV_REAL_ARRAY2D(send_buff,recv_buff,count1,count2,dest_rank,tag,send_rank)
    !==============================================================================!
    !           C O M M S _ S E N D _ R E C V _ R E A L _ A R R A Y 2 D            !
    !==============================================================================!
    ! Subroutine wrapper for recieving 2D arrays of REAL data.                     !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: in                                    !
    !           count1,            intent :: in                                    !
    !           count2,            intent :: in                                    !
    !           dest_rank,         intent :: in                                    !
    !           tag,               intent :: in                                    !
    !           send_rank,         intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count1,count2,dest_rank,tag,send_rank
    real(dp),dimension(count1,count2) :: send_buff,recv_buff
    call trace_entry("COMMS_SEND_RECV_REAL_ARRAY2D")
    call MPI_SENDRECV(send_buff,count1*count2,MPI_FLOAT,dest_rank,tag,recv_buff,count1*count2,&
         MPI_FLOAT,send_rank,tag,MPI_COMM_WORLD,ierr)
    call trace_exit("COMMS_SEND_RECV_REAL_ARRAY2D")

     
  end subroutine COMMS_SEND_RECV_REAL_ARRAY2D



  !Recv Routines

  subroutine COMMS_RECV_INT(recv_buff,count,source,tag)
    !==============================================================================!
    !                         C O M M S _ R E C V _ I N T                          !
    !==============================================================================!
    ! Subroutine wrapper for recieving data of type INT.                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    integer, intent(inout) :: recv_buff
    !   print*, "message in routine"
    call trace_entry("COMMS_RECV_INT")
    call MPI_RECV(recv_buff,count,MPI_INT,source,tag,MPI_COMM_WORLD,status1,ierr)
    !    print*, "after",recv_buff
    call trace_EXIT("COMMS_RECV_INT")
     
  end subroutine COMMS_RECV_INT

  subroutine COMMS_RECV_REAL(recv_buff,count,source,tag)
    !==============================================================================!
    !                        C O M M S _ R E C V _ R E A L                         !
    !==============================================================================!
    ! Subroutine wrapper for recieving data of type REAL.                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    real(dp), intent(inout) :: recv_buff
    call trace_entry("COMMS_RECV_REAL")
    !   print*, "Recv sent to routine"
    call MPI_RECV(recv_buff,count,MPI_REAL,source,tag,MPI_COMM_WORLD,status1,ierr)
    !  print*, "Recv success from rank",source 
    call trace_exit("COMMS_RECV_REAL")
     
  end subroutine COMMS_RECV_REAL

  subroutine COMMS_RECV_DOUBLE(recv_buff,count,source,tag)
    !==============================================================================!
    !                      C O M M S _ R E C V _ D O U B L E                       !
    !==============================================================================!
    ! Subroutine wrapper for recieving data of type DOUBLE.                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    real(dp),intent(inout) :: recv_buff
    call trace_entry("COMMS_RECV_DOUBLE")
    call MPI_RECV(recv_buff,count,MPI_DOUBLE,source,tag,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_RECV_DOUBLE")
     
  end subroutine COMMS_RECV_DOUBLE




  subroutine COMMS_RECV_INT_ARRAY(recv_buff,count,source,tag)
    !==============================================================================!
    !                   C O M M S _ R E C V _ I N T _ A R R A Y                    !
    !==============================================================================!
    ! Subroutine wrapper for recieving data of type INT.                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count,source,tag
    integer,dimension(1:count),intent(inout) :: recv_buff
    call MPI_RECV(recv_buff,count,MPI_INT,source,tag,MPI_COMM_WORLD,status1,ierr)
     
  end subroutine COMMS_RECV_INT_ARRAY

  subroutine COMMS_RECV_REAL_ARRAY(recv_buff,count,source,tag)
    !==============================================================================!
    !                  C O M M S _ R E C V _ R E A L _ A R R A Y                   !
    !==============================================================================!
    ! Subroutine wrapper for recieving data of type REAL                           !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count,source,tag
    real,dimension(1:count),intent(inout) :: recv_buff
    call trace_entry("COMMS_RECV_REAL_ARRAY")
    call MPI_RECV(recv_buff,count,MPI_REAL,source,tag,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_RECV_REAL_ARRAY")
     
  end subroutine COMMS_RECV_REAL_ARRAY

  subroutine COMMS_RECV_DOUBLE_ARRAY(recv_buff,count,source,tag)
    !==============================================================================!
    !                C O M M S _ R E C V _ D O U B L E _ A R R A Y                 !
    !==============================================================================!
    ! Subroutine wrapper for recieving data of type DOUBLE.                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count,source,tag
    real(dp),dimension(1:count),intent(inout) :: recv_buff
    call trace_entry("COMMS_RECV_DOUBLE_ARRAY")
    call MPI_RECV(recv_buff,count,MPI_DOUBLE,source,tag,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_RECV_DOUBLE_ARRAY")
     
  end subroutine COMMS_RECV_DOUBLE_ARRAY

  !2D array
  subroutine COMMS_RECV_REAL_ARRAY2D(recv_buff,count1,count2,source,tag)
    !==============================================================================!
    !                C O M M S _ R E C V _ R E A L _ A R R A Y 2 D                 !
    !==============================================================================!
    ! Subroutine wrapper for reciving 2D arrays of real data.                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           recv_buff,         intent :: inout                                 !
    !           count1,            intent :: in                                    !
    !           count2,            intent :: in                                    !
    !           source,            intent :: in                                    !
    !           tag,               intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count1,count2,source,tag
    real(dp),dimension(count1,count2),intent(inout) :: recv_buff
    call trace_entry("COMMS_RECV_REAL_ARRAY2D")
    call MPI_RECV(recv_buff,count1*count2,MPI_REAL,source,tag,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_RECV_REAL_ARRAY2D")
     
  end subroutine COMMS_RECV_REAL_ARRAY2D






  !Reduce Routines

  subroutine COMMS_REDUCE_INT(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !                       C O M M S _ R E D U C E _ I N T                        !
    !==============================================================================!
    ! Subroutine wrapper for reducing integer data from all processes to the       !
    ! root.                                                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    integer,intent(inout) :: recv_buff
    integer :: send_buff
    character(*) :: OP

    call trace_entry("COMMS_REDUCE_INT")

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then

       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    end if
    call trace_exit("COMMS_REDUCE_INT")
     
  end subroutine COMMS_REDUCE_INT

  subroutine COMMS_REDUCE_REAL(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !                      C O M M S _ R E D U C E _ R E A L                       !
    !==============================================================================!
    ! Subroutine wrapper for reducing real data from all processes to the root.    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    real,intent(inout) :: recv_buff
    real:: send_buff
    character(*) :: OP
    call trace_entry("COMMS_REDUCE_REAL")
    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    end if
    call trace_exit("COMMS_REDUCE_REAL")
     
  end subroutine COMMS_REDUCE_REAL

  subroutine COMMS_REDUCE_DOUBLE(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !                    C O M M S _ R E D U C E _ D O U B L E                     !
    !==============================================================================!
    ! Subroutine wrapper for reducing real(dp) data from all processes     !
    ! to the root.                                                                 !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    real(dp),intent(inout) :: recv_buff
    real(dp):: send_buff
    character(*) :: OP

    call trace_entry("COMMS_REDUCE_DOUBLE")
    
    if (trim(OP).eq."MPI_MAX")then

       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    end if

    call trace_exit("COMMS_REDUCE_DOUBLE")
     
  end subroutine COMMS_REDUCE_DOUBLE

  ! ARRAY

  subroutine COMMS_REDUCE_INT_ARRAY(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !                 C O M M S _ R E D U C E _ I N T _ A R R A Y                  !
    !==============================================================================!
    ! Subroutine wrapper for reducing integer array data from all processes to     !
    ! the root.                                                                    !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    integer,dimension(1:count),intent(inout) :: recv_buff
    integer,dimension(1:count) :: send_buff
    character(*) :: OP

    call trace_entry("COMMS_REDUCE_INT_ARRAY")
    
    
    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    end if
    call trace_exit("COMMS_REDUCE_INT_ARRAY")
     
  end subroutine COMMS_REDUCE_INT_ARRAY

  subroutine COMMS_REDUCE_REAL_ARRAY(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !                C O M M S _ R E D U C E _ R E A L _ A R R A Y                 !
    !==============================================================================!
    ! Subroutine wrapper for reducing real array data from all processes to the    !
    ! root.                                                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    real,dimension(1:count),intent(inout) :: recv_buff
    real,dimension(1:count) :: send_buff
    character(*) :: OP
    call trace_entry("COMMS_REDUCE_REAL_ARRAY")
    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    end if
    call trace_exit("COMMS_REDUCE_REAL_ARRAY")
     
  end subroutine COMMS_REDUCE_REAL_ARRAY

  subroutine COMMS_REDUCE_DOUBLE_ARRAY(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !              C O M M S _ R E D U C E _ D O U B L E _ A R R A Y               !
    !==============================================================================!
    ! Subroutine wrapper for reducing real(dp) arrays from all             !
    ! proccesses to the root.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    real(dp),dimension(1:count),intent(inout) :: recv_buff
    real(dp),dimension(1:count) :: send_buff
    character(*) :: OP
!    print*,rank, "before"
    call trace_entry("COMMS_REDUCE_DOUBLE_ARRAY")
    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    end if
!    print*, rank,"after"
    call trace_exit("COMMS_REDUCE_DOUBLE_ARRAY")
     
  end subroutine COMMS_REDUCE_DOUBLE_ARRAY



  subroutine COMMS_REDUCE_LOG(send_buff,recv_buff,count,OP)
    !==============================================================================!
    !                       C O M M S _ R E D U C E _ L O G                        !
    !==============================================================================!
    ! Subroutine wrapper for reducing integer data from all processes to the       !
    ! root.                                                                        !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           send_buff,         intent :: in                                    !
    !           recv_buff,         intent :: inout                                 !
    !           count,             intent :: in                                    !
    !           OP,                intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer:: count
    logical,intent(inout) :: recv_buff
    logical :: send_buff
    character(*) :: OP

    call trace_entry("COMMS_REDUCE_LOG")

    if (trim(OP).eq."MPI_MAX")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MAX,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_MIN")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_MIN,0,MPI_COMM_WORLD,status1,ierr)
    elseif (trim(OP).eq."MPI_SUM")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,status1,ierr)
    else if(trim(OP).eq."MPI_LOR")then
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_LOR,0,MPI_COMM_WORLD,status1,ierr)
    else if(trim(OP).eq."MPI_LAND")then	
       call MPI_REDUCE(send_buff,recv_buff,count,MPI_INT,MPI_LAND,0,MPI_COMM_WORLD,status1,ierr)
    end if
    call trace_exit("COMMS_REDUCE_LOG")
     
  end subroutine COMMS_REDUCE_LOG




  !BCAST routines
  subroutine COMMS_BCAST_INT(start_buff,count)
    !==============================================================================!
    !                        C O M M S _ B C A S T _ I N T                         !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting integer data from the root to all        !
    ! children processes.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    integer :: start_buff
    call trace_entry("COMMS_BCAST_INT")
    call MPI_BCAST(start_buff, count,MPI_INT,0,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_BCAST_INT")
     
  end subroutine COMMS_BCAST_INT

  subroutine COMMS_BCAST_REAL(start_buff,count)
    !==============================================================================!
    !                       C O M M S _ B C A S T _ R E A L                        !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting real data from the root to all           !
    ! children processes.                                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    real(dp):: start_buff
    call trace_entry("COMMS_BCAST_REAL")
    call MPI_BCAST(start_buff, count,MPI_FLOAT,0,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_BCAST_REAL")
     
  end subroutine COMMS_BCAST_REAL

  subroutine COMMS_BCAST_DOUBLE(start_buff,count)
    !==============================================================================!
    !                     C O M M S _ B C A S T _ D O U B L E                      !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting real(dp)data from the root to   !
    ! all children processes.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    real(dp):: start_buff

    call trace_entry("COMMS_BCAST_DOUBLE")
    call MPI_BCAST(start_buff, count,MPI_DOUBLE,0,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_BCAST_DOUBLE")
     
  end subroutine COMMS_BCAST_DOUBLE
  !ARRAY
  subroutine COMMS_BCAST_INT_ARRAY(start_buff,count)
    !==============================================================================!
    !                  C O M M S _ B C A S T _ I N T _ A R R A Y                   !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting array of integer data from the root to   !
    ! all children processes.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    integer,dimension(1:count) :: start_buff
    call trace_entry("COMMS_BCAST_INT_ARRAY")
    call MPI_BCAST(start_buff, count,MPI_INT,0,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_BCAST_INT_ARRAY")
     
  end subroutine COMMS_BCAST_INT_ARRAY

  subroutine COMMS_BCAST_REAL_ARRAY(start_buff,count)
    !==============================================================================!
    !                 C O M M S _ B C A S T _ R E A L _ A R R A Y                  !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting array of real data from thr root to      !
    ! all children processes.                                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    real,dimension(1:count) :: start_buff
    call MPI_BCAST(start_buff, count,MPI_FLOAT,0,MPI_COMM_WORLD,status1,ierr)
     
  end subroutine COMMS_BCAST_REAL_ARRAY

  subroutine COMMS_BCAST_DOUBLE_ARRAY(start_buff,count)
    !==============================================================================!
    !               C O M M S _ B C A S T _ D O U B L E _ A R R A Y                !
    !==============================================================================!
    ! Subroutine wrapper for broadcasting array of real(dp) data from      !
    ! the root to all children processes.                                          !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           start_buff,        intent :: in                                    !
    !           count,             intent :: in                                    !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    integer :: count
    real(dp),dimension(1:count) :: start_buff
    call trace_entry("COMMS_BCAST_DOUBLE_ARRAY")
    call MPI_BCAST(start_buff, count,MPI_DOUBLE,0,MPI_COMM_WORLD,status1,ierr)
    call trace_exit("COMMS_BCAST_DOUBLE_ARRAY")
     
  end subroutine COMMS_BCAST_DOUBLE_ARRAY




  function COMMS_WTIME() result(time)
    !==============================================================================!
    !                            C O M M S _ W T I M E                             !
    !==============================================================================!
    ! Function wrapper to time MPI processes.                                      !
    !------------------------------------------------------------------------------!
    ! Arguments:                                                                   !
    !           None                                                               !
    !------------------------------------------------------------------------------!
    ! Result:                                                                      !
    !           time                                                               !
    !------------------------------------------------------------------------------!
    ! Author:   Z. Hawkhead  16/08/2019                                            !
    !==============================================================================!
    real(dp):: time
!    call trace_entry("COMMS_WTIME")
    time = MPI_WTIME()
!    call trace_exit("COMMS_WTIME")
     
  end function COMMS_WTIME



  subroutine COMMS_SCHEME(N)
    implicit none
    integer,intent(in) :: N
    integer :: n_par,U_par
    integer, dimension(:),allocatable :: split
    integer :: i,j,k

    call trace_entry("comms_scheme")

    ! allocate the scheme array
    if (nprocs.eq.1)then
       allocate(comms_scheme_array(0:nprocs,1:4))
    else
       allocate(comms_scheme_array(0:nprocs-1,1:4))
    end if
    !comms_scheme_array=0

    if (nprocs.gt.N)u_scheme=.true.
    
    if (nprocs.lt.N.and.nprocs.ne.1)then
       allocate(split(1:nprocs))

       split(:)=0
       do i =1,N-nprocs
          if (i.gt.size(split))exit
          split(i)=split(i)+1
       end do
       comms_scheme_array(0,1)=1
       comms_scheme_array(0,2)=comms_scheme_array(0,1)+split(1)
       do i = 1,nprocs-1
          if (i.gt.nprocs-1) exit
          comms_scheme_array(i,1)=comms_scheme_array(i-1,2)+1
          if (i.ne.nprocs-1)then
             comms_scheme_array(i,2)=comms_scheme_array(i,1)+split(i+1)          
          else
             comms_scheme_array(i,2)=N
          end if
          end do
       comms_scheme_array(:,3)=1
       comms_scheme_array(:,4)=N
    else if (nprocs.eq.1)then
       comms_scheme_array(0,1)=1
       comms_scheme_array(0,2)=N
       comms_scheme_array(0,3)=1
       comms_scheme_array(0,4)=N
       
    else

       allocate(split(1:N))
       split(:)=1
       do i =1,-N+nprocs
         
          if (i.gt.size(split))exit
          split(i)=split(i)+1
       end do

       k=0
       do i=1,N
          if (i.gt.size(split))exit
          do j=1,split(i)
             comms_scheme_array(k,1)=i
             comms_scheme_array(k,2)=i
             comms_scheme_array(k,3)=(j-1)*(N)/split(i) +1              
             if (j.ne.split(i))then
                comms_scheme_array(k,4)=j*(N)/split(i)
             else
                comms_scheme_array(k,4)=N
             end  if
             k=k+1
          end do
       end do
    end if
    
!!$    do i = 0,nprocs-1
!!$       if (on_root_node)print*,comms_scheme_array(i,:)
!!$    end do
!!$    

    call trace_exit("comms_scheme")
     
  end subroutine COMMS_SCHEME


end module COMMS

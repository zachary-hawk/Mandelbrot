module some_module


contains
  subroutine a_subroutine(here,is,a,param)
    use another_module
    implicit none
    logical,intent(in)    :: here,is
    integer,intent(inout) :: a
    real,intent(out)      :: param

    here is some code

  end subroutine a_subroutine


end module some_module

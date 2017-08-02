module BeTRNmlType
  use betr_constants           , only : betr_string_length,betr_string_length_long
  use betr_constants           , only : betr_namelist_buffer_size
  use bshr_log_mod             , only : errMsg => shr_log_errMsg
  use babortutils              , only : endrun
implicit none
  character(len=*), parameter :: mod_filename = &
       __FILE__

  type, public :: betr_nml_type

    character(len=betr_string_length)      :: reaction_method
    logical                                :: advection_on, diffusion_on, reaction_on, ebullition_on

  contains
    procedure, public  :: Init
    procedure, private :: Read_NML
  end type betr_nml_type

  contains


  !-------------------------------------------------------------------------------
  subroutine Init(this, namelist_buffer, masterproc)

  use betr_constants, only : betr_namelist_buffer_size

  implicit none

  class(betr_nml_type), intent(inout) :: this
  character(len=betr_namelist_buffer_size), optional, intent(in) :: namelist_buffer
  logical, optional, intent(in) :: masterproc

  if(present(namelist_buffer))then
    if(present(masterproc))then
      call this%Read_NML(namelist_buffer, masterproc)
    else
      call this%Read_NML(namelist_buffer)
    endif
  endif

  end subroutine Init
  !-------------------------------------------------------------------------------

  subroutine Read_NML(this, namelist_buffer, masterproc)

  use betr_constants , only : stdout
  implicit none
  class(betr_nml_type), intent(inout) :: this
  character(len=betr_namelist_buffer_size) , intent(in)  :: namelist_buffer
  logical, optional, intent(in) :: masterproc

  ! LOCAL VARIABLES:
  integer                                :: nml_error
  character(len=*), parameter            :: subname = 'ReadNamelist'
  character(len=betr_string_length)      :: reaction_method
  character(len=betr_string_length_long) :: ioerror_msg
  logical                                :: advection_on, diffusion_on, reaction_on, ebullition_on

  logical :: masterproc_loc
  namelist / betr_parameters /                  &
         reaction_method,                         &
         advection_on, diffusion_on, reaction_on, &
         ebullition_on

  masterproc_loc=.true.
  if(present(masterproc))masterproc_loc=masterproc

  reaction_method = 'mock_run'
  advection_on    = .true.
  diffusion_on    = .true.
  reaction_on     = .true.
  ebullition_on   =.true.
  ! ----------------------------------------------------------------------
  ! Read namelist from standard input.
  ! ----------------------------------------------------------------------
  if ( index(trim(namelist_buffer),'betr_parameters')/=0 )then
     ioerror_msg=''
     read(namelist_buffer, nml=betr_parameters, iostat=nml_error, iomsg=ioerror_msg)
     if (nml_error /= 0) then
        call endrun(msg="ERROR reading sbetr_driver namelist "//errmsg(mod_filename, __LINE__))
     end if
  end if

  if (masterproc_loc) then
     write(stdout, *)
     write(stdout, *) '--------------------'
     write(stdout, *)
     write(stdout, *) ' betr bgc type :'
     write(stdout, *)
     write(stdout, *) ' betr_parameters namelist settings :'
     write(stdout, *)
     write(stdout, betr_parameters)
     write(stdout, *)
     write(stdout, *) '--------------------'
  endif
  this%reaction_method = reaction_method
  this%advection_on    = advection_on
  this%diffusion_on    = diffusion_on
  this%reaction_on     = reaction_on
  this%ebullition_on   = ebullition_on
  end subroutine Read_NML
end module BeTRNmlType

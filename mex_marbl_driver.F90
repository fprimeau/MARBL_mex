#include "fintrf.h"

! This is a hacked version of M Levy's standalon MARBL.
!
! Its been updated for use with Matlab R2019b
!
! And I needed to add/debug some of interface to MARBL init and logging...
!
! Joseph J Becker PhD 27 MAR 2020
!
! Some notes to myself about MEX functions and changes due to 64 bit Matlab and 
! their changing over to non-interleved complex variables
!
! Argument checking is done at compile time in C. In MATLAB, you can pass any 
! number or type of arguments to your M-function, which is responsible for argument 
! checking. This is also true for MEX-files. Your program must safely handle any number 
! of input or output arguments of any supported type.
! 
! 
! FIXME: need way more argument checking. e.g.
!  /* Check for proper number of arguments. */
!   if (nrhs != 1) 
!     mexErrMsgTxt("One input required.");
!   else if (nlhs > 1) 
!     mexErrMsgTxt("Too many output arguments.");
! 
!   /* Input must be string. */
!   if (mxIsChar(prhs[0]) != 1)
!     mexErrMsgTxt("Input must be string.");
! 
!   /* Input must be row vector. */
!   if (mxGetM(prhs[0]) != 1)
!     mexErrMsgTxt("Input must be row vector.");
!   if (nrhs .eq. 0) then
!     call mexPrintf('Need to include phase argument!\n')
!     return
!   end if
!
! However above is mess if "commands" here have different number of args or results
! so (limited amount of) checking is down after we know what cmd is
! 
! IMPORTANT !!!
!
! It is extremely important to use EXACTLY correct "mex" variable types or code will
! crash because of FORTREAN pass by reference (aka pointer). This conflicts because e.g.
! literal "1" gets turned into ptr to 32 bit (integer*4) constant, when ptr to 64
! bit (integer*8) constant is expected. Thus code grabs 64 bits of garbage not 32 bit
! int, and so on.
!
! Be super careful fiddling with any variable declarations because even if it is obvious
! that variable is ptr, F90 ptr like "delta_z" is -NOT- same as mwPointer!
 
! Need to use MATLAB command "doc" to see exactly what each and every call to MEX needs
! for arg types.
!
! e.g.
! 
! doc mexCallMATLAB
!
! Shows that mexCallMATLAB expects --ARRAY-- of ptrs, etc. Read that prototype carefully.
!
! FIXME R2018b madness of switching to "interleaved" complex numbers, 
! means mxGetPr should NOT work, but it does. Conversely, mxGetDoubles SHOULD work,
! but it does NOT, wont even compile...

! FIXME: clean up many subroutine to not use globals, calls by reference, etc...

subroutine mexFunction(nlhs, plhs, nrhs, prhs)

  use marbl_interface_wrapper_mod
  use marbl_kinds_mod, only : r8
  use marbl_logging,                    only : marbl_log_type

  implicit none

! FIXME: Max size of -ALL- diags, tracers, etc, reported to MATLAB. e.g. 386*nlev ~256K
integer*4, parameter          :: MAX_TMP_CNT = 256 *1024

  mwPointer plhs(*), prhs(*)    ! 32 vs. 64 pointers, etc
  integer*4 :: nlhs, nrhs       ! can NOT blindly use "mw stuff".
  
  ! mexFunction arguments:
  !   note everything is Pointers to input/output mxArrays:  
  ! call back to matlab using MEX Function also need these declarations:

! =============================================================================

  ! Declare MEX prototypes
  
!!! FIXME: why is it necessary to declare types of --MEX package-- here?
!!! FIXME: should this not be in fintrf.h ???
  mwPointer :: mxCreateDoubleMatrix
  mwPointer :: mxCreateString
  integer*4 :: log_line_cnt
  integer*4 :: mxGetString
  mwPointer :: mxGetPr
! !  mwPointer :: mxGetDoubles    ! FIXME not found in R2019b ?!?
!   mwPointer :: mxGetM           ! run doc mxGetM and note difference between C and FORTRAN
!   mwPointer :: mxGetN
  integer*4 :: mexCallMATLAB
! 
  ! Matlab 32 vs 64 bit integer conflict. 
  ! Must use 32/64 invariant types. 
  !   e.g. Use "mwSize" not "integer" calling MEX functions
  
  mwSize :: numRows, numCols, numEl
  mwPointer :: string_return(1)

    ! Beware literal constants: They don't have definite size, correct size crucial for MEX

  mwSize                    :: literalOne, literalZero
  integer*4                 :: ComplexFlag

! =============================================================================

  ! temps for passing cmd or "phase" from MATLAB into calls to MARBL routines
  
  ! need to convert log into one very big string to pass it back to MARTLAB

  mwSize,    parameter              :: MAXBUF = 80                   ! FIXME: WHY 80 ???
  character(len=char_len)           :: put_call, diag_name
  character(len=MAXBUF)             :: marbl_phase
  character(len=300)                :: mexInputStr                  ! FIXME: crashes if > about 300
   
  ! at last, some local F90 data e.g. number of layers in ocean, thickness, etc
  
  integer*4 :: nlev 
  integer*4 :: tracer_cnt
  real*8, allocatable, dimension(:) :: delta_z
  real*8, allocatable, dimension(:) :: zw
  real*8, allocatable, dimension(:) :: zt
  integer*4                         :: kmt 
  real*8                            :: x_r8
  type(marbl_diagnostics_type)      :: diag
  
! =============================================================================

  
  ! usual cruft to get return status and other F90 related stuff
  
  integer*4                 :: status
  integer*4                 :: init_result
  integer*4                 :: ioerr
  real*8                        :: tmp_array(1:MAX_TMP_CNT)
  
    ! Locals, tmps, and debug stuff...
  
  integer*4                 :: i, j, k, n  
  character(len=char_len)   :: msg
  
  literalOne = 1; literalZero = 0; ComplexFlag = 0
  
! =============================================================================

  ! Read command and displatch it...
  
  status = mxGetString(prhs(1), marbl_phase, MAXBUF)
  
  if (status .ne. 0) then
    write (msg, *) "ERROR: mxGetString() status: ", status, ", expected at least 1 string aka 'marbl_phase' or command\n"
    call mexErrMsgIdAndTxt("mex_marbl_driver:CheckInputs", msg)
  end if
  
  marbl_phase = trim(marbl_phase)
! write(msg,*) 'MARBL command: ', marbl_phase, '\n'; call mexPrintf(msg)

! =============================================================================

  select case (marbl_phase)

    case ('put setting')
      ! Did user provide proper inputs?
      if (nrhs .ne. 2) &
        call mexErrMsgIdAndTxt("mex_marbl_driver:put_setting:CheckInputs", &
          "Only single name value pair for Matlab call of MARBL 'put_setting'")

      ! read single string which is both setting and value

      status = mxGetString(prhs(2), put_call, MAXBUF)
      if (status .ne. 0) then
        write (msg, *) "ERROR: mxGetString() status: ", status, " expected string with var_name = value\n"
        call mexErrMsgIdAndTxt("mex_marbl_driver:put_setting:CheckInputs", &
          msg)
      end if
      write(msg, "(3A)") '  Setting: ', trim(put_call), '\n'
      call mexPrintf(msg)

      status = put_setting(put_call)
      if (status .ne. 0) &
        call mexPrintf('Error calling put_setting()')

! =============================================================================

    case ('init')
      call init ()
      

    case ('set_depth')      ! kmt is level in domain with ocean bottom
    
      numRows = 1
      call mxCopyPtrToReal8(mxGetPr(prhs(2)), x_r8, numRows) ! MUST read into a real
      kmt = x_r8
      
      marbl_instance%domain%kmt = kmt
!       write (msg, *) 'MARBL DEBUG: kmt (level # of bottom):', marbl_instance%domain%kmt, '\n'; call mexPrintf(msg)


    case ('surface_flux_compute')
    
      call marbl_instance%surface_flux_compute()
      
      call outputLog ()
      
      if (marbl_instance%StatusLog%labort_marbl) then
!         call mexErrMsgIdAndTxt("mex_marbl_driver:surface_flux_compute", "MARBL ERROR: surface_flux_compute() failed")
        call mexWarnMsgTxt("DRIVER ERROR: marbl_instance%surface_flux_compute() failed")
      end if


    case ('interior_tendency_compute')
    
      call marbl_instance%interior_tendency_compute()
      
      call outputLog ()
      
      if (marbl_instance%StatusLog%labort_marbl) then
!         call mexErrMsgIdAndTxt("mex_marbl_driver:interior_tendency_compute", "MARBL ERROR: interior_tendency_compute() failed")
        call mexWarnMsgTxt("marbl_instance%interior_tendency_compute() failed")
      end if



! Interior tracers and such...

    case ('tracers')
      call outputTracer ('tracers', marbl_instance%tracers, surfaceNotInterior = .false.)

    case ('interior_tendencies')
      call outputTracer ('interior_tendencies', marbl_instance%interior_tendencies, surfaceNotInterior = .false.)

    case ('interior_tendency_saved_state')
      call outputSaved_state ('interior_tendency_saved_state', marbl_instance%interior_tendency_saved_state)
      
    case ('interior_tendency_diags')
      call output_diag ('interior_tendency_diags', marbl_instance%interior_tendency_diags, surfaceNotInterior = .false.)


    case ('restore_interior_tendency_saved_state')
      call restore_saved_state('interior_tendency_saved_state', marbl_instance%interior_tendency_saved_state)

    case ('restore_tracers')
      call restoreTracer('tracers', marbl_instance%tracers, surfaceNotInterior = .false.)

    case ('restore_interior_tendency_forcings')
      call restore_interior_tendency_forcings(size(marbl_instance%tracers,2))

!     case ('restore_interior_tendencies')
!       call restoreTracer('interior_tendencies', marbl_instance%interior_tendencies, surfaceNotInterior = .false.)


! Surface tracers and such...

    case ('tracers_at_surface')
      call outputTracer ('tracers_at_surface', marbl_instance%tracers_at_surface, surfaceNotInterior = .true.)

    case ('surface_fluxes')
      call outputTracer ('surface_fluxes', marbl_instance%surface_fluxes, surfaceNotInterior = .true.)

    case ('sfo')
      call outputSfo ()

    case ('surface_flux_saved_state')
      call outputSaved_state ('surface_flux_saved_state', marbl_instance%surface_flux_saved_state)
      
    case ('surface_flux_diags')
      call output_diag ('surface_flux_diags', marbl_instance%surface_flux_diags, surfaceNotInterior = .true.)


    case ('restore_surface_flux_saved_state')
      call restore_saved_state('surface_flux_saved_state', marbl_instance%surface_flux_saved_state)
      
    case ('restore_tracers_at_surface')
      call restoreTracer('tracers_at_surface', marbl_instance%tracers_at_surface, surfaceNotInterior = .true.)

    case ('restore_surface_flux_forcings')
      call restore_surface_flux_forcings()
        
!     case ('restore_surface_fluxes')
!       call restoreTracer ('surface_fluxes', marbl_instance%surface_fluxes, surfaceNotInterior = .true.)



    case ('print_marbl')
      call jj_print_marbl()
      
    case ('print_sfo')
      call jj_print_sfo ()
      
    case ('print_surface_diags')
      call jj_print_diag('surface_flux_diags', marbl_instance%surface_flux_diags)
      
    case ('print_interior_tendency_diags')
      call jj_print_diag('interior_tendency_diags', marbl_instance%interior_tendency_diags)



    case ('print_interior_tendencies')
      call jj_print_tracer('interior_tendencies', marbl_instance%interior_tendencies, surfaceNotInterior = .false.)    

    case ('print_surface_fluxes')
      call jj_print_tracer('surface_fluxes', marbl_instance%surface_fluxes, surfaceNotInterior = .true.)
      
    case ('print_surface_flux_forcings')
      call jj_print_forcing('surface_flux_forcings', marbl_instance%surface_flux_forcings)
      
    case ('print_interior_tendency_forcings')
      call jj_print_forcing('interior_tendency_forcings', marbl_instance%interior_tendency_forcings)

    case ('print_surface_flux_saved_state')
      call jj_print_saved_state('surface_flux_saved_state', marbl_instance%surface_flux_saved_state)
      
    case ('print_interior_tendency_saved_state')
      call jj_print_saved_state('interior_tendency_saved_state', marbl_instance%interior_tendency_saved_state)
      



    case('tracer_index')
! FIXME: crashes if called before "init" or after "shutdown"
      status = mxGetString(prhs(2), mexInputStr, MAXBUF)
      if (status .ne. 0) then
        write (msg, *) "DRIVER ERROR: mxGetString() status: ", status, ", expected 1 string tracer short name\n"
        call mexErrMsgIdAndTxt("mex_marbl_driver:tracer_index:CheckInputs", &
          msg)
      end if
      mexInputStr = trim(mexInputStr)
      x_r8 = marbl_instance%get_tracer_index(mexInputStr)
      call outputReal ( x_r8 )
    
    
    case ('tracer_sname')
! FIXME: crashes if called before "init" or after "shutdown"
      numRows = 1
      call mxCopyPtrToReal8(mxGetPr(prhs(2)), x_r8, numRows) ! MUST read into a real
      n = x_r8
! FIXME: should check if index is valid...
      msg = trim(marbl_instance%tracer_metadata(n)%short_name)
      write (msg,*) trim(marbl_instance%tracer_metadata(n)%short_name), " (",trim(marbl_instance%tracer_metadata(n)%units), ")";
!       call mexPrintf(msg)
! FIXME: why oh why does F90 not do right thing with this??? Return number of diags...
!  k = len(msg)
      k   = len(trim(marbl_instance%tracer_metadata(n)%short_name))
      k = k+2+len(trim(marbl_instance%tracer_metadata(n)%units))+1+1 ! extra +1 for the lenght of F90 str
      ! Now usual gyrations to return a string to Matlab
      string_return(1) = mxCreateString(msg)
      call mxSetN( string_return(literalOne), literalOne* 1)
      call mxSetM( string_return(literalOne), literalOne* k)
      n = mexCallMATLAB(literalOne, plhs, literalOne, string_return, 'transpose')


    case ('diag_sname')
      ! this -does- work after "shutdown" but not before "init"
      ! Get name of diags of interest, as a string
      status = mxGetString(prhs(2), diag_name, MAXBUF)
      if (status .ne. 0) then
        write (msg, *) "ERROR: mxGetString() status: ", status, " F90 MARBL code diag name\n"
        call mexErrMsgIdAndTxt("mex_marbl_driver:diag_sname:CheckInputs", msg)
      end if
      ! Use name of diag to point to actual diag in instance
      if (diag_name .EQ. 'interior_tendency_diags')   diag = marbl_instance%interior_tendency_diags
      if (diag_name .EQ. 'surface_flux_diags')        diag = marbl_instance%surface_flux_diags
      ! read index of particular diag
      numRows = 1
      call mxCopyPtrToReal8(mxGetPr(prhs(3)), x_r8, numRows) ! MUST read into a real
      n = x_r8
      !  write(msg, "(3A,I0,A)") '  returning short name of ', trim(diag_name), '(',n,')\n'; call mexPrintf(msg)
! FIXME: should check if index is valid...
!       msg = trim(diag%diags (n)%short_name)
      write (msg,*) trim(diag%diags (n)%short_name), " (",trim(diag%diags(n)%units), ")";
! FIXME: why oh why does F90 not do right thing with this??? Return number of diags...
!  k = len(msg)
      k   = len(trim(diag%diags (n)%short_name))
      k = k+2+len(trim(diag%diags(n)%units))+1+1 ! extra +1 for the lenght of F90 str
      !  call mexPrintf(msg//'\n')
      ! Now usual gyrations to return a string to Matlab
      string_return(1) = mxCreateString(msg)
      call mxSetN( string_return(literalOne), literalOne* 1)
      call mxSetM( string_return(literalOne), literalOne* k)
      n = mexCallMATLAB(literalOne, plhs, literalOne, string_return, 'transpose')


    case ('read_settings_file')
      status = mxGetString(prhs(2), mexInputStr, MAXBUF)
      if (status .ne. 0) then
        write (msg, *) "DRIVER ERROR:read_settings_file:mxGetString() status: ", status, &
          ", expected 1 string: settings file name\n"
        call mexErrMsgIdAndTxt("mex_marbl_driver:read_settings_file:CheckInputs", msg)
      end if
      mexInputStr = trim(mexInputStr)
      call read_settings_file (mexInputStr, ioerr, marbl_instance)
      ! return ioerr
      tmp_array(1) = ioerr; numRows=1; numCols=1
      plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
      call mxCopyReal8ToPtr(tmp_array, mxGetPr(plhs(1)), numRows * numCols)

    case ('write_settings_file')
      status = mxGetString(prhs(2), mexInputStr, MAXBUF)
      if (status .ne. 0) then
        write (msg, *) "DRIVER ERROR:write_settings_file:mxGetString() status: ", status, &
          ", expected 1 string: settings file name\n"
        call mexErrMsgIdAndTxt("mex_marbl_driver:write_settings_file:CheckInputs", msg)
      end if
      mexInputStr = trim(mexInputStr)
      call write_settings_file (mexInputStr, ioerr)
      ! return ioerr
      tmp_array(1) = ioerr; numRows=1; numCols=1
      plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
      call mxCopyReal8ToPtr(tmp_array, mxGetPr(plhs(1)), numRows * numCols)



    case ('shutdown')
      ! MARBL does NOT deallocate SFO that have been allocated
      if (size(marbl_instance%surface_flux_output%sfo) .GT. 1) then
        deallocate(marbl_instance%surface_flux_output%sfo)
      end if
      status = shutdown_marbl();
      write (msg, *) "status", status, '\n'
      if (status .ne. 0) then
        call mexErrMsgIdAndTxt("mex_marbl_driver:shutdown", "error shutting down MARBL")
      end if

    case ('log')
!     FIXME: doesnt work when called from Matlab, seems log gets rest after every command or something....
        call outputLog ()

    case DEFAULT
      write(msg, *) 'mex_marbl_driver: unknown marble_phase (aka command): ', marbl_phase
      call mexWarnMsgTxt(msg)
  end select


! =============================================================================

  return

! =============================================================================

contains

! =============================================================================

subroutine init ( )

  mwPointer :: mxCreateDoubleMatrix
  mwPointer :: mxGetN

  ! Did user provide proper inputs?
  
  if (nrhs .ne. 4) &
    call mexErrMsgIdAndTxt("mex_marbl_driver:init:CheckInputs", &
      "cmd 'init' requires 3 arguments: delta_z, zw, zt")

! FIXME: original code got array as MATLAB style "Column Major" array of "M" rows but in 
! starting in R2018a args (FIXME?) arrays are passed as 1xN matrix or Row Major like C. 
! Thus we need "mxGetN"
!     nlev = mxGetM(prhs(2))    ! M is 1 for vector
  nlev = mxGetN(prhs(2))    ! N is length of vector

! FIXME: using variable of mwSize type is important to mxCopyPtrToReal8,
! FIXME: but MARBL code is expecting pointer to integer*4, not long (8 byte). 
! Need both :-(

  numEl = nlev

  allocate(delta_z(numEl), zt(numEl), zw(numEl))

  call mxCopyPtrToReal8(mxGetPr(prhs(2)), delta_z,  numEl) 
  call mxCopyPtrToReal8(mxGetPr(prhs(3)), zw,   numEl) 
  call mxCopyPtrToReal8(mxGetPr(prhs(4)), zt,   numEl) 

  init_result = init_marbl(delta_z, zw, zt, nlev, tracer_cnt)
  
  deallocate(delta_z, zw, zt)

  if (init_result .NE. 0) then 
  
    call mexPrintf('DRIVER ERROR: : initialization failed!\n')
    write (msg, *) "init_result", init_result, "\n"; call mexPrintf(msg)
    
  else
  
    ! default set bottom at max depth
    marbl_instance%domain%kmt = size(zw)

    ! initialize everything to zero

    call jj_init_surface_flux_saved_state (8.0)
    call jj_init_saved_interior_tendency_saved_state (8.8)
    
    call jj_init_tracers_at_surface()   ! must be called before jj_init_tracers
    call jj_init_tracers()              ! must be called after jj_init_tracers_at_surface
    
    call jj_init_surface_flux_forcings()
    call jj_init_interior_tendency_forcings()
    
    call jj_init_sfo
    

! FIXME: much insanity with what MEX can do with literals, ptr to 64 vs ptr to 32...
! FIXME: mxGetDoubles() is NOT in MEX...  version -itis about 32 bit?
! FIXME: do NOT, do NOT, use literals in calls to mex functions...

    numRows = 1
    numCols = 1

    ! Return tracer count
    plhs(2) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
    call mxCopyReal8ToPtr(DBLE(tracer_cnt), mxGetPr(plhs(2)), numCols)

    ! Return interior_tendency_forcings size
    plhs(3) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
    call mxCopyReal8ToPtr(DBLE(size(marbl_instance%interior_tendency_forcings,1)), mxGetPr(plhs(3)), numCols)

    ! Return interior_tendency_diags size
    plhs(4) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
    call mxCopyReal8ToPtr(DBLE(size(marbl_instance%interior_tendency_diags%diags)), mxGetPr(plhs(4)), numCols)

    ! Return surface_flux_forcings size
    plhs(5) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
    call mxCopyReal8ToPtr(DBLE(size(marbl_instance%surface_flux_forcings)), mxGetPr(plhs(5)), numCols)

    ! Return interior_tendency_diags size
    plhs(6) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
    call mxCopyReal8ToPtr(DBLE(size(marbl_instance%surface_flux_diags%diags)), mxGetPr(plhs(6)), numCols)

  end if
  
    call outputLog ( )

end subroutine init

! =============================================================================
subroutine outputReal ( x )

  mwPointer                     :: mxCreateDoubleMatrix

  real(kind=r8),  intent(in)    :: x

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  integer*4                     :: ComplexFlag
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  ComplexFlag = 0
!     call jj_print_tracer(var_name, myTracer, surfaceNotInterior)

  numRows = 1
  numCols = 1

  plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
  call mxCopyReal8ToPtr(x, mxGetPr(plhs(1)), numRows * numCols)

end subroutine outputReal

! =============================================================================

subroutine outputRealMatrix ( x )

  mwPointer :: mxCreateDoubleMatrix

  real(kind=r8),  intent(in)    :: x(:,:)

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  integer*4                     :: ComplexFlag
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  ComplexFlag = 0
!     call jj_print_tracer(var_name, myTracer, surfaceNotInterior)

  numRows = size(x, 1)
  numCols = size(x, 2)

  plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
  call mxCopyReal8ToPtr(x, mxGetPr(plhs(1)), numRows * numCols)

end subroutine outputRealMatrix

! =============================================================================

subroutine outputSfo ()

  mwPointer :: mxCreateDoubleMatrix

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  integer*4                     :: ComplexFlag
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  ComplexFlag = 0
!     call jj_print_sfo

  numRows = 1
  numCols = size(marbl_instance%surface_flux_output%sfo)

  k = 0
  do j=1,numCols  ! FORTRAN is column major, Matlab and C are row major
    k = k+1
    tmp_array(k) = marbl_instance%surface_flux_output%sfo(j)%forcing_field(1)
  end do
  
  plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
  call mxCopyReal8ToPtr(tmp_array, mxGetPr(plhs(1)), numRows * numCols)

end subroutine outputSfo

! =============================================================================

subroutine outputTracer ( var_name, myTracer, surfaceNotInterior )

  mwPointer                     :: mxCreateDoubleMatrix

  character (*),  intent(in)    :: var_name
  real(kind=r8),  intent(in)    :: myTracer(:,:)
  logical,        intent(in)    :: surfaceNotInterior

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  integer*4                     :: ComplexFlag
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  ComplexFlag = 0
  numRows = size(myTracer, 1)
  numCols = size(myTracer, 2)

  k = 0
    do j=1,numCols  ! FORTRAN is column major, Matlab and C are row major
  do i=1,numRows
    k = k+1
      tmp_array(k) = myTracer(i,j)
    end do
  end do

  plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
  call mxCopyReal8ToPtr(tmp_array, mxGetPr(plhs(1)), numRows * numCols)

end subroutine outputTracer

! =============================================================================

subroutine output_diag ( var_name, diag, surfaceNotInterior )

  character (*),                intent(in) :: var_name
  type(marbl_diagnostics_type), intent(in) :: diag
  logical,                      intent(in) :: surfaceNotInterior

  mwSize                        :: numRows
  mwSize                        :: numCols
  integer*4                     :: ComplexFlag
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  integer*4                     :: i, j, k, n
  character(len=char_len*10)    :: msg

  associate( diags => diag%diags )
  
  ComplexFlag = 0
  numRows = size(diags)
  if (surfaceNotInterior) then
    numCols = 1
  else
    numCols = diag%num_levels
  end if
!   write (msg, *) var_name, ' numRows: ', numRows, "\n";  call mexPrintf(msg)
!   write (msg, *) var_name, ' numCols: ', numCols, "\n";  call mexPrintf(msg)

   do n=1,size(diags)

!     write(msg, *) var_name, n,                     &
!       '\tname: ',  trim(diags(n)%short_name),      &
!       '\tunits: ', trim(diags(n)%units),           &
!       '\t: ',      trim(diags(n)%long_name), "\n"; 
!       call mexPrintf(msg)
! write (msg,*) shape(diags(n)%field_2d), "\n"; call mexPrintf(msg)

      k = n
      do j=1,numCols
      
        if (allocated(diags(n)%field_2d)) tmp_array(k) = diags(n)%field_2d(1)
        if (allocated(diags(n)%field_3d)) tmp_array(k) = diags(n)%field_3d(j,1);
! write (msg,*) n, j, k, tmp_array(k), "\n"; call mexPrintf(msg)
        k = k +numRows

! FIXME: tempting to do something like next line...
!         if (allocated(diags(n)%field_2d)) tmp_array(k:k+numCols-1) = diags(n)%field_2d(1)
!         if (allocated(diags(n)%field_3d)) tmp_array(k:k+numCols-1) = diags(n)%field_3d(1:numCols,1);
! .. except we are writing transpose(diags), k+=numRows, which makes my brain explode...
        
      end do ! j
  end do ! n
  end associate

  plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
  call mxCopyReal8ToPtr(tmp_array, mxGetPr(plhs(1)), numRows * numCols)

 end subroutine output_diag

 ! =============================================================================

  subroutine outputSaved_state ( var_name, saved_state )
  
  character (*),  intent(in)    :: var_name
  type(marbl_saved_state_type)  :: saved_state
  mwPointer                     :: mxCreateDoubleMatrix

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  integer*4                     :: ComplexFlag
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  mwSize                        :: numState
  integer*4                     :: rank
  
  ComplexFlag = 0
  associate( state => saved_state%state )

  numState = saved_state%saved_state_cnt
! FIXME: hack: all states are same dimesnion
  rank = state(1)%rank
  numRows = numState
  if ( rank .EQ. 2) then
    numcols = 1
  else
    numCols = size ( state(1)%field_3d, 1)
  end if

! write(msg, *) 'numRows', numRows, "\n"; call mexPrintf(msg)
! write(msg, *) 'numCols', numCols, "\n"; call mexPrintf(msg)
    
  k = 0

    if ( rank .EQ. 2 ) then
    
  do n=1, numState
    k = k+1
    tmp_array(k) = state(n)%field_2d (1)
  end do ! n
    
    else
    
    do j=1,numCols    ! FORTRAN is column major, Matlab and C are row major
  do n=1, numState
      k = k+1
! write(msg, *) 'k', k, "\n"; call mexPrintf(msg)
      tmp_array(k) = state(n)%field_3d( j, 1 )
  end do ! n
    
    end do ! j
    
    end if
    

  end associate
   
  plhs(1) = mxCreateDoubleMatrix(numRows, numCols, ComplexFlag)
  call mxCopyReal8ToPtr(tmp_array, mxGetPr(plhs(1)), numRows * numCols)

 end subroutine outputSaved_state

! =============================================================================

subroutine outputLog ( )

  mwPointer :: mxCreateString
  integer*4 :: mexCallMATLAB

  mwPointer :: string_return(1)
  integer*4 :: log_line_cnt

  ! 1) create MATLAB string, intialized to string version of log.
  
  call convertMarblLogToSingleString(log_line_cnt, log_as_single_string)
  string_return(1) = mxCreateString(log_as_single_string)
  
  ! 2) tell MATLAB what shape is
  ! Need to calculate transpose to get log to look as expected in MATLAB
  !  sadly shape is transposed. 
  !    Call MATLAB transpose. plhs(1) is ptr to result
  
  call mxSetN(string_return(literalOne), literalOne* log_line_cnt)
  call mxSetM(string_return(literalOne), literalOne* char_len)

  !  mexCallMATLAB args use -ARRAY- of PTR to reference data, not ptr
  !    i.e. plhs, not plhs(1)

  n = mexCallMATLAB(literalOne, plhs, literalOne, string_return, 'transpose')

 end subroutine outputLog

! =============================================================================

subroutine restoreTracer ( var_name, myTracer, surfaceNotInterior )

  character (*),  intent(in)    :: var_name
  real(kind=r8),  intent(inout) ::myTracer(:,:)
  logical,        intent(in)    :: surfaceNotInterior

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  mwPointer                     :: x_ptr

  numRows = size(myTracer, 1)
  numCols = size(myTracer, 2)
  x_ptr = mxGetPr(prhs(2))  
  call mxCopyPtrToReal8(x_ptr, tmp_array, numRows * numCols) 

  k = 0
    do j=1,numCols    ! FORTRAN is column major, Matlab and C are row major
  do i=1,numRows
    k = k+1
    myTracer(i,j) = tmp_array(k)
    end do
  end do

end subroutine restoreTracer

 ! =============================================================================

subroutine restore_saved_state ( var_name, saved_state )
  
  character (*),  intent(in)    :: var_name
  type(marbl_saved_state_type)  :: saved_state

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  mwSize                        :: numState
  integer*4                     :: rank
  mwPointer                     :: x_ptr

  associate( state => saved_state%state )

  numState = saved_state%saved_state_cnt
! FIXME: hack: all states in struct are same dimesnion
  rank = state(1)%rank
  numRows = numState
  if ( rank .EQ. 2) then
    numcols = 1
  else
    numCols = size ( state(1)%field_3d, 1)
  end if

! write(msg, *) 'numRows', numRows, "\n"; call mexPrintf(msg)
! write(msg, *) 'numCols', numCols, "\n"; call mexPrintf(msg)

  x_ptr = mxGetPr(prhs(2))  
  call mxCopyPtrToReal8(x_ptr, tmp_array, numRows * numCols) 
    
  k = 0
    if ( rank .EQ. 2 ) then
  do n=1, numState
      k = k+1
! write(msg, *) 'k', k, "tmp_array(k)", tmp_array(k), "\n"; call mexPrintf(msg)
      state(n)%field_2d (1) = tmp_array(k)
  end do ! n

    else
      do j=1,numCols    ! FORTRAN is column major, Matlab and C are row major
  do n=1, numState
        k = k+1
! write(msg, *) 'k', k, "tmp_array(k)", tmp_array(k), "j", j, "\n"; call mexPrintf(msg)
        state(n)%field_3d( j, 1 ) = tmp_array(k)
  end do ! n
      end do ! j
    end if

  end associate
   
  end subroutine restore_saved_state


 ! =============================================================================

  subroutine restore_surface_flux_forcings ()

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  mwPointer                     :: x_ptr

  associate( marbl_state => marbl_instance%surface_flux_forcings )

  numRows = 1
  numCols = size(marbl_state)
  x_ptr = mxGetPr(prhs(2))  
  call mxCopyPtrToReal8(x_ptr, tmp_array, numRows * numCols) 

  k = 0
    do j=1,numCols
  do i=1,numRows
    k = k+1
    marbl_state(j)%field_0d = tmp_array(k)
    end do
  end do

  end associate
!   call jj_print_forcing('surface_flux_forcings', marbl_instance%surface_flux_forcings)

  end subroutine restore_surface_flux_forcings

! =============================================================================

  subroutine restore_interior_tendency_forcings ( nlev )

  integer,          intent(in)  :: nlev

  integer*4                     :: i, j, k
  mwSize                        :: numRows
  mwSize                        :: numCols
  real*8                        :: tmp_array(1:MAX_TMP_CNT)

  mwPointer                     :: x_ptr

  associate( forcing => marbl_instance%interior_tendency_forcings )

! FIXME: hack to get around F90 structure and just send a matrix
tracer_cnt = size(forcing)
numRows = tracer_cnt
numCols = nlev

  x_ptr = mxGetPr(prhs(2))  
  call mxCopyPtrToReal8(x_ptr, tmp_array, numRows * numCols) 

  do n=1,tracer_cnt
  
! FIXME: hack to get around F90 structure and just send a matrix
    if (forcing(n)%metadata%rank.EQ.0) then
    
      k = 1+(n-1)*nlev
! FIXME: Matlab sends transpose of what FORTRAN expects
      k = n+(1-1)*tracer_cnt
! write(msg,*) n, numRows, numCols, k, "\n"; call mexPrintf(msg)
      forcing(n)%field_0d(:) = tmp_array(k)

    else
    
! FIXME: hack to get around F90 structure and just send a matrix
! FIXME: assumes rank 1
      numRows = size(forcing(n)%field_1d, 1)
      numCols = size(forcing(n)%field_1d, 2)
      do i=1,numRows
        do j=1,numCols
          k = j+(n-1)*nlev
! FIXME: Matlab sends transpose of what FORTRAN expects
          k = n+(j-1)*tracer_cnt
! write(msg,*) n, numRows, numCols, k, "\n"; call mexPrintf(msg)
          forcing(n)%field_1d(i,j) = tmp_array(k)
      
        end do
      end do
        
      end if
      
    end do

    end associate
!     call jj_print_forcing('interior_tendency_forcings', marbl_instance%interior_tendency_forcings)

  end subroutine restore_interior_tendency_forcings

end subroutine mexFunction


module marbl_interface_wrapper_mod

  use iso_c_binding
  
  use marbl_kinds_mod, only                 : r8, char_len
  use marbl_interface, only                 : marbl_interface_class
  use marbl_interface_public_types, only    : marbl_diagnostics_type
  use marbl_interface_public_types, only    : marbl_saved_state_type
  use marbl_interface_public_types, only    : marbl_forcing_fields_type
  use marbl_logging,   only                 : marbl_log_type

  implicit none
  
  public
  save

  type(marbl_interface_class)   :: marbl_instance

! FIXME: large values of MAX_LOG_ENTRY_CNT makes everything extremely slow. WHY?
  integer*4, parameter          :: MAX_LOG_ENTRY_CNT = 10 *1024
  integer*4, parameter          :: MAX_STR_LEN = MAX_LOG_ENTRY_CNT*char_len
  character(len=MAX_STR_LEN)    :: log_as_single_string

  interface put_setting
    module procedure            :: put_setting_without_line_len
    module procedure            :: put_setting_with_line_len
  end interface put_setting

  private :: get_return_code
  private :: put_setting_without_line_len
  private :: put_setting_with_line_len

contains

! =============================================================================

  function init_marbl(delta_z, zw, zt, nlev, nt)

    use marbl_kinds_mod, only : r8
    use marbl_constants_mod, only : p5
    use marbl_constants_mod, only : c1

    integer,                        intent(in)  :: nlev
    real(kind=r8), dimension(nlev), intent(in)  :: delta_z, zw, zt
    integer,                        intent(out) :: nt
    integer :: init_marbl

    call marbl_instance%init(gcm_num_levels = size(delta_z),       &
                             gcm_num_PAR_subcols = 1,              &
                             gcm_num_elements_surface_flux = 1,    &
                             gcm_delta_z = delta_z,                &
                             gcm_zw = zw,                          &
                             gcm_zt = zt,                          &
                             lgcm_has_global_ops = .false.)
    nt = size(marbl_instance%tracer_metadata)


    init_marbl = get_return_code()

  end function init_marbl

! =============================================================================

  function shutdown_marbl()

    integer :: shutdown_marbl

    call marbl_instance%shutdown()
    shutdown_marbl = get_return_code()

  end function shutdown_marbl

! =============================================================================

  ! Use this subroutine to return the log to a fortran driver when you do not
  ! have access to stdout
  subroutine get_marbl_log(log_ptr, msg_cnt)

    use marbl_logging, only : marbl_status_log_entry_type
    use iso_c_binding, only : c_ptr, c_loc

    type(c_ptr), intent(out) :: log_ptr
    integer,     intent(out) :: msg_cnt

    type(marbl_status_log_entry_type), pointer :: msg_ptr
    character(len=char_len),           pointer :: log_array(:)

    ! Determine number of messages
    msg_cnt = 0
    msg_ptr => marbl_instance%StatusLog%FullLog
    do while (associated(msg_ptr))
      msg_cnt = msg_cnt + 1
      msg_ptr => msg_ptr%next
    end do

    ! memory for messages to return
    allocate(log_array(msg_cnt))
    log_array(:) = ''

    ! Copy messages to log_array
    msg_cnt = 0
    msg_ptr => marbl_instance%StatusLog%FullLog
    do while (associated(msg_ptr))
      msg_cnt = msg_cnt + 1
      log_array(msg_cnt) = trim(msg_ptr%LogMessage)
      msg_ptr => msg_ptr%next
    end do

    call marbl_instance%StatusLog%erase()
    marbl_instance%StatusLog%labort_marbl = .false.

    log_ptr = c_loc(log_array(1))
!     deallocate(log_array(msg_cnt))

  end subroutine get_marbl_log

! =============================================================================

  ! Use this subroutine to return the log to a fortran driver when you do not
  ! have access to stdout
  subroutine get_timer_summary(timer_array, timer_cnt)

    character(len=*), pointer, intent(out) :: timer_array(:)
    integer,                   intent(out) :: timer_cnt

    integer :: n

    timer_cnt = marbl_instance%timer_summary%num_timers
    allocate(timer_array(timer_cnt+5))

    ! Header block of text
    write(timer_array(1),"(A)") ''
    write(timer_array(2),"(A)") '-------------'
    write(timer_array(3),"(A)") 'Timer Summary'
    write(timer_array(4),"(A)") '-------------'
    write(timer_array(5),"(A)") ''

    ! Get timers from instance
    do n = 1, timer_cnt
      write(timer_array(n+5),"(A, ': ', F11.3, ' seconds')") &
            trim(marbl_instance%timer_summary%names(n)),        &
            marbl_instance%timer_summary%cumulative_runtimes(n)
    end do
    ! deallocate(timer_array(timer_cnt+5))

  end subroutine get_timer_summary

! =============================================================================

  function put_setting_without_line_len(line_in) result(put_setting)

    character(len=*), intent(in) :: line_in
    integer :: put_setting

    call marbl_instance%put_setting(line_in)
    put_setting = get_return_code()

  end function put_setting_without_line_len

! =============================================================================

  function put_setting_with_line_len(line_in, line_len) result(put_setting)

    integer,                 intent(in) :: line_len
    character(len=line_len), intent(in) :: line_in
    integer :: put_setting

    call marbl_instance%put_setting(line_in)
    put_setting = get_return_code()

  end function put_setting_with_line_len

! =============================================================================

  function get_return_code()

    integer :: get_return_code

    if (marbl_instance%StatusLog%labort_marbl) then
      get_return_code = -1
    else
      get_return_code = 0
    end if

  end function get_return_code

! =============================================================================

  subroutine jj_print_tracer_metadata(var_name, n)

    character (*),  intent(in) :: var_name
    integer,        intent(in) :: n
    
    character(len=char_len) :: msg
    
    write(msg, *) var_name, n, &
      '\tmodule: ', trim(marbl_instance%tracer_metadata(n)%tracer_module_name),     &
      '\tname: ',   trim(marbl_instance%tracer_metadata(n)%short_name),             &
      '\tunits: ',  trim(marbl_instance%tracer_metadata(n)%units),                  &
      '\t: ',   trim(marbl_instance%tracer_metadata(n)%long_name), "\n"; 
    call mexPrintf(msg)
    
  end subroutine jj_print_tracer_metadata

! =============================================================================

  subroutine jj_print_tracer(var_name, myTracer, surfaceNotInterior)

    use marbl_kinds_mod, only : r8

    character (*),  intent(in) :: var_name
    real(kind=r8),  intent(in) :: myTracer(:,:)
    logical,        intent(in) :: surfaceNotInterior

    integer*4 :: n, k
    character(len=char_len) :: msg

    write (msg, *) "\n ", var_name, ' shape ', shape(myTracer), "\n"; call mexPrintf(msg)
    
    do n = 1, size(marbl_instance%tracer_metadata)
    
      call jj_print_tracer_metadata(var_name, n)
      
      ! tracers_at_surface is organized as transpose of interior tracers

      if (surfaceNotInterior) then
        write (msg, *) myTracer(1,n), '\n'   ;
      else  
! FIXME: this crash if tracer_cnt > 15 or so
!         write (msg, *) myTracer(n,:), '\n'   ;
        do k=1,size(myTracer,2)
          write (msg, *) myTracer(n,k); call mexPrintf(msg)
        end do
        write (msg, *) "\n"; call mexPrintf(msg)
      end if
      
      call mexPrintf(msg)

    end do

  end subroutine jj_print_tracer

! =============================================================================

  subroutine jj_print_saved_state(var_name, saved_state)
  
    character (*),  intent(in)      :: var_name
    type(marbl_saved_state_type)    :: saved_state

    integer*4 :: n, k
    character(len=char_len) :: msg

    associate( state => saved_state%state )

    write (msg, *) "\n ", var_name, ' shape: ', shape(state), "\n"; call mexPrintf(msg)
        
    do n=1, saved_state%saved_state_cnt

      write(msg, *) var_name, n,                 &
        '\tsname: ',  trim(state(n)%short_name),  &
        '\tunits: ', trim(state(n)%units),       &
        '\tlname: ',      trim(state(n)%long_name); 
      call mexPrintf(msg)
      
      write (msg, *) 'rank:' , state(n)%rank, '\n' ; call mexPrintf(msg)
      if (state(n)%rank.EQ.2) then
        write (msg, *) 'shape:' , shape(state(n)%field_2d), '\n' ; call mexPrintf(msg)
        write (msg, *)            state(n)%field_2d(:),     '\n' ; call mexPrintf(msg)
      else
! FIXME: this fails for some reason if nlev > 5 or so
!         write (msg, *)            state(n)%field_3d(:,1),   '\n' ; call mexPrintf(msg)
!         write (msg, *) 'shape:' , shape(state(n)%field_3d), '\n' ; call mexPrintf(msg)
        do k=1,size(state(n)%field_3d)
          write (msg, *) state(n)%field_3d(k,1); call mexPrintf(msg)
        end do
        write (msg, *) "\n"; call mexPrintf(msg)
      end if

    end do
    
    end associate
    
  end subroutine jj_print_saved_state

! =============================================================================

  subroutine jj_print_diag(var_name, diag)
  
    character (*),  intent(in)      :: var_name
    type(marbl_diagnostics_type)    :: diag

    integer*4 :: n, k
    character(len=char_len*10) :: msg

    associate( diags => diag%diags )

    write (msg, *) "\n ", var_name, ' size: ', size(diags), "\n";  call mexPrintf(msg)

    do n=1,size(diags)
      write(msg, *) var_name, n,                     &
        '\tname: ',  trim(diags(n)%short_name),      &
        '\tunits: ', trim(diags(n)%units),           &
        '\t: ',      trim(diags(n)%long_name), "\n"; 
      call mexPrintf(msg)

      if (allocated(diags(n)%field_2d)) then
        write (msg,*) diags(n)%field_2d(:), "\n"; call mexPrintf(msg)
      end if

      if (allocated(diags(n)%field_3d)) then
!         write (msg,*) diags(n)%field_3d(:,:), "\n"; call mexPrintf(msg)
!         write (msg,*) shape(diags(n)%field_3d), "\n"; call mexPrintf(msg)
        do k=1,size(diags(n)%field_3d)
! FIXME: if roughly 1,000 or more levels in a field, msg gets so long that Matlab command window truncates it.
          write (msg, *) diags(n)%field_3d(k,1); call mexPrintf(msg)
        end do
        write (msg, *) "\n"; call mexPrintf(msg)
      end if

    end do
    end associate

 end subroutine jj_print_diag

! =============================================================================

  subroutine jj_print_sfo ()

    integer*4 :: n, j
    character(len=char_len) :: msg

    associate( sfo      => marbl_instance%surface_flux_output )
    associate( forcing  => sfo%sfo )

    write(msg,*) "size(sfo): ", size(marbl_instance%surface_flux_output%sfo), "\n"; call mexPrintf(msg);
    do n=1,size(forcing)
    
      write (msg, *) "surface_flux_output", n,      &
        "\tname: ", trim(forcing(n)%short_name),    &
        "\tname: ", trim(forcing(n)%long_name),     &
        "\tunit: ", trim(forcing(n)%units)
      call mexPrintf(msg)
      write (msg, *) 'size: '  , size( forcing(n)%forcing_field),   "\n"    ; call mexPrintf(msg)
      
      do j=1,size( forcing(n)%forcing_field)
        write (msg, *)                 forcing(n)%forcing_field(j), '\n'    ; call mexPrintf(msg)
      end do
      
    end do

    end associate
    end associate

 end subroutine jj_print_sfo

! =============================================================================

  subroutine jj_print_forcing(var_name, forcing)
  
    character (*),  intent(in)      :: var_name
    type(marbl_forcing_fields_type) :: forcing(:)

    integer*4 :: n, k
    character(len=char_len) :: msg

    write (msg, *) "\n ", var_name, ' shape: ', shape(forcing), "\n";    
    call mexPrintf(msg)
    do n=1,size(forcing)
    
      write (msg, *) var_name, n,                           &
        "\tname: ", trim(forcing(n)%metadata%varname),      &
        "\tunit: ", trim(forcing(n)%metadata%field_units),  &
        "\trank: ", (forcing(n)%metadata%rank)
      call mexPrintf(msg)
      if (forcing(n)%metadata%rank.EQ.0) then
        write (msg, *) 'size: '  , size( forcing(n)%field_0d), "\n"          ; call mexPrintf(msg)
        write (msg, *)                   forcing(n)%field_0d(:), '\n'        ; call mexPrintf(msg)
      else
        write (msg, *) 'shape: ' , shape(forcing(n)%field_1d)                ; call mexPrintf(msg)
        write (msg, *) 'extent: ',       forcing(n)%metadata%extent(:), "\n" ; call mexPrintf(msg)
! FIXME: the print fails use (;,:) if nlev > 15 or so
!         write (msg, *)                   forcing(n)%field_1d(:,:), '\n'      ; call mexPrintf(msg)
        do k=1,size(forcing(n)%field_1d,2)
          write (msg, *) (forcing(n)%field_1d(1,k)); call mexPrintf(msg)
        end do
        write (msg, *) "\n"; call mexPrintf(msg)
      end if
    
      call mexPrintf("\n")
      
    end do
    
  end subroutine jj_print_forcing

! =============================================================================

  subroutine jj_print_domain()

    integer*4 :: k
    character(len=char_len) :: msg

    write (msg, *) "num_PAR_subcols               ", marbl_instance%domain%num_PAR_subcols,"\n"; call mexPrintf(msg)
    write (msg, *) "num_elements_surface_flux     ", marbl_instance%domain%num_elements_surface_flux,"\n"; call mexPrintf(msg)
    write (msg, *) "num_elements_interior_tendency", marbl_instance%domain%num_elements_interior_tendency,"\n"; call mexPrintf(msg)
    write (msg, *) "km                            ", marbl_instance%domain%km,"\n"; call mexPrintf(msg)
    write (msg, *) "kmt                           ", marbl_instance%domain%kmt,       "\n"; call mexPrintf(msg)
    
! FIXME: these crash with print (*,zt(:), but works in a for loop, but only if km > 147 WTF?
!     write (msg, *) "delta_z", marbl_instance%domain%delta_z(:),"\n"; call mexPrintf(msg)
!     write (msg, *) "zw     ", marbl_instance%domain%zw(:),     "\n"; call mexPrintf(msg)
!     write (msg, *) "zt     ", marbl_instance%domain%zt(:),     "\n"; call mexPrintf(msg)
    
    write (msg, *) "zt     \n"; call mexPrintf(msg)
    do k=1,marbl_instance%domain%km
        write (msg, *) (marbl_instance%domain%zt(k)); call mexPrintf(msg)
    end do
    write (msg, *) "\n"; call mexPrintf(msg)
    
    write (msg, *) "zw     \n"; call mexPrintf(msg)
    do k=1,marbl_instance%domain%km
        write (msg, *) (marbl_instance%domain%zw(k)); call mexPrintf(msg)
    end do
    write (msg, *) "\n"; call mexPrintf(msg)
    
    write (msg, *) "delta_z\n"; call mexPrintf(msg)
    do k=1,marbl_instance%domain%km
        write (msg, *) (marbl_instance%domain%delta_z(k)); call mexPrintf(msg)
    end do
    write (msg, *) "\n"; call mexPrintf(msg)
    
  end subroutine jj_print_domain
  
! =============================================================================

  subroutine jj_print_marbl()

! Locals, tmps, and debug stuff...
    integer*4 :: num_fields, rank
    type(marbl_saved_state_type) :: marbl_state

    integer*4 :: k
    character(len=char_len) :: msg
        
! === check that the MARBL instance is alive and well
    call jj_print_domain()

! === data related to computing interior tendencies

    call jj_print_saved_state('interior_tendency_saved_state', marbl_instance%interior_tendency_saved_state)
    call jj_print_tracer('tracers', marbl_instance%tracers, surfaceNotInterior = .false.)
    call jj_print_forcing('interior_tendency_forcings', marbl_instance%interior_tendency_forcings)
    call jj_print_tracer('interior_tendencies', marbl_instance%interior_tendencies, surfaceNotInterior = .false.)
    call jj_print_diag('interior_tendency_diags', marbl_instance%interior_tendency_diags)

! === data related to computing surface fluxes

    call jj_print_saved_state('surface_flux_saved_state', marbl_instance%surface_flux_saved_state)
    call jj_print_tracer('tracers_at_surface', marbl_instance%tracers_at_surface, surfaceNotInterior = .true.)
    call jj_print_forcing('surface_flux_forcings', marbl_instance%surface_flux_forcings)
    call jj_print_tracer('surface_fluxes', marbl_instance%surface_fluxes, surfaceNotInterior = .true.)
    call jj_print_diag('surface_flux_diags', marbl_instance%surface_flux_diags)

    call jj_print_sfo ()
    
  end subroutine jj_print_marbl

! =============================================================================

  subroutine jj_print_log ()


 end subroutine jj_print_log

! =============================================================================

  subroutine jj_init_surface_flux_saved_state (initial_PH)

    real,                 intent(in) :: initial_PH

! Locals, tmps, and debug stuff...
    integer*4 :: n

    associate( flux => marbl_instance%surface_flux_saved_state)
    associate( marbl_state => flux%state )

      do n=1, flux%saved_state_cnt
        if (marbl_state(n)%rank.EQ.2) then
          marbl_state(n)%field_2d(:) = initial_PH
        else
          marbl_state(n)%field_3d(:,:) = initial_PH
        end if
      end do

    end associate
    end associate
  end subroutine jj_init_surface_flux_saved_state

 ! =============================================================================

  subroutine jj_init_saved_interior_tendency_saved_state (initial_PH)

    real,                 intent(in) :: initial_PH

    integer*4   :: n

    associate( flux => marbl_instance%interior_tendency_saved_state)
    associate( marbl_state => flux%state )

      do n=1, flux%saved_state_cnt
      
        if (marbl_state(n)%rank.EQ.2) then
          marbl_state(n)%field_2d(:) = initial_PH
        else
          marbl_state(n)%field_3d(:,:) = initial_PH
        end if
        
      end do

    end associate
    end associate
  end subroutine jj_init_saved_interior_tendency_saved_state

 ! =============================================================================

  subroutine jj_init_surface_flux_forcings ()

    integer*4   :: n

    associate( marbl_state => marbl_instance%surface_flux_forcings )

    do n=1,size(marbl_state)
      marbl_state(n)%field_0d = 0
    end do
    
    marbl_state( 2)%field_0d =  35; ! SSS(psu)
    marbl_state( 3)%field_0d =   3; ! SST(C)
    marbl_state( 9)%field_0d =   1; ! air pressure(bar)
    marbl_state(10)%field_0d = 380; ! xco2(ppmv)
    marbl_state(11)%field_0d = 380; ! xco2_alt_co2(ppmv)

    end associate
  end subroutine jj_init_surface_flux_forcings

 ! =============================================================================

  subroutine jj_init_interior_tendency_forcings ()

    integer*4   :: n

character(len=char_len) :: msg

    associate( marbl_state => marbl_instance%interior_tendency_forcings )

! write(msg, *) 'size(interior_tendency_forcings)', size(marbl_state), "\n"; call mexPrintf(msg)

    do n=1,size(marbl_state)
! write(msg, *) 'n', n, "\n"; call mexPrintf(msg)
    
      if (marbl_state(n)%metadata%rank.EQ.0) then
! write(msg, *) 'size(marbl_state(n)%field_0d,1)', size(marbl_state(n)%field_0d), "\n"; call mexPrintf(msg)
        marbl_state(n)%field_0d(:) = 0
      else
! write(msg, *) 'size(marbl_state(n)%field_1d,1)', size(marbl_state(n)%field_1d,1), "\n"; call mexPrintf(msg)
! write(msg, *) 'size(marbl_state(n)%field_1d,2)', size(marbl_state(n)%field_1d,2), "\n"; call mexPrintf(msg)
        marbl_state(n)%field_1d(:,:) = 0
      end if
      
    end do

    marbl_state(3)%field_1d(:,:) = 4.0 ! Potential T(C)

    end associate
  end subroutine jj_init_interior_tendency_forcings

 ! =============================================================================

  subroutine jj_init_tracers_at_surface ()

character(len=char_len) :: msg

! write(msg, *) 'size(tracers_at_surface,1)', size(marbl_instance%tracers_at_surface,1), "\n"; call mexPrintf(msg)
! write(msg, *) 'size(tracers_at_surface,2)', size(marbl_instance%tracers_at_surface,2), "\n"; call mexPrintf(msg)

  marbl_instance%tracers_at_surface(1,:)  = 0.001;

! some approximate and simple intial conditions

  marbl_instance%tracers_at_surface(1,1)  = 2.5   ! PO4
  marbl_instance%tracers_at_surface(1,2)  = 12.   ! NO3
  marbl_instance%tracers_at_surface(1,3)  = 1.    ! SiO3
  marbl_instance%tracers_at_surface(1,4)  = 0.8   ! NH4
  marbl_instance%tracers_at_surface(1,5)  = 0.005 ! Fe
  marbl_instance%tracers_at_surface(1,7)  = 10.   ! O2
  marbl_instance%tracers_at_surface(1,8)  = 2100. ! DOC
  marbl_instance%tracers_at_surface(1,9)  = 2100. ! DOC alt
  marbl_instance%tracers_at_surface(1,10) = 2350. ! ALK
  marbl_instance%tracers_at_surface(1,11) = 2350. ! ALK alt
  marbl_instance%tracers_at_surface(1,12) = 60.   ! DOC
  marbl_instance%tracers_at_surface(1,13) = 16.   ! DON
  marbl_instance%tracers_at_surface(1,14) = 0.3   ! DOP
  marbl_instance%tracers_at_surface(1,15) = 0.01  ! DOPr
  marbl_instance%tracers_at_surface(1,16) = 0.1   ! DONr
  marbl_instance%tracers_at_surface(1,17) = 1.    ! DOCr
  
  end subroutine jj_init_tracers_at_surface

 ! =============================================================================

  subroutine jj_init_tracers ()

  integer*4   :: n, k

character(len=char_len) :: msg

! write(msg, *) 'size(tracers,1)', size(marbl_instance%tracers,1), "\n"; call mexPrintf(msg)
! write(msg, *) 'size(tracers,2)', size(marbl_instance%tracers,2), "\n"; call mexPrintf(msg)

!   do n = 1, size(marbl_instance%tracers,1)        ! num tracers
    do   k = 1, size(marbl_instance%tracers,2)  ! num cols
        marbl_instance%tracers(:,k) = marbl_instance%tracers_at_surface(1,:)
    end do
!   end do
! return  
!   marbl_instance%tracers(:,1)  = 2.5   ! PO4
!   marbl_instance%tracers(:,2)  = 12.   ! NO3
!   marbl_instance%tracers(:,3)  = 1.    ! SiO3
!   marbl_instance%tracers(:,4)  = 0.8   ! NH4
!   marbl_instance%tracers(:,5)  = 0.005 ! Fe
!   marbl_instance%tracers(:,7)  = 10.   ! O2
!   marbl_instance%tracers(:,8)  = 2100. ! DOC
!   marbl_instance%tracers(:,9)  = 2100. ! DOC alt
!   marbl_instance%tracers(:,10) = 2350.  ! ALK
!   marbl_instance%tracers(:,11) = 2350.  ! ALK alt
!   marbl_instance%tracers(:,12) = 60.    ! DOC
!   marbl_instance%tracers(:,13) = 16.    ! DON
!   marbl_instance%tracers(:,14) = 0.3    ! DOP
!   marbl_instance%tracers(:,15) = 0.01   ! DOPr
!   marbl_instance%tracers(:,16) = 0.1    ! DONr
!   marbl_instance%tracers(:,17) = 1.     ! DOCr
  
  end subroutine jj_init_tracers

 ! =============================================================================

  subroutine jj_init_sfo ()

    type(marbl_log_type)    :: driver_status_log

    integer*4               :: sfo_id
    
    character(len=char_len) :: msg

    call marbl_instance%surface_flux_output%add_sfo( &
      field_name       ='flux_o2', &
      num_elements     = 1, &
      marbl_status_log = driver_status_log, &
      sfo_id = sfo_id)
    
    call marbl_instance%surface_flux_output%add_sfo( &
      field_name       ='flux_co2', &
      num_elements     = 1, &
      marbl_status_log = driver_status_log, &
      sfo_id = sfo_id)

    call marbl_instance%surface_flux_output%add_sfo( &
      field_name       ='flux_nhx', &
      num_elements     = 1, &
      marbl_status_log = driver_status_log, &
      sfo_id = sfo_id)
    
    call marbl_instance%surface_flux_output%add_sfo( &
      field_name       ='totalChl', &
      num_elements     = 1, &
      marbl_status_log = driver_status_log, &
      sfo_id = sfo_id)

!     write(msg,*) "sfo_id after jj_init_sfo: ", sfo_id, "\n";      call mexPrintf(msg);

  end subroutine jj_init_sfo

 ! =============================================================================      

  subroutine convertMarblLogToSingleString(log_line_cnt, log_as_single_string)

    integer,                    intent(out) :: log_line_cnt
    character(len=MAX_STR_LEN), intent(out) :: log_as_single_string

    ! Allocatable array for storing C_ptr returned by MARBL to log
    
    type(c_ptr)                             :: str_array_ptr
    integer*4                               :: strLen
    character(len=char_len), pointer        :: str_array(:)
  
    integer*4                               :: k, n
    character(len=char_len)                 :: msg, tmp

    ! Much tedious string hacking to get log file from MARBL over to MATLAB
    !
    ! 1) get -C- ptr of log from FORTRAN, and number of lines in log
    ! 2) allocate FORTRAN array of strings with log_cnt lines,of "char_len" per MARBL
    ! 3) Get FORTRAN pointer with same shape pointing to log

    call get_marbl_log(str_array_ptr, log_line_cnt)
    allocate(str_array(log_line_cnt))
    call c_f_pointer(str_array_ptr, str_array, shape=[log_line_cnt])
    
    ! check if buf is large enough
    
    if (log_line_cnt .ge. MAX_LOG_ENTRY_CNT) then
    
      write(msg, "(A,I0,A,I0,A)") "\n\nLog has ",  &
        log_line_cnt,                              &
        " entries, > max, Truncating to max of ", &
        MAX_LOG_ENTRY_CNT-1,                       &
        " entries...\n\n"
        
      call mexPrintf(msg)
      
      log_line_cnt = MAX_LOG_ENTRY_CNT-1
      
    end if

    ! create huge string with whole log in it, from array of short strings
    
    log_as_single_string = ''
    strLen = 0
    
    do n=1, log_line_cnt
      tmp = trim(str_array(n))
      
! FIXME: brute force copy
      do k=1,char_len
        log_as_single_string(strLen+k:strLen+k) = tmp(k:k)
      end do
      
      strLen = strLen + char_len
      
    end do

    ! Fiddle with log file as string, to send it to MATLAB
    
    n = strLen + 1
    log_as_single_string(n:n) = '.'
    
    deallocate(str_array)

  end subroutine convertMarblLogToSingleString

! =============================================================================

!  FIXME: doesnt work..
 subroutine removePercentFromString(str_in)
  use marbl_kinds_mod, only : char_len

  character(len=char_len), intent(inout) :: str_in

  integer*4 bad_char, i
  character(len=char_len) msg

  do i=1,LEN(str_in)
    bad_char = scan(str_in,'%') ! find first illegal character
    if (bad_char.NE.0) then
      str_in(bad_char:bad_char)='.'
    end if
  end do
  
  end subroutine removePercentFromString

! =============================================================================

  subroutine read_settings_file (settings_fname, ioerr, local_instance)

    character(len=*),            intent(in)     :: settings_fname
    integer,                     intent(out)    :: ioerr
    type(marbl_interface_class), intent(inout)  :: local_instance ! inout as inquire_settings_metadata updates StatusLog

    character(len=char_len), parameter          :: subname = 'marbl::read_settings_file'
    character(len=char_len)                     :: input_line

    character(len=char_len)                     :: msg
    integer                                     :: n
    
    write(msg, "(3A)") "Reading settings from file '", trim(settings_fname), "'\n"; call mexPrintf(msg)
    ioerr = 777
        
!     if (my_task .eq. 0) 
    open(97, file=trim(settings_fname), status="old", iostat=ioerr)
!     call marbl_mpi_bcast(ioerr, 0)
    if (ioerr .ne. 0) then
!       if (my_task .eq. 0) then
      write(*,"(A,I0)") "ioerr = ", ioerr
      write(*,"(2A)") "ERROR encountered when opening MARBL input file ", trim(settings_fname)
!       end if
!       call marbl_mpi_abort()
      write(msg,*) "read_settings_file: ioerr = ", ioerr,'\n'; call mexPrintf(msg)
      write(msg,*) "read_settings_file: ERROR encountered when opening MARBL input file \n", &
        trim(settings_fname); call mexPrintf(msg)
      write(msg,*) 'read_settings_file: aborting\n'; call mexPrintf(msg);
      return
    end if

    input_line = ''
    n = 0;
    do while(ioerr .eq. 0)
      ! (i) broadcast input_line and call put_setting(); abort if error
      !     calling with empty input_line on first entry to loop is okay, and
      !     this ensures we don't call put_setting with a garbage line if
      !     ioerr is non-zero
!       call marbl_mpi_bcast(input_line, 0)

      call local_instance%put_setting(input_line)
      
      if (local_instance%StatusLog%labort_marbl) then
        write (msg,*) "read_settings_file: ERROR encountered: put_setting \n";  call mexPrintf(msg)
        write (msg,*) 'read_settings_file: aborting\n';        call mexPrintf(msg); return
!         call local_instance%StatusLog%log_error_trace("put_setting(input_line)", subname)
!     FIXME:call print_marbl_log(local_instance%StatusLog)
      end if

      ! (ii) master task reads next line in input file
    !       if (my_task .eq. 0) 
      read(97,"(A)", iostat=ioerr) input_line

      ! (iii) broadcast input file line to all tasks (along with iostat)
    !       call marbl_mpi_bcast(ioerr, 0)
    end do

    if (.not.is_iostat_end(ioerr)) then
    !       if (my_task .eq. 0) then
        write(*,"(A,I0)") "read_settings_file: ioerr = ", ioerr
        write(*,"(2A)")   "read_settings_file: ERROR encountered when reading MARBL input file ", trim(settings_fname)
    !       end if
    !       call marbl_mpi_abort()
    end if

    !     if (my_task .eq. 0) 
      close(97)
      ioerr = 0

  end subroutine read_settings_file

! =============================================================================

  subroutine write_settings_file (settings_fname, ioerr)
! FIXME: doesnt work. var_cnt returns 20
    type(marbl_log_type)    :: driver_status_log

    character(len=*),            intent(in)     :: settings_fname
    integer,                     intent(out)    :: ioerr

    character(len=char_len), parameter          :: subname = 'marbl::write_settings_file'
    character(len=char_len)                     :: input_line, varname, sname, datatype
    character(len=char_len)                     :: sval
    logical                                     :: lval
    real(r8)                                    :: rval
    integer                                     :: ival
    
    integer                                     :: n
    integer                                     :: bad_char
    character(len=char_len)                     :: msg
    character(len=256)                          :: poo, foo, log_message, junk



!     if (.not. marbl_instance%StatusLog%labort_marbl) then
    
    ioerr = -1
    call driver_status_log%construct()

    write (msg, *) 'write_settings_file: settings_fname ', settings_fname, ' \n'; call mexPrintf(msg)
    write (msg, *) 'write_settings_file: settings_fname ', settings_fname, ' \n'; call mexPrintf(msg)
    n = marbl_instance%get_settings_var_cnt()
    write (msg, *) 'write_settings_file: get_settings_var_cnt:', n, ' \n'; call mexPrintf(msg)

    do n=1,marbl_instance%get_settings_var_cnt()

      call marbl_instance%inquire_settings_metadata(n, sname=varname)
      if (marbl_instance%StatusLog%labort_marbl) exit

      call marbl_instance%get_setting(varname, input_line, linput_file_format=.true.)
      if (marbl_instance%StatusLog%labort_marbl) exit

      call driver_status_log%log_noerror(input_line, subname)

    end do

    call print_marbl_log(driver_status_log, outfile=settings_fname)
! FIXME: print_marbl_log does not work printing to command window of Matlab
    call print_marbl_log(driver_status_log)

! FIXME: need to free the driver_status_log?
    ioerr = 110;

  end subroutine write_settings_file
  
! =============================================================================

  subroutine print_marbl_log(log_to_print, outfile)

    use marbl_logging, only                      : marbl_status_log_entry_type

    class(marbl_log_type),      intent(inout)   :: log_to_print
    character(len=*), optional, intent(in)      :: outfile
    
    type(marbl_status_log_entry_type), pointer  :: tmp
    integer                                     :: out_unit, n
    character(len=char_len)                     :: msg, foo

    ! write to stdout unless outfile is provided (only task 0 writes to file)
    out_unit = 6
!     if ((my_task .eq. 0) .and. (present(outfile))) then
    if (present(outfile)) then
      out_unit = 99
      open(out_unit, file=outfile, action="write", status="replace")
write(msg, "(3A)") "Writing to settings file '", trim(outfile), "'\n"; call mexPrintf(msg)
    end if

    tmp => log_to_print%FullLog
    n = 0;
    do while (associated(tmp))
!       if (mpi_on .and. (.not. tmp%lonly_master_writes)) then
!         ! If running in parallel and all tasks are writing to the log, prefix
!         ! the task # to log message
!         write(out_unit, "(I0,': ',A)") my_task, trim(tmp%LogMessage)
!       elseif (my_task.eq.0) then
        ! Otherwise only task 0 writes to the log and no prefix is necessary
        write(out_unit, "(A)") trim(tmp%LogMessage)
n = n+1;
! foo = trim(tmp%LogMessage)
! call removePercentFromString(foo)
! write(msg,         * ) n," ", foo, '\n'; call mexPrintf(msg)
!       end if
      tmp => tmp%next
    end do

!     if ((my_task .eq. 0) .and. (present(outfile))) then
    if (present(outfile)) then
      close(out_unit)
!       if (my_task .eq. 0) write(6, "(A)") "  ... Done writing to file!"
! write(msg, "(A)") "Done writing to file\n";call mexPrintf(msg)
    end if

    call log_to_print%erase()

!     if (log_to_print%labort_marbl) call marbl_mpi_abort()



! FIXME: "input_line" and "sname" both have the percent character "%". 
! FIXME: the percent character causes mexPrintf to either garble the string or crash
! FIXME: as a hack replace all '%' with '*'

! call marbl_instance%inquire_settings_metadata(n, sname=sname, datatype=datatype)
! 
! foo = trim(sname);
! bad_char = scan(foo,'%') ! find first illegal character
! if (bad_char.NE.0) then
!   foo(bad_char:bad_char)='*'
! end if
! 
! ! write(poo,"(I3,A,A,A)") n, " ", trim(foo), '\n'; call mexPrintf(poo)
! 
!       select case (trim(datatype))
!         case ('real')
!           call marbl_instance%get_setting(sname, rval)
!           write(log_message,  "(2A,E24.16,A)")    trim(foo), ' = ', rval,'\n'
! 
!         case ('integer')
!           call marbl_instance%get_setting(sname, ival)
!           write(log_message,  "(2A,I0,A)")        trim(foo), ' = ', ival,'\n'
!         
!         case ('string')
!           call marbl_instance%get_setting(sname, sval)
!           write(log_message,  "(4A)")             trim(foo), " = '", trim(sval),"'\n"
!         
!         case ('logical')
!           call marbl_instance%get_setting(sname, lval)
!           if (lval) then
!             write(log_message, "(3A)")          trim(foo), ' = T','\n'
!           else
!             write(log_message, "(3A)")          trim(foo), ' = F','\n'
!           end if
!         
!       end select
!       call mexPrintf(log_message)




  end subroutine print_marbl_log

! =============================================================================

end module marbl_interface_wrapper_mod

module ice_comp_mct

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!BOP
! !MODULE: ice_comp_mct
! !INTERFACE:

! !DESCRIPTION:
!  This is the main driver for the Model for Predication Across Scales Sea-Ice Model (MPAS-CICE).
!
! !REVISION HISTORY:
!  SVN:$Id:
!
! !USES:
   use mct_mod
   use esmf
   use seq_flds_mod
   use seq_cdata_mod
   use seq_infodata_mod
   use seq_timemgr_mod
   use seq_comm_mct,      only : seq_comm_suffix, seq_comm_inst, seq_comm_name
   use shr_file_mod 
   use shr_cal_mod,       only : shr_cal_date2ymd
   use shr_sys_mod
   use shr_pio_mod
   use perf_mod
   use ice_communicator,  only: mpi_communicator_ice

   use mpsi_cpl_indices
   use mpsi_mct_vars

   use mpas_framework
   use mpas_core
   use mpas_kind_types
   use mpas_io_units
   use mpas_stream_manager, only : MPAS_streamManager_type
!
! !PUBLIC MEMBER FUNCTIONS:
  implicit none
  public :: ice_init_mct
  public :: ice_run_mct
  public :: ice_final_mct
  SAVE
  private                              ! By default make data private

!
! ! PUBLIC DATA:
!
! !REVISION HISTORY:
! Author: Doug Jacobsen, Adrian Turner
!
!EOP
! !PRIVATE MODULE FUNCTIONS:
  private :: ice_export_mct
  private :: ice_import_mct
  private :: ice_SetGSMap_mct
  private :: ice_domain_mct
!
! !PRIVATE MODULE VARIABLES

  logical :: ldiag_cpl = .false.

  integer, private ::   &
      cpl_write_restart,   &! flag id for write restart
      cpl_write_history,   &! flag id for write history
      cpl_write_tavg,      &! flag id for write tavg      
      cpl_diag_global,     &! flag id for computing diagnostics
      cpl_diag_transp,     &! flag id for computing diagnostics
      my_task

   real (kind=RKIND) :: tlast_coupled

   integer  :: nsend, nrecv

   character(len=StrKIND) :: runtype, coupleTimeStamp

   type(seq_infodata_type), pointer :: infodata   
   type (iosystem_desc_t), pointer :: io_system 

   !! MPAS-CICE Datatypes
   type (dm_info), pointer :: dminfo
   type (domain_type), save, target:: domain
   type (MPAS_streamManager_type), pointer :: stream_manager
   integer :: itimestep, ice_cpl_dt

!=======================================================================

contains

!***********************************************************************
!BOP
!
! !IROUTINE: ice_init_mct
!
! !INTERFACE:
  subroutine ice_init_mct( EClock, cdata_i, x2i_i, i2x_i, NLFilename )!{{{
!
! !DESCRIPTION:
! Initialize MPAS-CICE
!
    !use mpas_stream_manager, only : MPAS_stream_mgr_init
    use mpas_stream_manager
    use iso_c_binding, only : c_char, c_loc, c_ptr, c_int
    use mpas_c_interfacing, only : mpas_f_to_c_string, mpas_c_to_f_string
    use mpas_timekeeping, only : MPAS_Time_type, mpas_get_clock_time, mpas_get_time
    use mpas_bootstrapping, only : mpas_bootstrap_framework_phase1, mpas_bootstrap_framework_phase2, mpas_setup_immutable_streams
    use mpas_io_units
! !INPUT/OUTPUT PARAMETERS:

    type(ESMF_Clock), intent(inout) :: EClock
    type(seq_cdata), intent(inout) :: cdata_i
    type(mct_aVect), intent(inout) :: x2i_i, i2x_i
    character(len=*), optional, intent(in) :: NLFilename ! Namelist filename
!
! !REVISION HISTORY:
! Author: Doug Jacobsen, Adrian Turner
!EOP
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

    type (domain_type), pointer :: domain_ptr

    integer :: ICEID, mpicom_i, lsize, start_ymd, start_tod, start_year, start_day,   &
       start_month, start_hour, iyear, shrloglev, shrlogunit, stdout_shr, pio_iotype

    type(mct_gsMap), pointer :: gsMap_i

    type(mct_gGrid), pointer :: dom_i

    integer :: errorCode  ! error code

    integer :: nThreads

    real (kind=RKIND) :: precadj
    character(len=StrKIND) :: cpl_seq_option

    character(len=StrKIND) :: cesm_calendar
    character(len=StrKIND) :: calendar_name
    integer :: calendar_id
    type (MPAS_Time_Type) :: currTime
    integer :: iam,ierr 
    integer :: iyear0, imonth0
    character(len=StrKIND)  :: starttype          ! infodata start type
    character(len=StrKIND)  :: timeStamp
    character(len=StrKIND)  :: nml_filename
    character(len=16) :: inst_suffix
    integer :: lbnum
    logical :: atm_aero

    !!!!AKT!!!! These need to be properly defined in MPAS-CICE when column physics added
    real(kind=RKIND) :: nextsw_cday ! ice_calendar
    real(kind=RKIND) :: eccen ! ice_orbital
    real(kind=RKIND) :: mvelpp ! ice_orbital
    real(kind=RKIND) :: lambm0 ! ice_orbital
    real(kind=RKIND) :: obliqr ! ice_orbital
    !!!!AKT!!!! These need to be properly defined in MPAS-CICE when column physics added

    type (MPAS_Time_Type) :: alarmStartTime
    type (MPAS_TimeInterval_Type) :: alarmTimeStep
    type (block_type), pointer :: block_ptr

    logical :: exists
    integer, pointer :: config_frames_per_outfile

    character(kind=c_char), dimension(StrKIND+1) :: c_filename       ! StrKIND+1 for C null-termination character
    integer(kind=c_int) :: c_comm
    integer(kind=c_int) :: c_ierr
    type (c_ptr) :: mgr_p
    character(len=StrKIND) :: mesh_stream
    character(len=StrKIND) :: mesh_filename
    character(len=StrKIND) :: mesh_filename_temp
    character(len=StrKIND) :: ref_time_temp
    character(len=StrKIND) :: filename_interval_temp
    character(kind=c_char), dimension(StrKIND+1) :: c_mesh_stream
    character(kind=c_char), dimension(StrKIND+1) :: c_mesh_filename_temp
    character(kind=c_char), dimension(StrKIND+1) :: c_ref_time_temp
    character(kind=c_char), dimension(StrKIND+1) :: c_filename_interval_temp
    type (MPAS_Time_type) :: start_time
    type (MPAS_Time_type) :: ref_time
    type (MPAS_TimeInterval_type) :: filename_interval
    character(len=StrKIND) :: start_timestamp

    logical, pointer :: config_write_output_on_startup
    logical, pointer :: tempLogicalConfig
    character(len=StrKIND), pointer :: tempCharConfig

    interface
       subroutine xml_stream_parser(xmlname, mgr_p, comm, ierr) bind(c)
          use iso_c_binding, only : c_char, c_ptr, c_int
          character(kind=c_char), dimension(*), intent(in) :: xmlname
          type (c_ptr), intent(inout) :: mgr_p
          integer(kind=c_int), intent(inout) :: comm
          integer(kind=c_int), intent(out) :: ierr
       end subroutine xml_stream_parser

       subroutine xml_stream_get_attributes(xmlname, streamname, comm, filename, reftime, fileintv, ierr) bind(c)
          use iso_c_binding, only : c_char, c_int
          character(kind=c_char), dimension(*), intent(in) :: xmlname
          character(kind=c_char), dimension(*), intent(in) :: streamname
          integer(kind=c_int), intent(inout) :: comm
          character(kind=c_char), dimension(*), intent(in) :: filename
          character(kind=c_char), dimension(*), intent(in) :: reftime
          character(kind=c_char), dimension(*), intent(in) :: fileintv
          integer(kind=c_int), intent(out) :: ierr
       end subroutine xml_stream_get_attributes
    end interface


!-----------------------------------------------------------------------
!
!   set cdata pointers
!
!-----------------------------------------------------------------------

    errorCode = 0 

    call seq_cdata_setptrs(cdata_i, ID=ICEID, mpicom=mpicom_i, &
         gsMap=gsMap_i, dom=dom_i, infodata=infodata)

    ! Determine time of next atmospheric shortwave calculation
    call seq_infodata_GetData(infodata, nextsw_cday=nextsw_cday )

    ! Determine if aerosols are coming from the coupler
    call seq_infodata_GetData(infodata, atm_aero=atm_aero )

    ! Determine orbital parameters
    call seq_infodata_GetData(infodata, orb_eccen=eccen, orb_mvelpp=mvelpp, &
         orb_lambm0=lambm0, orb_obliqr=obliqr)

    MPSI_MCT_ICEID =  ICEID
    MPSI_MCT_gsMap_i => gsMap_i
    MPSI_MCT_dom_i   => dom_i

    ! Remove after buildnml is building an actual namelist
    call seq_timemgr_EClockGetData(EClock, calendar=cesm_calendar)
    if( trim(cesm_calendar) == trim(seq_timemgr_noleap)) then
       write(calendar_name, *) 'gregorian_noleap'
    else if( trim(cesm_calendar) == trim(seq_timemgr_gregorian)) then
       write(calendar_name, *) 'gregorian'
    end if

     call MPI_comm_rank(mpicom_i,iam,ierr)
#if (defined _MEMTRACE)
     if(iam == 0) then
         lbnum=1
         call memmon_dump_fort('memmon.out','ice_init_mct:start::',lbnum) 
     endif
#endif

     ! Determine coupling type
     !!!AKT!!!call seq_infodata_GetData(infodata, cpl_seq_option=cpl_seq_option)

     ! The following communicator module variable will be utilized in init_communicate that
     ! is called by initial - this is done to make the code backwards compatible

     mpi_communicator_ice = mpicom_i

!-----------------------------------------------------------------------
!
!   initialize the model run 
!
!-----------------------------------------------------------------------

    call mpsi_cpl_indices_set()
    call seq_infodata_GetData( infodata, start_type=starttype)

    if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
       runtype = "initial"
    else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
       runtype = "continue"
    else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
       runtype = "branch"
    else
       write(stderrUnit, *) 'Start type is:', trim(starttype)
       call mpas_dmpar_global_abort(' ice_comp_mct ERROR: unknown starttype')
    end if
   
!-----------------------------------------------------------------------
!
!   first initializaiton phase of mpas-cice
!   initialize mpas-cice because grid information is needed for
!   creation of GSMap_ice.
!   call mpas-o initialization routines
!
!-----------------------------------------------------------------------
!   inst_name   = seq_comm_name(ICEID)
!   inst_index  = seq_comm_inst(ICEID)
    inst_suffix = seq_comm_suffix(ICEID)

    call t_startf('mpsi_init')
    nml_filename = "mpsi_in"

    io_system => shr_pio_getiosys(ICEID)

    pio_iotype = shr_pio_getiotype(ICEID)
    call MPAS_io_set_iotype(pio_iotype)

    inquire(file='ice_modelio.nml'//trim(inst_suffix),exist=exists)
    if(exists) then
       call shr_file_setio('ice_modelio.nml'//trim(inst_suffix),stdout_shr)
    endif

#ifndef MPAS_DEBUG
    if ( iam /= 0 ) then
       stdout_shr = 0
       stdout_shr = shr_file_getUnit()

       open(stdout_shr, file='/dev/null', position='APPEND')
    end if
#endif

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout_shr)

    domain_ptr => domain
    !DWJ 01/07/2015 Remove calendar name after buildnml is building an actual namelist
    call mpas_framework_init(dminfo, domain_ptr, mpi_communicator_ice, nml_filename, io_system, calendar_name, stdoutUnit_in=stdout_shr, stderrUnit_in=stdout_shr)
    !call mpas_framework_init(dminfo, domain_ptr, mpi_communicator_ice, nml_filename, io_system, stdoutUnit=stdout_shr, stderrUnit=stdout_shr)
    call mpas_core_setup_packages(domain_ptr % configs, domain_ptr % packages, ierr)

    ! Override namelist options based on start type
    if (runtype == "initial") then ! Start up run
        ! Turn off restart
        call mpas_pool_get_config(domain_ptr % configs, "config_do_restart", tempLogicalConfig)
        tempLogicalConfig = .false.

        ! Setup start time. Will be over written later when clocks are synchronized
        call mpas_pool_get_config(domain_ptr % configs, "config_start_time", tempCharConfig)
        tempCharConfig = "0001-01-01_00:00:00"

        ! Setup run duration. Will be ignored in coupled run, since coupler defines how long the run is.
        call mpas_pool_get_config(domain_ptr % configs, "config_run_duration", tempCharConfig)
        tempCharConfig = "0001-00-00_00:00:00"
    else if (runtype == "continue" .or. runtype == "branch") then ! Restart run or branch run
        ! Turn on restart
        call mpas_pool_get_config(domain_ptr % configs, "config_do_restart", tempLogicalConfig)
        tempLogicalConfig = .true.

        ! Set start time to be read from file
        call mpas_pool_get_config(domain_ptr % configs, "config_start_time", tempCharConfig)
        tempCharConfig = "file"

        ! Setup run duration. Will be ignored in coupled run, since coupler defines how long the run is.
        call mpas_pool_get_config(domain_ptr % configs, "config_run_duration", tempCharConfig)
        tempCharConfig = "0001-00-00_00:00:00"
    end if

    call mpas_core_setup_clock(domain_ptr % clock, domain_ptr % configs, ierr)


    !
    ! Using information from the namelist, a graph.info file, and a file containing
    !    mesh fields, build halos and allocate blocks in the domain
    !
    call mpas_core_get_mesh_stream(domain_ptr % configs, mesh_stream, ierr)
    if (ierr /= 0) then
       call mpas_dmpar_abort(dminfo)
    end if
    call mpas_f_to_c_string(domain % streams_filename, c_filename)
    call mpas_f_to_c_string(mesh_stream, c_mesh_stream)
    c_comm = dminfo % comm
    call xml_stream_get_attributes(c_filename, c_mesh_stream, c_comm, &
                                   c_mesh_filename_temp, c_ref_time_temp, &
                                   c_filename_interval_temp, c_ierr)
    if (c_ierr /= 0) then
       call mpas_dmpar_abort(dminfo)
    end if
    call mpas_c_to_f_string(c_mesh_filename_temp, mesh_filename_temp)
    call mpas_c_to_f_string(c_ref_time_temp, ref_time_temp)
    call mpas_c_to_f_string(c_filename_interval_temp, filename_interval_temp)

    start_time = mpas_get_clock_time(domain_ptr % clock, MPAS_START_TIME, ierr)
    if ( trim(ref_time_temp) == 'initial_time' ) then
        call mpas_get_time(start_time, dateTimeString=ref_time_temp, ierr=ierr)
    end if

    if ( trim(filename_interval_temp) == 'none' ) then
        call mpas_expand_string(ref_time_temp, mesh_filename_temp, mesh_filename)
    else
        call mpas_set_time(ref_time, dateTimeString=ref_time_temp, ierr=ierr)
        call mpas_set_timeInterval(filename_interval, timeString=filename_interval_temp, ierr=ierr)
        call mpas_build_stream_filename(ref_time, start_time, filename_interval, mesh_filename_temp, mesh_filename, ierr)
    end if
    write(stderrUnit, *) ' ** Attempting to bootstrap MPAS framework using stream: ', trim(mesh_stream)

    call mpas_bootstrap_framework_phase1(domain_ptr, mesh_filename)

    !
    ! Set up run-time streams
    !
    call MPAS_stream_mgr_init(stream_manager, domain % clock, domain % blocklist % allFields, domain % packages, domain % blocklist % allStructs)

    call add_stream_attributes(stream_manager, domain)

    call mpas_setup_immutable_streams(stream_manager)

    mgr_p = c_loc(stream_manager)
    call xml_stream_parser(c_filename, mgr_p, c_comm, c_ierr)
    if (c_ierr /= 0) then
       call mpas_dmpar_abort(dminfo)
    end if

    my_task = dminfo % my_proc_id

    !
    ! Finalize the setup of blocks and fields
    !
    call mpas_bootstrap_framework_phase2(stream_manager, domain_ptr)

    call mpas_timer_start("total time")

    call mpas_core_init(domain_ptr, stream_manager, timeStamp)

!-----------------------------------------------------------------------
!
!   initialize time-stamp information
!
!-----------------------------------------------------------------------
    call t_stopf ('mpsi_init')

!----------------------------------------------------------------------------
!
!   reset shr logging to my log file
!
!----------------------------------------------------------------------------
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdout_shr)

   
!-----------------------------------------------------------------------
!
!   check for consistency of mpas-o and sync clock initial time
!
!-----------------------------------------------------------------------

    if (runtype == 'initial') then
       call seq_timemgr_EClockGetData(EClock, ECurrTime=currTime % t)
       call mpas_set_clock_time(domain % clock, currTime, MPAS_START_TIME, ierr)
       call mpas_set_clock_time(domain % clock, currTime, MPAS_NOW, ierr)

       currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)
       call mpas_get_time(curr_time=currTime, YYYY=iyear0, MM=imonth0, ierr=ierr)
       call seq_timemgr_EClockGetData(EClock, start_ymd=start_ymd, start_tod=start_tod)
       call shr_cal_date2ymd(start_ymd,start_year,start_month,start_day)

       if (iyear0 /= start_year) then
          write(shrlogunit, *) ' Iyear0 is: ', iyear0
          write(shrlogunit,*) ' start_year is: ', start_year
          call mpas_dmpar_global_abort(' iyear0 does not match start_year')
       end if
       if (imonth0 /= start_month) then
          write(shrlogunit, *) ' Imonth0 is: ', imonth0
          write(shrlogunit, *) ' start_month is: ', start_month
          call mpas_dmpar_global_abort(' imonth0 does not match start_month')
       end if
    else if (runtype == 'continue' .or. runtype == 'branch') then
       call seq_timemgr_EClockGetData(EClock, ECurrTime=currTime % t)
       call mpas_set_clock_time(domain % clock, currTime, MPAS_START_TIME, ierr)
       call mpas_set_clock_time(domain % clock, currTime, MPAS_NOW, ierr)
    end if

!-----------------------------------------------------------------------
!
!   initialize MCT attribute vectors and indices
!
!-----------------------------------------------------------------------

    call t_startf ('mpsi_mct_init')

    call ice_SetGSMap_mct( mpicom_i, ICEID, GSMap_i )
    lsize = mct_gsMap_lsize(gsMap_i, mpicom_i)

    ! Initialize mct ice domain (needs ice initialization info)
    call ice_domain_mct( lsize, gsMap_i, dom_i )
    
    ! Inialize mct attribute vectors
    
    call mct_aVect_init(x2i_i, rList=seq_flds_x2i_fields, lsize=lsize)
    call mct_aVect_zero(x2i_i)
    
    call mct_aVect_init(i2x_i, rList=seq_flds_i2x_fields, lsize=lsize) 
    call mct_aVect_zero(i2x_i)
    
    nsend = mct_avect_nRattr(i2x_i)
    nrecv = mct_avect_nRattr(x2i_i)

!-----------------------------------------------------------------------
!
!   initialize necessary  coupling info
!
!-----------------------------------------------------------------------

    call seq_timemgr_EClockGetData(EClock, dtime=ice_cpl_dt)
    call convert_seconds_to_timestamp(ice_cpl_dt, coupleTimeStamp)

    ! set coupling alarm
    call mpas_set_timeInterval(alarmTimeStep, timeString=coupleTimeStamp, ierr=ierr)
    alarmStartTime = currTime
    call mpas_add_clock_alarm(domain % clock, coupleAlarmID, alarmStartTime, alarmTimeStep, ierr=ierr)
    call mpas_reset_clock_alarm(domain_ptr % clock, coupleAlarmID, ierr=ierr)

    ! Setup clock for initial runs
    if ( runtype == 'initial' ) then
      ! Check to see if cpl_seq_option is either tight option ('CESM1_ORIG_TIGHT' or 'CESM1_MOD_TIGHT')
      !!!!AKT!!!! These cpl_seq_option dont seem to be defined for CICE
      !if ( trim (cpl_seq_option) == 'CESM1_ORIG' .or. trim(cpl_seq_option) == 'CESM1_MOD' .or. &
      !     trim (cpl_seq_option) == 'RASM_OPTION1' .or. trim(cpl_seq_option) == 'RASM_OPTION2') then
         do while (.not. mpas_is_alarm_ringing(domain_ptr % clock, coupleAlarmID, ierr=ierr))
            itimestep = itimestep + 1
            call mpas_advance_clock(domain_ptr % clock)
         end do
      !end if
    end if

!-----------------------------------------------------------------------
!
!   send intial state to driver
!
!-----------------------------------------------------------------------

    call ice_export_mct(i2x_i, errorCode)  
    if (errorCode /= 0) then
       call mpas_dmpar_global_abort('ERROR in ice_export_mct')
    endif

    call t_stopf ('mpsi_mct_init')

    !!!AKT!!!call seq_infodata_PutData( infodata, ice_prognostic=.true., icerof_prognostic=.true.)

!-----------------------------------------------------------------------
!
!   get intial state from driver
!
!-----------------------------------------------------------------------

    call ice_import_mct(x2i_i, errorCode)  
    if (errorCode /= 0) then
       call mpas_dmpar_global_abort('ERROR in ice_import_mct')
    endif

    currTime = mpas_get_clock_time(domain % clock, MPAS_NOW, ierr)
    call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)
    write(stderrUnit,*) 'Initial time ', trim(timeStamp)

    itimestep = 0

    ! Reset all output alarms, to prevent intial time step from writing any output, unless it's ringing.
    call mpas_stream_mgr_reset_alarms(stream_manager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)
    call mpas_stream_mgr_reset_alarms(stream_manager, direction=MPAS_STREAM_INPUT, ierr=ierr)

!----------------------------------------------------------------------------
!
!   Reset shr logging to original values
!
!----------------------------------------------------------------------------

    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)

#if defined (_MEMTRACE)
    if(iam  == 0) then
        lbnum=1
        call memmon_dump_fort('memmon.out','ice_init_mct:end::',lbnum) 
        call memmon_reset_addr()
    endif
#endif
    call shr_sys_flush(stderrUnit)


!-----------------------------------------------------------------------
!EOC

 end subroutine ice_init_mct!}}}

!***********************************************************************
!BOP
!
! !IROUTINE: ice_run_mct
!
! !INTERFACE:
  subroutine ice_run_mct( EClock, cdata_i, x2i_i, i2x_i)!{{{

    use mpas_kind_types
    use mpas_grid_types
    use mpas_stream_manager
    use mpas_timer

    implicit none
!
! !DESCRIPTION:
! Run MPAS-CICE for one coupling interval
!
! !INPUT/OUTPUT PARAMETERS:
    type(ESMF_Clock)            , intent(inout) :: EClock
    type(seq_cdata)             , intent(inout) :: cdata_i
    type(mct_aVect)             , intent(inout) :: x2i_i
    type(mct_aVect)             , intent(inout) :: i2x_i

!! !REVISION HISTORY:
!! Author: Doug Jacobsen, Adrian Turner
!!EOP
!!-----------------------------------------------------------------------
!!
!!  local variables
!!
!!-----------------------------------------------------------------------
      integer :: ymd, tod, ihour, iminute, isecond
      integer :: iyear, imonth, iday, curr_ymd, curr_tod
      integer :: shrloglev, shrlogunit
      real (kind=RKIND) :: dt
      type (block_type), pointer :: block_ptr

      type (MPAS_Time_Type) :: currTime, alarmStartTime
      type (MPAS_TimeInterval_Type) :: alarmTimeStep
      type (domain_type), pointer :: domain_ptr
      character(len=StrKIND) :: timeStamp
      type (MPAS_timeInterval_type) :: timeStep
      integer :: ierr
      logical, pointer :: config_write_output_on_startup
      character (len=StrKIND), pointer :: config_restart_timestamp_name

      domain_ptr => domain

      call mpas_pool_get_config(domain_ptr % configs, 'config_restart_timestamp_name', config_restart_timestamp_name)

      ! Setup log information.
      call shr_file_getLogUnit (shrlogunit)
      call shr_file_getLogLevel(shrloglev)
      call shr_file_setLogUnit (stdoutUnit)

      timeStep = mpas_get_clock_timestep(domain_ptr % clock, ierr=ierr)
      call mpas_get_timeInterval(timeStep, dt=dt)
      call mpas_reset_clock_alarm(domain_ptr % clock, coupleAlarmID, ierr=ierr)

      ! Import state from coupler
      call ice_import_mct(x2i_i, ierr)

      ! Handle writing initial state
      call mpas_pool_get_config(domain_ptr % configs, 'config_write_output_on_startup', config_write_output_on_startup)
      if (config_write_output_on_startup) then
          call mpas_stream_mgr_write(stream_manager, 'output', forceWriteNow=.true., ierr=ierr)

          ! Reset config to false, so we don't write the state every coupling interval.
          config_write_output_on_startup = .false.
      endif

      ! During integration, time level 1 stores the model state at the beginning of the
      !   time step, and time level 2 stores the state advanced dt in time by timestep(...)
      ! This integration loop continues for a single coupling interval.
      do while (.not. mpas_is_alarm_ringing(domain_ptr % clock, coupleAlarmID, ierr=ierr))
         call mpas_stream_mgr_read(stream_manager, ierr=ierr)
         call mpas_stream_mgr_reset_alarms(stream_manager, direction=MPAS_STREAM_INPUT, ierr=ierr)

         itimestep = itimestep + 1
         call mpas_advance_clock(domain_ptr % clock)

         currTime = mpas_get_clock_time(domain_ptr % clock, MPAS_NOW, ierr)
         call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)
         write(stderrUnit,*) 'Doing timestep ', trim(timeStamp)

         call mpas_timer_start("time integration", .false., timeIntTimer)
         call mpas_timestep(domain_ptr, itimestep, dt, timeStamp)
         call mpas_timer_stop("time integration", timeIntTimer)

         ! Reset the restart alarm to prevent restart files being written without the coupler requesting it.
         call mpas_stream_mgr_reset_alarms(stream_manager, streamID='restart', ierr=ierr)

         call mpas_stream_mgr_write(stream_manager, streamID='output', ierr=ierr)
         call mpas_stream_mgr_reset_alarms(stream_manager, streamID='output', ierr=ierr)

         call mpas_stream_mgr_write(stream_manager, ierr=ierr)
         call mpas_stream_mgr_reset_alarms(stream_manager, direction=MPAS_STREAM_OUTPUT, ierr=ierr)

         call shr_sys_flush(stderrUnit)
      end do

      ! Check if coupler wants us to write a restart file.
      ! We only write restart files at the end of a coupling interval
      if (seq_timemgr_RestartAlarmIsOn(EClock)) then
         open(22, file=config_restart_timestamp_name, form='formatted', status='replace')
         write(22, *) trim(timeStamp)
         close(22)

         ! Write a restart file, because the coupler asked for it.
         call mpas_stream_mgr_write(stream_manager, forceWriteNow=.true., streamID='restart', ierr=ierr)
      end if

      ! Export state to coupler
      call ice_export_mct(i2x_i, ierr)

      ! Check if clocks are in sync
      currTime = mpas_get_clock_time(domain_ptr % clock, MPAS_NOW, ierr)
      call mpas_get_time(curr_time=currTime, YYYY=iyear, MM=imonth, DD=iday, H=ihour, M=iminute, S=isecond, ierr=ierr)
      call seq_timemgr_EClockGetData(EClock, curr_ymd=curr_ymd, curr_tod=curr_tod)
 
      ymd = iyear * 10000 + imonth*100 + iday
      tod = ihour * 3600 + iminute * 60 + isecond
      if (.not. seq_timemgr_EClockDateInSync( EClock, ymd, tod)) then
         write(stderrUnit,*) 'MPAS ymd=',ymd,' MPAS tod=', tod
         write(stderrUnit,*) 'sync ymd=',curr_ymd,' sync tod=', curr_tod
         write(stderrUnit,*) 'Internal mpas clock not in sync with sync clock'
      end if

      ! Reset I/O logs
      call shr_file_setLogUnit (shrlogunit)
      call shr_file_setLogLevel(shrloglev)


!-----------------------------------------------------------------------
!EOC

  end subroutine ice_run_mct!}}}

!***********************************************************************
!BOP
!
! !IROUTINE: ice_final_mct
!
! !INTERFACE:
  subroutine ice_final_mct( EClock, cdata_i, x2i_i, i2x_i)!{{{

    use mpas_stream_manager, only : MPAS_stream_mgr_finalize
!
! !DESCRIPTION:
! Finalize MPAS-CICE
!
! !USES:
! !ARGUMENTS:
    type(ESMF_Clock)            , intent(inout) :: EClock
    type(seq_cdata)             , intent(inout) :: cdata_i
    type(mct_aVect)             , intent(inout) :: x2i_i
    type(mct_aVect)             , intent(inout) :: i2x_i
!
! !REVISION HISTORY:
! Author: Doug Jacobsen, Adrian Turner
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------


    integer :: shrloglev, shrlogunit
    integer :: &
       errorCode         ! error code

    type (domain_type), pointer :: domain_ptr

!-----------------------------------------------------------------------

    domain_ptr => domain

    ! Setup I/O logs
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (stdoutUnit)

    ! Finalize MPAS-O
    call mpas_timer_stop("total time")
    call mpas_timer_write()
    call mpas_core_finalize(domain_ptr, stream_manager)
    call MPAS_stream_mgr_finalize(stream_manager)
    call mpas_framework_finalize(dminfo, domain_ptr, io_system)

    ! Reset I/O logs
    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)

  end subroutine ice_final_mct!}}}

!***********************************************************************
!BOP
!IROUTINE: ice_SetGSMap_mct
! !INTERFACE:

 subroutine ice_SetGSMap_mct( mpicom_ice, ICEID, gsMap_ice )!{{{

   use mpas_dmpar

! !DESCRIPTION:
!  This routine sets up the MPAS-CICE grid numbering for MCT
!
! !REVISION HISTORY:
!  same as module

! !INPUT/OUTPUT PARAMETERS:

    implicit none
    integer        , intent(in)    :: mpicom_ice
    integer        , intent(in)    :: ICEID
    type(mct_gsMap), intent(inout) :: gsMap_ice

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

    integer,allocatable :: &
      gindex(:)

    integer ::   &
      i,j, k, n, iblock, &
      lsize, gsize,   &
      ier

    type (block_type), pointer :: block_ptr 
    type (mpas_pool_type), pointer :: meshPool

    integer, dimension(:), pointer :: indexToCellID

    integer, pointer :: nCellsSolve

    ! Loop over all cells in all blocks to determine total number.
    n = 0
    block_ptr => domain % blocklist
    do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)

      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)

      n = n + nCellsSolve
      block_ptr => block_ptr % next
    end do

    ! Determine total number of cells across all processors
    lsize = n
    call mpas_dmpar_sum_int(dminfo, lsize, gsize)
    allocate(gindex(lsize),stat=ier)

    ! Setup the mapping (gindex)
    n = 0
    block_ptr => domain % blocklist
    do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)

      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)

      call mpas_pool_get_array(meshPool, 'indexToCellID', indexToCellID)

      do i = 1, nCellsSolve
        n = n + 1
        gindex(n) = indexToCellID(i)
      end do
      block_ptr => block_ptr % next
    end do

    ! Init the gsMap with gindex
    call mct_gsMap_init( gsMap_ice, gindex, mpicom_ice, ICEID, lsize, gsize )

    deallocate(gindex)

!-----------------------------------------------------------------------
!EOC

  end subroutine ice_SetGSMap_mct!}}}

!***********************************************************************
!BOP
! !IROUTINE: ice_domain_mct
! !INTERFACE:

 subroutine ice_domain_mct( lsize, gsMap_i, dom_i )!{{{

! !DESCRIPTION:
!  This routine sets up the MCT domain for MPAS-CICE
!
! !REVISION HISTORY:
!  same as module
!
! !INPUT/OUTPUT PARAMETERS:

   use mpas_constants, only: pii

    implicit none
    integer        , intent(in)    :: lsize
    type(mct_gsMap), intent(in)    :: gsMap_i
    type(mct_ggrid), intent(inout) :: dom_i   

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

    integer, pointer :: idata(:)

    real(kind=RKIND), pointer :: data(:)
    real(kind=RKIND) :: r2d

    integer :: i,j, k, n, ier

    type (block_type), pointer :: block_ptr

    type (mpas_pool_type), pointer :: meshPool

    integer, pointer :: nCellsSolve

    real (kind=RKIND), dimension(:), pointer :: lonCell, latCell, areaCell

    real (kind=RKIND), pointer :: sphere_radius

    r2d = 180.0/pii

!-------------------------------------------------------------------
!
!  initialize mct domain type
!
!-------------------------------------------------------------------

    call mct_gGrid_init( GGrid=dom_i, CoordChars=trim(seq_flds_dom_coord), &
       OtherChars=trim(seq_flds_dom_other), lsize=lsize )
    call mct_aVect_zero(dom_i%data)
    allocate(data(lsize))

!-------------------------------------------------------------------
!
! Determine global gridpoint number attribute, GlobGridNum, which is set automatically by MCT
!
!-------------------------------------------------------------------

    call mct_gsMap_orderedPoints(gsMap_i, my_task, idata)
    call mct_gGrid_importIAttr(dom_i,'GlobGridNum',idata,lsize)

!-------------------------------------------------------------------
!
! Determine domain 
! Initialize attribute vector with special value
!
!-------------------------------------------------------------------

    data(:) = -9999.0_R8 
    call mct_gGrid_importRAttr(dom_i,"lat"  ,data,lsize) 
    call mct_gGrid_importRAttr(dom_i,"lon"  ,data,lsize) 
    call mct_gGrid_importRAttr(dom_i,"area" ,data,lsize) 
    call mct_gGrid_importRAttr(dom_i,"aream",data,lsize) 
    data(:) = 1.0_R8     
    call mct_gGrid_importRAttr(dom_i,"mask",data,lsize) 
    call mct_gGrid_importRAttr(dom_i,"frac",data,lsize) 

!-------------------------------------------------------------------
!
! Fill in correct values for domain components
!
!-------------------------------------------------------------------

    n = 0
    block_ptr => domain % blocklist
    do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)

      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)

      call mpas_pool_get_array(meshPool, 'lonCell', lonCell)

      do i = 1, nCellsSolve
        n = n + 1
        data(n) = lonCell(i) * r2d
      end do
      
      block_ptr => block_ptr % next
    end do
    call mct_gGrid_importRattr(dom_i,"lon",data,lsize) 

    n = 0
    block_ptr => domain % blocklist
    do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)

      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)

      call mpas_pool_get_array(meshPool, 'latCell', latCell)

      do i = 1, nCellsSolve
        n = n + 1
        data(n) = latCell(i) * r2d
      end do
      block_ptr => block_ptr % next
    end do
    call mct_gGrid_importRattr(dom_i,"lat",data,lsize) 

    n = 0
    block_ptr => domain % blocklist
    do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)

      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)

      call mpas_pool_get_array(meshPool, 'areaCell', areaCell)

      call mpas_pool_get_config(meshPool, 'sphere_radius', sphere_radius)
      do i = 1, nCellsSolve
        n = n + 1
        data(n) = areaCell(i) / (sphere_radius * sphere_radius)
      end do
      block_ptr => block_ptr % next
    end do
    call mct_gGrid_importRattr(dom_i,"area",data,lsize) 

    data(:) = 1.0_RKIND ! No land cells in MPAS-O, only Ocean cells
    call mct_gGrid_importRattr(dom_i,"mask",data,lsize) 
    call mct_gGrid_importRattr(dom_i,"frac",data,lsize) 

    deallocate(data)
    deallocate(idata)

!-----------------------------------------------------------------------
!EOC

  end subroutine ice_domain_mct!}}}

!***********************************************************************
!BOP
! !IROUTINE: ice_import_mct
! !INTERFACE:

 subroutine ice_import_mct(x2i_i, errorCode)!{{{

! !DESCRIPTION:
!-----------------------------------------------------------------------
!
! !REVISION HISTORY:
!  same as module

! !INPUT/OUTPUT PARAMETERS:

    type(mct_aVect)   , intent(inout) :: x2i_i

! !OUTPUT PARAMETERS:

   integer, intent(out) :: &
      errorCode              ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   character (len=StrKIND) :: &
        label, &
        message
 
   integer ::  &
        iCell, &
        n
   
   type (block_type), pointer :: &
        block_ptr

   type (mpas_pool_type), pointer :: &
        meshPool, &
        atmosCouplingPool, &
        oceanCouplingPool

   integer, pointer :: nCellsSolve

   ! atmospheric forcing
   type (field1DReal), pointer :: &
        airLevelHeightField, &
        airPotentialTemperatureField, &
        airTemperatureField, &
        airSpecificHumidityField, &
        airDensityField, &
        shortwaveVisibleDirectDownField, &
        shortwaveVisibleDiffuseDownField, &
        shortwaveIRDirectDownField, &
        shortwaveIRDiffuseDownField, &
        longwaveDownField, &
        rainfallRateField, &
        snowfallRateField, &
        uAirVelocityField, &
        vAirVelocityField

   real(kind=RKIND), dimension(:), pointer :: &
        airLevelHeight, &
        airPotentialTemperature, &
        airTemperature, &
        airSpecificHumidity, &
        airDensity, &
        shortwaveVisibleDirectDown, &
        shortwaveVisibleDiffuseDown, &
        shortwaveIRDirectDown, &
        shortwaveIRDiffuseDown, &
        longwaveDown, &
        rainfallRate, &
        snowfallRate, &
        uAirVelocity, &
        vAirVelocity

   ! oceanic forcing
   type (field1DReal), pointer :: &
        seaSurfaceTemperatureField, &
        seaSurfaceSalinityField, &
        freezingMeltingPotentialField, &
        uOceanVelocityField, &
        vOceanVelocityField, &
        seaSurfaceTiltUField, &
        seaSurfaceTiltVField

   real(kind=RKIND), dimension(:), pointer :: &
        seaSurfaceTemperature, &
        seaSurfaceSalinity, &
        freezingMeltingPotential, &
        uOceanVelocity, &
        vOceanVelocity, &
        seaSurfaceTiltU, &
        seaSurfaceTiltV

!----------------------------------------------------------------------- 
!
!  zero out padded cells 
!
!-----------------------------------------------------------------------

   errorCode = 0

!-----------------------------------------------------------------------
!
!  unpack and distribute wind stress, then convert to correct units
!  and rotate components to local coordinates
!
!-----------------------------------------------------------------------

   n = 0
   block_ptr => domain % blocklist
   do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)
      call mpas_pool_get_subpool(block_ptr % structs, 'atmos_coupling', atmosCouplingPool)
      call mpas_pool_get_subpool(block_ptr % structs, 'ocean_coupling', oceanCouplingPool)
      
      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
      
      ! atmosphere
      call mpas_pool_get_field(atmosCouplingPool, "airLevelHeight", airLevelHeightField)
      call mpas_pool_get_field(atmosCouplingPool, "airPotentialTemperature", airPotentialTemperatureField)
      call mpas_pool_get_field(atmosCouplingPool, "airTemperature", airTemperatureField)
      call mpas_pool_get_field(atmosCouplingPool, "airSpecificHumidity", airSpecificHumidityField)
      call mpas_pool_get_field(atmosCouplingPool, "airDensity", airDensityField)
      call mpas_pool_get_field(atmosCouplingPool, "shortwaveVisibleDirectDown", shortwaveVisibleDirectDownField)
      call mpas_pool_get_field(atmosCouplingPool, "shortwaveVisibleDiffuseDown", shortwaveVisibleDiffuseDownField)
      call mpas_pool_get_field(atmosCouplingPool, "shortwaveIRDirectDown", shortwaveIRDirectDownField)
      call mpas_pool_get_field(atmosCouplingPool, "shortwaveIRDiffuseDown", shortwaveIRDiffuseDownField)
      call mpas_pool_get_field(atmosCouplingPool, "longwaveDown", longwaveDownField)
      call mpas_pool_get_field(atmosCouplingPool, "rainfallRate", rainfallRateField)
      call mpas_pool_get_field(atmosCouplingPool, "snowfallRate", snowfallRateField)
      call mpas_pool_get_field(atmosCouplingPool, "uAirVelocity", uAirVelocityField)
      call mpas_pool_get_field(atmosCouplingPool, "vAirVelocity", vAirVelocityField)

      ! ocean
      call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceTemperature", seaSurfaceTemperatureField)
      call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceSalinity", seaSurfaceSalinityField)
      call mpas_pool_get_field(oceanCouplingPool, "freezingMeltingPotential", freezingMeltingPotentialField)
      call mpas_pool_get_field(oceanCouplingPool, "uOceanVelocity", uOceanVelocityField)
      call mpas_pool_get_field(oceanCouplingPool, "vOceanVelocity", vOceanVelocityField)
      call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceTiltU", seaSurfaceTiltUField)
      call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceTiltV", seaSurfaceTiltVField)

      ! atmosphere
      airLevelHeight              => airLevelHeightField % array
      airPotentialTemperature     => airPotentialTemperatureField % array
      airTemperature              => airTemperatureField % array
      airSpecificHumidity         => airSpecificHumidityField % array
      airDensity                  => airDensityField % array
      shortwaveVisibleDirectDown  => shortwaveVisibleDirectDownField % array
      shortwaveVisibleDiffuseDown => shortwaveVisibleDiffuseDownField % array
      shortwaveIRDirectDown       => shortwaveIRDirectDownField % array
      shortwaveIRDiffuseDown      => shortwaveIRDiffuseDownField % array
      longwaveDown                => longwaveDownField % array
      rainfallRate                => rainfallRateField % array
      snowfallRate                => snowfallRateField % array
      uAirVelocity                => uAirVelocityField % array
      vAirVelocity                => vAirVelocityField % array

      ! ocean
      seaSurfaceTemperature       => seaSurfaceTemperatureField % array
      seaSurfaceSalinity          => seaSurfaceSalinityField % array
      freezingMeltingPotential    => freezingMeltingPotentialField % array
      uOceanVelocity              => uOceanVelocityField % array
      vOceanVelocity              => vOceanVelocityField % array
      seaSurfaceTiltU             => seaSurfaceTiltUField % array
      seaSurfaceTiltV             => seaSurfaceTiltVField % array

      do iCell = 1, nCellsSolve
         n = n + 1
         
         ! atmosphere
         airLevelHeight(iCell)              = x2i_i % rAttr(index_x2i_Sa_z, n)
         airPotentialTemperature(iCell)     = x2i_i % rAttr(index_x2i_Sa_ptem, n)
         airTemperature(iCell)              = x2i_i % rAttr(index_x2i_Sa_tbot, n)
         airSpecificHumidity(iCell)         = x2i_i % rAttr(index_x2i_Sa_shum, n)
         airDensity(iCell)                  = x2i_i % rAttr(index_x2i_Sa_dens, n)
         shortwaveVisibleDirectDown(iCell)  = x2i_i % rAttr(index_x2i_Faxa_swvdr, n)
         shortwaveVisibleDiffuseDown(iCell) = x2i_i % rAttr(index_x2i_Faxa_swvdf, n)
         shortwaveIRDirectDown(iCell)       = x2i_i % rAttr(index_x2i_Faxa_swndr, n)
         shortwaveIRDiffuseDown(iCell)      = x2i_i % rAttr(index_x2i_Faxa_swndf, n)
         longwaveDown(iCell)                = x2i_i % rAttr(index_x2i_Faxa_lwdn, n)
         rainfallRate(iCell)                = x2i_i % rAttr(index_x2i_Faxa_rain, n)
         snowfallRate(iCell)                = x2i_i % rAttr(index_x2i_Faxa_snow, n)
         uAirVelocity(iCell)                = x2i_i % rAttr(index_x2i_Sa_u, n)
         vAirVelocity(iCell)                = x2i_i % rAttr(index_x2i_Sa_v, n)

         ! ocean
         seaSurfaceTemperature(iCell)       = x2i_i % rAttr(index_x2i_So_t, n)
         seaSurfaceSalinity(iCell)          = x2i_i % rAttr(index_x2i_So_s, n)
         freezingMeltingPotential(iCell)    = x2i_i % rAttr(index_x2i_Fioo_q, n)
         uOceanVelocity(iCell)              = x2i_i % rAttr(index_x2i_So_u, n)
         vOceanVelocity(iCell)              = x2i_i % rAttr(index_x2i_So_v, n)
         seaSurfaceTiltU(iCell)             = x2i_i % rAttr(index_x2i_So_dhdx, n)
         seaSurfaceTiltV(iCell)             = x2i_i % rAttr(index_x2i_So_dhdy, n)

      end do ! iCell
      
      block_ptr => block_ptr % next
   end do ! blocks
   
   call mpas_pool_get_subpool(domain % blocklist % structs, 'atmos_coupling', atmosCouplingPool)
   call mpas_pool_get_subpool(domain % blocklist % structs, 'ocean_coupling', oceanCouplingPool)

   ! atmosphere
   call mpas_pool_get_field(atmosCouplingPool, "airLevelHeight", airLevelHeightField)
   call mpas_pool_get_field(atmosCouplingPool, "airPotentialTemperature", airPotentialTemperatureField)
   call mpas_pool_get_field(atmosCouplingPool, "airTemperature", airTemperatureField)
   call mpas_pool_get_field(atmosCouplingPool, "airSpecificHumidity", airSpecificHumidityField)
   call mpas_pool_get_field(atmosCouplingPool, "airDensity", airDensityField)
   call mpas_pool_get_field(atmosCouplingPool, "shortwaveVisibleDirectDown", shortwaveVisibleDirectDownField)
   call mpas_pool_get_field(atmosCouplingPool, "shortwaveVisibleDiffuseDown", shortwaveVisibleDiffuseDownField)
   call mpas_pool_get_field(atmosCouplingPool, "shortwaveIRDirectDown", shortwaveIRDirectDownField)
   call mpas_pool_get_field(atmosCouplingPool, "shortwaveIRDiffuseDown", shortwaveIRDiffuseDownField)
   call mpas_pool_get_field(atmosCouplingPool, "longwaveDown", longwaveDownField)
   call mpas_pool_get_field(atmosCouplingPool, "rainfallRate", rainfallRateField)
   call mpas_pool_get_field(atmosCouplingPool, "snowfallRate", snowfallRateField)
   call mpas_pool_get_field(atmosCouplingPool, "uAirVelocity", uAirVelocityField)
   call mpas_pool_get_field(atmosCouplingPool, "vAirVelocity", vAirVelocityField)
   
   ! ocean
   call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceTemperature", seaSurfaceTemperatureField)
   call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceSalinity", seaSurfaceSalinityField)
   call mpas_pool_get_field(oceanCouplingPool, "freezingMeltingPotential", freezingMeltingPotentialField)
   call mpas_pool_get_field(oceanCouplingPool, "uOceanVelocity", uOceanVelocityField)
   call mpas_pool_get_field(oceanCouplingPool, "vOceanVelocity", vOceanVelocityField)
   call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceTiltU", seaSurfaceTiltUField)
   call mpas_pool_get_field(oceanCouplingPool, "seaSurfaceTiltV", seaSurfaceTiltVField)

   ! atmosphere
   call mpas_dmpar_exch_halo_field(airLevelHeightField)
   call mpas_dmpar_exch_halo_field(airPotentialTemperatureField)
   call mpas_dmpar_exch_halo_field(airTemperatureField)
   call mpas_dmpar_exch_halo_field(airSpecificHumidityField)
   call mpas_dmpar_exch_halo_field(airDensityField)
   call mpas_dmpar_exch_halo_field(shortwaveVisibleDirectDownField)
   call mpas_dmpar_exch_halo_field(shortwaveVisibleDiffuseDownField)
   call mpas_dmpar_exch_halo_field(shortwaveIRDirectDownField)
   call mpas_dmpar_exch_halo_field(shortwaveIRDiffuseDownField)
   call mpas_dmpar_exch_halo_field(longwaveDownField)
   call mpas_dmpar_exch_halo_field(rainfallRateField)
   call mpas_dmpar_exch_halo_field(snowfallRateField)
   call mpas_dmpar_exch_halo_field(uAirVelocityField)
   call mpas_dmpar_exch_halo_field(vAirVelocityField)

   ! ocean
   call mpas_dmpar_exch_halo_field(seaSurfaceTemperatureField)
   call mpas_dmpar_exch_halo_field(seaSurfaceSalinityField)
   call mpas_dmpar_exch_halo_field(freezingMeltingPotentialField)
   call mpas_dmpar_exch_halo_field(uOceanVelocityField)
   call mpas_dmpar_exch_halo_field(vOceanVelocityField)
   call mpas_dmpar_exch_halo_field(seaSurfaceTiltUField)
   call mpas_dmpar_exch_halo_field(seaSurfaceTiltVField)

!-----------------------------------------------------------------------
!EOC

 end subroutine ice_import_mct!}}}

!***********************************************************************
!BOP
! !IROUTINE: ice_export_mct
! !INTERFACE:

 subroutine ice_export_mct(i2x_i, errorCode)   !{{{

   !use cice_constants, only: &
   !     ciceFreshWaterFreezingPoint
   use cice_mesh, only: &
        cice_latlon_vector_rotation_backward
   use shr_const_mod, only: SHR_CONST_SPVAL

! !DESCRIPTION:
!  This routine calls the routines necessary to send MPAS-O fields to
!  the CCSM cpl7 driver
!
! !REVISION HISTORY:
!  same as module
!
! !INPUT/OUTPUT PARAMETERS:

   type(mct_aVect)   , intent(inout) :: i2x_i

! !OUTPUT PARAMETERS:

   integer, intent(out) :: &
      errorCode              ! returned error code

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer :: &
        iCell, &
        n

   type (block_type), pointer :: &
        block_ptr

   type (mpas_pool_type), pointer :: &
        meshPool, &
        tracersPool, &
        icestatePool, &
        atmosForcingPool, &
        atmosFluxesPool, &
        oceanFluxesPool, &
        velocitySolverPool
  
   logical, pointer :: &
        config_rotate_cartesian_grid

   ! mesh
   integer, pointer :: &
        nCellsSolve

   real(kind=RKIND), dimension(:), pointer :: &
        xCell, &
        yCell, &
        zCell, &
        latCell, &
        lonCell

   real(kind=RKIND), pointer :: &
        sphere_radius

   ! tracers / state
   real(kind=RKIND), dimension(:), pointer :: &
        surfaceTemperature, &
        iceAreaCell, &
        snowVolumeCell

   ! atmosphere forcing
   real(kind=RKIND), dimension(:), pointer :: &
        albedoVisibleDirect, &
        albedoIRDirect, &
        albedoVisibleDiffuse, &
        albedoIRDiffuse, &
        atmosReferenceSpeed10m, &
        atmosReferenceTemperature2m, &
        atmosReferenceHumidity2m

   ! atmospheric fluxes
   real(kind=RKIND), dimension(:), pointer :: &
        uAirStress, &
        vAirStress, &
        latentHeatFlux, &
        sensibleHeatFlux, &
        longwaveUp, &
        evaporativeWaterFlux, &
        absorbedShortwaveFlux

   ! oceanic fluxes
   real(kind=RKIND), dimension(:), pointer :: &
        oceanHeatFlux, &
        penetratingShortwave, &
        freshWaterFlux, &
        saltFlux, &
        oceanStressU, &
        oceanStressV

   ! local temporary coupling variables
   real(kind=RKIND) :: &
        iceArea, &
        uAirStressCoupler, &
        vAirStressCoupler, &
        oceanStressUCoupler, &
        oceanStressVCoupler

   errorcode = 0

   ! init i2x array
   i2x_i % rAttr(:,:) = SHR_CONST_SPVAL

   n = 0
   block_ptr => domain % blocklist
   do while(associated(block_ptr))

     call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)
     call mpas_pool_get_subpool(block_ptr % structs, 'tracers', tracersPool)
     call mpas_pool_get_subpool(block_ptr % structs, 'icestate', icestatePool)
     call mpas_pool_get_subpool(block_ptr % structs, 'atmos_forcing', atmosForcingPool)
     call mpas_pool_get_subpool(block_ptr % structs, 'atmos_fluxes', atmosFluxesPool)
     call mpas_pool_get_subpool(block_ptr % structs, 'ocean_fluxes', oceanFluxesPool)
     call mpas_pool_get_subpool(block_ptr % structs, 'velocity_solver', velocitySolverPool)

     call mpas_pool_get_config(block_ptr % configs, 'config_rotate_cartesian_grid', config_rotate_cartesian_grid)

     ! mesh
     call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
     call mpas_pool_get_array(meshPool, 'xCell', xCell)
     call mpas_pool_get_array(meshPool, 'yCell', yCell)
     call mpas_pool_get_array(meshPool, 'zCell', zCell)
     call mpas_pool_get_array(meshPool, 'latCell', latCell)
     call mpas_pool_get_array(meshPool, 'lonCell', lonCell)
     call mpas_pool_get_config(meshPool, 'sphere_radius', sphere_radius)

     ! tracers / state
     call mpas_pool_get_array(tracersPool, 'surfaceTemperature', surfaceTemperature)
     call mpas_pool_get_array(icestatePool, 'iceAreaCell', iceAreaCell)
     call mpas_pool_get_array(icestatePool, 'snowVolumeCell', snowVolumeCell)

     ! atmosphere forcing
     call mpas_pool_get_array(atmosForcingPool, 'albedoVisibleDirect', albedoVisibleDirect)
     call mpas_pool_get_array(atmosForcingPool, 'albedoIRDirect', albedoIRDirect)
     call mpas_pool_get_array(atmosForcingPool, 'albedoVisibleDiffuse', albedoVisibleDiffuse)
     call mpas_pool_get_array(atmosForcingPool, 'albedoIRDiffuse', albedoIRDiffuse)
     call mpas_pool_get_array(atmosForcingPool, 'atmosReferenceSpeed10m', atmosReferenceSpeed10m)
     call mpas_pool_get_array(atmosForcingPool, 'atmosReferenceTemperature2m', atmosReferenceTemperature2m) 
     call mpas_pool_get_array(atmosForcingPool, 'atmosReferenceHumidity2m', atmosReferenceHumidity2m)

     ! atmospheric fluxes
     call mpas_pool_get_array(atmosFluxesPool, 'uAirStress', uAirStress)
     call mpas_pool_get_array(atmosFluxesPool, 'vAirStress', vAirStress)
     call mpas_pool_get_array(atmosFluxesPool, 'latentHeatFlux', latentHeatFlux)
     call mpas_pool_get_array(atmosFluxesPool, 'sensibleHeatFlux', sensibleHeatFlux)
     call mpas_pool_get_array(atmosFluxesPool, 'longwaveUp', longwaveUp)
     call mpas_pool_get_array(atmosFluxesPool, 'evaporativeWaterFlux', evaporativeWaterFlux)
     call mpas_pool_get_array(atmosFluxesPool, 'absorbedShortwaveFlux', absorbedShortwaveFlux)

     ! oceanic fluxes
     call mpas_pool_get_array(oceanFluxesPool, 'oceanHeatFlux', oceanHeatFlux)
     call mpas_pool_get_array(oceanFluxesPool, 'penetratingShortwave', penetratingShortwave)
     call mpas_pool_get_array(oceanFluxesPool, 'freshWaterFlux', freshWaterFlux)
     call mpas_pool_get_array(oceanFluxesPool, 'saltFlux', saltFlux)

     call mpas_pool_get_array(velocitySolverPool, 'oceanStressU', oceanStressU)
     call mpas_pool_get_array(velocitySolverPool, 'oceanStressV', oceanStressV)

     do iCell = 1, nCellsSolve
       n = n + 1

       ! states
       iceArea = min(iceAreaCell(iCell), 1.0_RKIND)

       if (iceArea < 0.0_RKIND) then
          write(stderrUnit,*) '(ice) send: ERROR iceArea < 0.0', iCell, iceArea
          call shr_sys_flush(stderrUnit)
       endif

       i2x_i % rAttr(index_i2x_Si_ifrac, n) = iceArea

       if (iceArea > 0.0_RKIND) then

          ! states
          i2x_i % rAttr(index_i2x_Si_t, n)       = surfaceTemperature(iCell)! + ciceFreshWaterFreezingPoint
          i2x_i % rAttr(index_i2x_Si_avsdr, n)   = albedoVisibleDirect(iCell)
          i2x_i % rAttr(index_i2x_Si_anidr, n)   = albedoIRDirect(iCell)
          i2x_i % rAttr(index_i2x_Si_avsdf, n)   = albedoVisibleDiffuse(iCell)
          i2x_i % rAttr(index_i2x_Si_anidf, n)   = albedoIRDiffuse(iCell)
          i2x_i % rAttr(index_i2x_Si_u10, n)     = atmosReferenceSpeed10m(iCell)
          i2x_i % rAttr(index_i2x_Si_tref, n)    = atmosReferenceTemperature2m(iCell)
          i2x_i % rAttr(index_i2x_Si_qref, n)    = atmosReferenceHumidity2m(iCell)
          i2x_i % rAttr(index_i2x_Si_snowh, n)   = snowVolumeCell(iCell) / iceArea

          ! atmosphere fluxes calculated by sea ice
          call cice_latlon_vector_rotation_backward(&
               uAirStressCoupler, &
               vAirStressCoupler, &
               uAirStress(iCell), &
               vAirStress(iCell), &
               latCell(iCell), &
               lonCell(iCell), &
               xCell(iCell), &
               yCell(iCell), &
               zCell(iCell), &
               sphere_radius, &
               config_rotate_cartesian_grid)

          i2x_i % rAttr(index_i2x_Faii_taux, n)  = uAirStressCoupler
          i2x_i % rAttr(index_i2x_Faii_tauy, n)  = vAirStressCoupler
          i2x_i % rAttr(index_i2x_Faii_lat, n)   = latentHeatFlux(iCell)
          i2x_i % rAttr(index_i2x_Faii_sen, n)   = sensibleHeatFlux(iCell)
          i2x_i % rAttr(index_i2x_Faii_lwup, n)  = longwaveUp(iCell)
          i2x_i % rAttr(index_i2x_Faii_evap, n)  = evaporativeWaterFlux(iCell)
          i2x_i % rAttr(index_i2x_Faii_swnet, n) = absorbedShortwaveFlux(iCell)

          ! ocean fluxes computed by sea ice
          call cice_latlon_vector_rotation_backward(&
               oceanStressUCoupler, &
               oceanStressVCoupler, &
               oceanStressU(iCell), &
               oceanStressV(iCell), &
               latCell(iCell), &
               lonCell(iCell), &
               xCell(iCell), &
               yCell(iCell), &
               zCell(iCell), &
               sphere_radius, &
               config_rotate_cartesian_grid)

          i2x_i % rAttr(index_i2x_Fioi_melth, n) = oceanHeatFlux(iCell)
          i2x_i % rAttr(index_i2x_Fioi_swpen, n) = penetratingShortwave(iCell)
          i2x_i % rAttr(index_i2x_Fioi_meltw, n) = freshWaterFlux(iCell)
          i2x_i % rAttr(index_i2x_Fioi_salt, n)  = saltFlux(iCell)
          i2x_i % rAttr(index_i2x_Fioi_taux, n)  = oceanStressUCoupler
          i2x_i % rAttr(index_i2x_Fioi_tauy, n)  = oceanStressVCoupler

       endif

     end do ! iCell

     block_ptr => block_ptr % next
   end do ! blocks

!-----------------------------------------------------------------------
!EOC

 end subroutine ice_export_mct!}}}

!-----------------------------------------------------------------------

 subroutine convert_seconds_to_timestamp(seconds, timeStamp)!{{{
   integer, intent(in) :: seconds
   character (len=StrKIND), intent(out) :: timeStamp
   real (kind=RKIND) :: secondsPerHour, secondsPerMinute, remaining
   integer :: minutes, hours, secondsLeft

   secondsPerHour = 3600
   secondsPerMinute = 60

   if(seconds < 0 .or. seconds > 86400) then
     secondsLeft = 00
     minutes = 00
     hours = 00
   else
     hours = int(seconds/secondsPerHour)
     remaining = seconds - real(hours) * secondsPerHour

     minutes = int(remaining/secondsPerMinute)
     remaining = remaining - real(minutes) * secondsPerMinute

     secondsLeft = int(remaining)
   end if

   write(timeStamp,"(a,i2.2,a,i2.2,a,i2.2)") "00_",hours,":",minutes,":",secondsLeft
   timeStamp = trim(timeStamp)

 end subroutine convert_seconds_to_timestamp!}}}

   subroutine add_stream_attributes(stream_manager, domain)!{{{

      use mpas_stream_manager, only : MPAS_stream_mgr_add_att

      implicit none

      type (MPAS_streamManager_type), intent(inout) :: stream_manager
      type (domain_type), intent(inout) :: domain

      type (MPAS_Pool_iterator_type) :: itr
      integer, pointer :: intAtt
      logical, pointer :: logAtt
      character (len=StrKIND), pointer :: charAtt
      real (kind=RKIND), pointer :: realAtt
      character (len=StrKIND) :: histAtt

      integer :: local_ierr

      if (domain % dminfo % nProcs < 10) then
          write(histAtt, '(A,I1,A,A,A)') 'mpirun -n ', domain % dminfo % nProcs, ' ./', trim(domain % coreName), '_model'
      else if (domain % dminfo % nProcs < 100) then
          write(histAtt, '(A,I2,A,A,A)') 'mpirun -n ', domain % dminfo % nProcs, ' ./', trim(domain % coreName), '_model'
      else if (domain % dminfo % nProcs < 1000) then
          write(histAtt, '(A,I3,A,A,A)') 'mpirun -n ', domain % dminfo % nProcs, ' ./', trim(domain % coreName), '_model'
      else if (domain % dminfo % nProcs < 10000) then
          write(histAtt, '(A,I4,A,A,A)') 'mpirun -n ', domain % dminfo % nProcs, ' ./', trim(domain % coreName), '_model'
      else if (domain % dminfo % nProcs < 100000) then
          write(histAtt, '(A,I5,A,A,A)') 'mpirun -n ', domain % dminfo % nProcs, ' ./', trim(domain % coreName), '_model'
      else
          write(histAtt, '(A,I6,A,A,A)') 'mpirun -n ', domain % dminfo % nProcs, ' ./', trim(domain % coreName), '_model'
      end if
     

      call MPAS_stream_mgr_add_att(stream_manager, 'on_a_sphere', domain % on_a_sphere)
      call MPAS_stream_mgr_add_att(stream_manager, 'sphere_radius', domain % sphere_radius)
      call MPAS_stream_mgr_add_att(stream_manager, 'model_name', domain % modelName)
      call MPAS_stream_mgr_add_att(stream_manager, 'core_name', domain % coreName)
      ! DWJ 10/01/2014: Eventually add the real history attribute, for now (due to length restrictions)
      ! add a shortened version.
!     call MPAS_stream_mgr_add_att(stream_manager, 'history', domain % history)
      call MPAS_stream_mgr_add_att(stream_manager, 'history', histAtt)
      call MPAS_stream_mgr_add_att(stream_manager, 'source', domain % source)
      call MPAS_stream_mgr_add_att(stream_manager, 'Conventions', domain % Conventions)
      call MPAS_stream_mgr_add_att(stream_manager, 'parent_id', domain % parent_id)
      call MPAS_stream_mgr_add_att(stream_manager, 'mesh_spec', domain % mesh_spec)
      call MPAS_stream_mgr_add_att(stream_manager, 'git_version', domain % git_version)

      call mpas_pool_begin_iteration(domain % configs)

      do while (mpas_pool_get_next_member(domain % configs, itr))

         if ( itr % memberType == MPAS_POOL_CONFIG) then

            if ( itr % dataType == MPAS_POOL_REAL ) then
               call mpas_pool_get_config(domain % configs, itr % memberName, realAtt)
               call MPAS_stream_mgr_add_att(stream_manager, itr % memberName, realAtt, ierr=local_ierr)
            else if ( itr % dataType == MPAS_POOL_INTEGER ) then
               call mpas_pool_get_config(domain % configs, itr % memberName, intAtt)
               call MPAS_stream_mgr_add_att(stream_manager, itr % memberName, intAtt, ierr=local_ierr)
            else if ( itr % dataType == MPAS_POOL_CHARACTER ) then
               call mpas_pool_get_config(domain % configs, itr % memberName, charAtt)
               call MPAS_stream_mgr_add_att(stream_manager, itr % memberName, charAtt, ierr=local_ierr)
            else if ( itr % dataType == MPAS_POOL_LOGICAL ) then
               call mpas_pool_get_config(domain % configs, itr % memberName, logAtt)
               if (logAtt) then
                  call MPAS_stream_mgr_add_att(stream_manager, itr % memberName, 'YES', ierr=local_ierr)
               else
                  call MPAS_stream_mgr_add_att(stream_manager, itr % memberName, 'NO', ierr=local_ierr)
               end if
            end if

          end if
      end do

   end subroutine add_stream_attributes!}}}


end module ice_comp_mct

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#!/bin/csh

# For now, manually build the namelist. Soon this will call the standard CESM
# build-namelist script.

mkdir -p $CASEROOT/CaseDocs

/bin/cp $CODEROOT/ocn/mpas-o/bld/m120/mpas-o.graph.info.part.* $CASEROOT/Buildconf/

# MNL - the mpas grid file (ocean120km.nc) is 51M so I don't want to add it to
# the repository. I think it should go in the inputdata repository, but we can
# figure that out later. For now, don't forget to copy ocean120km.nc from
# ~mlevy/MPAS-CESM_files/ to $CASEROOT/Buildconf
echo "NOTE: You must copy ocean120km.nc into $CASEROOT/Buildconf"

set MPAS_NML = $CASEROOT/CaseDocs/mpaso.in
touch $MPAS_NML
chmod 644 $MPAS_NML

if ($CONTINUE_RUN == 'TRUE') then
    set config_do_restart = .true.
    set config_start_time = 'file'
	set config_set_restingThickness_to_IC = .false.
	set config_alter_ICs_for_pbcs = .false.
    #TODO - config_start_time must not be read in - but obtained from the coupler
else
    set config_do_restart = .false.
    set config_start_time = '0001-01-01_00:00:00' 
	set config_set_restingThickness_to_IC = .true.
	set config_alter_ICs_for_pbcs = .true.
endif

cat >! $MPAS_NML << EOF
&time_management
  config_do_restart = $config_do_restart
  config_start_time = '$config_start_time'
  config_stop_time = 'none'
  config_run_duration = '000_01:00:01'
  config_calendar_type = '360day'
/
&io
  config_input_name = '$CASEROOT/Buildconf/ocean120km.nc'
  config_output_name = '$CASE.mpaso.hi.nc'
  config_restart_name = '$CASE.mpaso.r.nc'
  config_restart_interval = '1_00:00:00'
  config_restart_timestamp_name = 'rpointer.ocn'
  config_output_interval = '00_02:00:00'
  config_stats_interval = '1_00:00:00'
  config_write_stats_on_startup = .true.
  config_write_output_on_startup = .true.
  config_frames_per_outfile = 1000
  config_pio_num_iotasks = 0
  config_pio_stride = 16
/
&time_integration
  config_dt = 1800.0
  config_time_integrator = 'split_explicit'
/
&ALE_vertical_grid
	config_vert_coord_movement = "uniform_stretching"
	config_use_min_max_thickness = .false.
	config_min_thickness = 1.0
	config_max_thickness_factor = 6.0
	config_set_restingThickness_to_IC = $config_set_restingThickness_to_IC
	config_dzdk_positive = .false.
/
&ALE_frequency_filtered_thickness
	config_use_freq_filtered_thickness = .false.
	config_thickness_filter_timescale = 5.0
	config_use_highFreqThick_restore = .false.
	config_highFreqThick_restore_time = 30.0
	config_use_highFreqThick_del2 = .false.
	config_highFreqThick_del2 = 100.0
/
&partial_bottom_cells
	config_alter_ICs_for_pbcs = $config_alter_ICs_for_pbcs
	config_pbc_alteration_type = "full_cell"
	config_min_pbc_fraction = 0.10
	config_check_ssh_consistency = .true.
/
&decomposition
  config_num_halos = 3
  config_number_of_blocks = 0
  config_block_decomp_file_prefix = '$CASEROOT/Buildconf/mpas-o.graph.info.part.'
  config_explicit_proc_decomp = .false.
  config_proc_decomp_file_prefix = '$CASEROOT/Buildconf/mpas-o.graph.info.part.'
/
&hmix
  config_hmix_ScaleWithMesh = .false.
  config_maxMeshDensity = -1.0
  config_apvm_scale_factor = 0.0
/
&hmix_del2
  config_use_mom_del2 = .false.
  config_use_tracer_del2 = .false.
  config_mom_del2 = 0.1
  config_tracer_del2 = 0.1
/
&hmix_del4
  config_use_mom_del4 = .true.
  config_use_tracer_del4 = .false.
  config_mom_del4 = 5.0e13
  config_tracer_del4 = 5.0e14
/
&hmix_Leith
  config_use_Leith_del2 = .false.
  config_Leith_parameter = 1.0
  config_Leith_dx = 27000.0
  config_Leith_visc2_max = 2.5e3
/
&standard_GM
  config_h_kappa = 0.0
  config_h_kappa_q = 0.0
/
&Rayleigh_damping
  config_Rayleigh_friction = .false.
  config_Rayleigh_damping_coeff = 0.0
/
&vmix
  config_convective_visc = 1.0
  config_convective_diff = 1.0
/
&vmix_const
  config_use_const_visc = .false.
  config_use_const_diff = .false.
  config_vert_visc = 2.5e-4
  config_vert_diff = 2.5e-5
/
&cvmix
  config_use_cvmix = .false.
  config_cvmix_prandtl_number = 1.0
  config_use_cvmix_background = .true.
  config_cvmix_background_diffusion = 1.0e-5
  config_cvmix_background_viscosity = 1.0e-4
  config_use_cvmix_convection = .false.
  config_cvmix_convective_diffusion = 1.0
  config_cvmix_convective_viscosity = 1.0
  config_use_cvmix_kpp = .true.
  config_cvmix_kpp_criticalBulkRichardsonNumber = 0.25
  config_cvmix_kpp_interpolationOMLType = "quadratic"
/
&vmix_rich
  config_use_rich_visc = .true.
  config_use_rich_diff = .true.
  config_bkrd_vert_visc = 1.0e-4
  config_bkrd_vert_diff = 1.0e-5
  config_rich_mix = 0.005
/
&vmix_tanh
  config_use_tanh_visc = .false.
  config_use_tanh_diff = .false.
  config_max_visc_tanh = 2.5e-1
  config_min_visc_tanh = 1.0e-4
  config_max_diff_tanh = 2.5e-2
  config_min_diff_tanh = 1.0e-5
  config_zMid_tanh = -100
  config_zWidth_tanh = 100
/
&forcing
  config_forcing_type = 'bulk'
  config_restoreT_timescale = 90.0
  config_restoreS_timescale = 90.0
  config_restoreT_lengthscale = 50.0
  config_restoreS_lengthscale = 50.0
  config_flux_attenuation_coefficient = 0.01
  config_frazil_ice_formation = .true.
  config_sw_absorption_type = "jerlov"
  config_jerlov_water_type = 3 
  config_fixed_jerlov_weights = .true.
/
&advection
  config_vert_tracer_adv = 'stencil'
  config_vert_tracer_adv_order = 3
  config_horiz_tracer_adv_order = 3
  config_coef_3rd_order = 0.25
  config_monotonic = .true.
/
&bottom_drag
  config_bottom_drag_coeff = 1.0e-3
/
&pressure_gradient
  config_pressure_gradient_type = 'pressure_and_zmid'
  config_density0 = 1014.65
/
&eos
  config_eos_type = 'jm'
/
&eos_linear
  config_eos_linear_alpha = 2.55e-1
  config_eos_linear_beta = 7.64e-1
  config_eos_linear_Tref = 19.0
  config_eos_linear_Sref = 35.0
  config_eos_linear_densityref = 1025.022
/
&split_explicit_ts
  config_n_ts_iter = 2
  config_n_bcl_iter_beg = 1
  config_n_bcl_iter_mid = 2
  config_n_bcl_iter_end = 2
  config_n_btr_subcycles = 20
  config_n_btr_cor_iter = 2
  config_vel_correction = .true.
  config_btr_subcycle_loop_factor = 2
  config_btr_gam1_velWt1 = 0.5
  config_btr_gam2_SSHWt1 = 1.0
  config_btr_gam3_velWt2 = 1.0
  config_btr_solve_SSH2 = .false.
/
&debug
  config_check_zlevel_consistency = .false.
  config_filter_btr_mode = .false.
  config_prescribe_velocity = .false.
  config_prescribe_thickness = .false.
  config_include_KE_vertex = .false.
  config_check_tracer_monotonicity = .false.
  config_disable_thick_all_tend = .false.
  config_disable_thick_hadv = .false.
  config_disable_thick_vadv = .false.
  config_disable_vel_all_tend = .false.
  config_disable_vel_coriolis = .false.
  config_disable_vel_pgrad = .false.
  config_disable_vel_hmix = .false.
  config_disable_vel_windstress = .false.
  config_disable_vel_vmix = .false.
  config_disable_vel_vadv = .false.
  config_disable_tr_all_tend = .false.
  config_disable_tr_adv = .false.
  config_disable_tr_hmix = .false.
  config_disable_tr_vmix = .false.
/
EOF

/bin/cp $CASEROOT/CaseDocs/mpaso.in $RUNDIR

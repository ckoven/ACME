module micro_mg_utils

!--------------------------------------------------------------------------
!
! This module contains process rates and utility functions used by the MG
! microphysics.
!
! Original MG authors: Andrew Gettelman, Hugh Morrison
! Contributions from: Peter Caldwell, Xiaohong Liu and Steve Ghan
!
! Separated from MG 1.5 by B. Eaton.
! Separated module switched to MG 2.0 and further changes by S. Santos.
!
! for questions contact Hugh Morrison, Andrew Gettelman
! e-mail: morrison@ucar.edu, andrew@ucar.edu
!
!--------------------------------------------------------------------------
!
! List of required external functions that must be supplied:
!   gamma --> standard mathematical gamma function (if gamma is an
!       intrinsic, define HAVE_GAMMA_INTRINSICS)
!
!--------------------------------------------------------------------------
!
! Constants that must be specified in the "init" method (module variables):
!
! kind            kind of reals (to verify correct linkage only) -
! gravit          acceleration due to gravity                    m s-2
! rair            dry air gas constant for air                   J kg-1 K-1
! rh2o            gas constant for water vapor                   J kg-1 K-1
! cpair           specific heat at constant pressure for dry air J kg-1 K-1
! tmelt           temperature of melting point for water         K
! latvap          latent heat of vaporization                    J kg-1
! latice          latent heat of fusion                          J kg-1
!
!--------------------------------------------------------------------------

#ifndef HAVE_GAMMA_INTRINSICS
use shr_spfn_mod, only: gamma => shr_spfn_gamma
#endif

implicit none
private
save

public :: &
     micro_mg_utils_init, &
     size_dist_param_liq, &
     size_dist_param_basic, &
     avg_diameter, &
     ice_deposition_sublimation, &
     kk2000_liq_autoconversion, &
     ice_autoconversion, &
     immersion_freezing, &
     contact_freezing, &
     snow_self_aggregation, &
     accrete_cloud_water_snow, &
     secondary_ice_production, &
     accrete_rain_snow, &
     heterogeneous_rain_freezing, &
     accrete_cloud_water_rain, &
     self_collection_rain, &
     accrete_cloud_ice_snow, &
     evaporate_sublimate_precip, &
     bergeron_process_snow

! 8 byte real and integer
integer, parameter, public :: r8 = selected_real_kind(12)
integer, parameter, public :: i8 = selected_int_kind(18)

public :: MGHydrometeorProps

type :: MGHydrometeorProps
   ! Density (kg/m^3)
   real(r8) :: rho
   ! Information for size calculations.
   ! Basic calculation of mean size is:
   !     lambda = (shape_coef*nic/qic)^(1/eff_dim)
   ! Then lambda is constrained by bounds.
   real(r8) :: eff_dim
   real(r8) :: shape_coef
   real(r8) :: lambda_bounds(2)
   ! Minimum average particle mass (kg).
   ! Limit is applied at the beginning of the size distribution calculations.
   real(r8) :: min_mean_mass
end type MGHydrometeorProps

interface MGHydrometeorProps
   module procedure NewMGHydrometeorProps
end interface

type(MGHydrometeorProps), public :: mg_liq_props
type(MGHydrometeorProps), public :: mg_ice_props
type(MGHydrometeorProps), public :: mg_rain_props
type(MGHydrometeorProps), public :: mg_snow_props

!=================================================
! Public module parameters (mostly for MG itself)
!=================================================

! Pi to 20 digits; more than enough to reach the limit of double precision.
real(r8), parameter, public :: pi = 3.14159265358979323846_r8

! "One minus small number": number near unity for round-off issues.
real(r8), parameter, public :: omsm   = 1._r8 - 1.e-5_r8

! Smallest mixing ratio considered in microphysics.
real(r8), parameter, public :: qsmall = 1.e-18_r8

! minimum allowed cloud fraction
real(r8), parameter, public :: mincld = 0.0001_r8

real(r8), parameter, public :: rhosn = 250._r8  ! bulk density snow
real(r8), parameter, public :: rhoi = 500._r8   ! bulk density ice
real(r8), parameter, public :: rhow = 1000._r8  ! bulk density liquid
real(r8), parameter, public :: rhows = 917._r8  ! bulk density water solid

! autoconversion size threshold for cloud ice to snow (m)
real(r8) :: dcs

! fall speed parameters, V = aD^b (V is in m/s)
! droplets
real(r8), parameter, public :: ac = 3.e7_r8
real(r8), parameter, public :: bc = 2._r8
! snow
real(r8), parameter, public :: as = 11.72_r8
real(r8), parameter, public :: bs = 0.41_r8
! cloud ice
real(r8), parameter, public :: ai = 700._r8
real(r8), parameter, public :: bi = 1._r8
! rain
real(r8), parameter, public :: ar = 841.99667_r8
real(r8), parameter, public :: br = 0.8_r8

! mass of new crystal due to aerosol freezing and growth (kg)
real(r8), parameter, public :: mi0 = &
     4._r8/3._r8*pi*rhoi*(10.e-6_r8)*(10.e-6_r8)*(10.e-6_r8)

!=================================================
! Private module parameters
!=================================================

! Signaling NaN bit pattern that represents a limiter that's turned off.
integer(i8), parameter :: limiter_off = int(Z'7FF1111111111111', i8)

! alternate threshold used for some in-cloud mmr
real(r8), parameter :: icsmall = 1.e-8_r8

! particle mass-diameter relationship
! currently we assume spherical particles for cloud ice/snow
! m = cD^d
! exponent
real(r8), parameter :: dsph = 3._r8

! Bounds for mean diameter for different constituents.
! (E.g. ice must be at least 10 microns but no more than twice the
! threshold for autoconversion to snow.
real(r8) :: lam_bnd_ice(2)
real(r8), parameter :: lam_bnd_rain(2) = 1._r8/[500.e-6_r8, 20.e-6_r8]
real(r8), parameter :: lam_bnd_snow(2) = 1._r8/[2000.e-6_r8, 10.e-6_r8]

! Minimum average mass of particles.
real(r8), parameter :: min_mean_mass_liq = 1.e-20_r8
real(r8), parameter :: min_mean_mass_ice = 1.e-20_r8

! ventilation parameters
! for snow
real(r8), parameter :: f1s = 0.86_r8
real(r8), parameter :: f2s = 0.28_r8
! for rain
real(r8), parameter :: f1r = 0.78_r8
real(r8), parameter :: f2r = 0.308_r8

! collection efficiencies
! aggregation of cloud ice and snow
real(r8), parameter :: eii = 0.1_r8

! immersion freezing parameters, bigg 1953
real(r8), parameter :: bimm = 100._r8
real(r8), parameter :: aimm = 0.66_r8

! Mass of each raindrop created from autoconversion.
real(r8), parameter :: droplet_mass_25um = 4._r8/3._r8*pi*rhow*(25.e-6_r8)**3

!=========================================================
! Constants set in initialization
!=========================================================

! Set using arguments to micro_mg_init
real(r8) :: rv          ! water vapor gas constant
real(r8) :: cpp         ! specific heat of dry air
real(r8) :: tmelt       ! freezing point of water (K)

! latent heats of:
real(r8) :: xxlv        ! vaporization
real(r8) :: xlf         ! freezing
real(r8) :: xxls        ! sublimation

! additional constants to help speed up code
real(r8) :: gamma_bs_plus3
real(r8) :: gamma_half_br_plus5
real(r8) :: gamma_half_bs_plus5

!==========================================================================
contains
!==========================================================================

! Initialize module variables.
!
! "kind" serves no purpose here except to check for unlikely linking
! issues; always pass in the kind for a double precision real.
!
! "errstring" is the only output; it is blank if there is no error, or set
! to a message if there is an error.
!
! Check the list at the top of this module for descriptions of all other
! arguments.
subroutine micro_mg_utils_init( kind, rh2o, cpair, tmelt_in, latvap, &
     latice, errstring, dcs_in)

  integer,  intent(in)  :: kind
  real(r8), intent(in)  :: rh2o
  real(r8), intent(in)  :: cpair
  real(r8), intent(in)  :: tmelt_in
  real(r8), intent(in)  :: latvap
  real(r8), intent(in)  :: latice
  real(r8), intent(in)  :: dcs_in

  character(128), intent(out) :: errstring

  !-----------------------------------------------------------------------

  errstring = ' '

  if( kind .ne. r8 ) then
     errstring = 'micro_mg_init: KIND of reals does not match'
     return
  endif

  ! declarations for MG code (transforms variable names)

  rv= rh2o                  ! water vapor gas constant
  cpp = cpair               ! specific heat of dry air
  tmelt = tmelt_in
  dcs = dcs_in
  lam_bnd_ice(1) = 1._r8/(2._r8*dcs)
  lam_bnd_ice(2) = 1._r8/10.e-6_r8

  ! latent heats

  xxlv = latvap         ! latent heat vaporization
  xlf  = latice         ! latent heat freezing
  xxls = xxlv + xlf     ! latent heat of sublimation

  ! Define constants to help speed up code (this limits calls to gamma function)
  gamma_bs_plus3=gamma(3._r8+bs)
  gamma_half_br_plus5=gamma(5._r8/2._r8+br/2._r8)
  gamma_half_bs_plus5=gamma(5._r8/2._r8+bs/2._r8)

  ! Don't specify lambda bounds for cloud liquid, as they are determined by
  ! pgam dynamically.
  mg_liq_props = MGHydrometeorProps(rhow, dsph, min_mean_mass=min_mean_mass_liq)
  mg_ice_props = MGHydrometeorProps(rhoi, dsph, lam_bnd_ice, min_mean_mass_ice)
  mg_rain_props = MGHydrometeorProps(rhow, dsph, lam_bnd_rain)
  mg_snow_props = MGHydrometeorProps(rhosn, dsph, lam_bnd_snow)

end subroutine micro_mg_utils_init

! Constructor for a constituent property object.
function NewMGHydrometeorProps(rho, eff_dim, lambda_bounds, min_mean_mass) &
     result(res)
  real(r8), intent(in) :: rho, eff_dim
  real(r8), intent(in), optional :: lambda_bounds(2), min_mean_mass
  type(MGHydrometeorProps) :: res

  res%rho = rho
  res%eff_dim = eff_dim
  if (present(lambda_bounds)) then
     res%lambda_bounds = lambda_bounds
  else
     res%lambda_bounds = no_limiter()
  end if
  if (present(min_mean_mass)) then
     res%min_mean_mass = min_mean_mass
  else
     res%min_mean_mass = no_limiter()
  end if

  res%shape_coef = rho*pi*gamma(eff_dim+1._r8)/6._r8

end function NewMGHydrometeorProps

!========================================================================
!FORMULAS
!========================================================================

! Calculate correction due to latent heat for evaporation/sublimation
elemental function calc_ab(t, qv, xxl) result(ab)
  real(r8), intent(in) :: t     ! Temperature
  real(r8), intent(in) :: qv    ! Saturation vapor pressure
  real(r8), intent(in) :: xxl   ! Latent heat

  real(r8) :: ab

  real(r8) :: dqsdt

  dqsdt = xxl*qv / (rv * t**2)
  ab = 1._r8 + dqsdt*xxl/cpp

end function calc_ab

! get cloud droplet size distribution parameters
elemental subroutine size_dist_param_liq(props, qcic, ncic, rho, pgam, lamc)
  type(MGHydrometeorProps), intent(in) :: props
  real(r8), intent(in) :: qcic
  real(r8), intent(inout) :: ncic
  real(r8), intent(in) :: rho

  real(r8), intent(out) :: pgam
  real(r8), intent(out) :: lamc

  type(MGHydrometeorProps) :: props_loc

  if (qcic > qsmall) then

     ! Local copy of properties that can be modified.
     ! (Elemental routines that operate on arrays can't modify scalar
     ! arguments.)
     props_loc = props

     ! Get pgam from fit to observations of martin et al. 1994
     pgam = 0.0005714_r8*(ncic/1.e6_r8*rho) + 0.2714_r8
     pgam = 1._r8/(pgam**2) - 1._r8
     pgam = max(pgam, 2._r8)
     pgam = min(pgam, 15._r8)

     ! Set coefficient for use in size_dist_param_basic.
     props_loc%shape_coef = pi * props_loc%rho / 6._r8 * &
          rising_factorial(pgam+1._r8, props_loc%eff_dim)

     ! Limit to between 2 and 50 microns mean size.
     props_loc%lambda_bounds = (pgam+1._r8)*1._r8/[50.e-6_r8, 2.e-6_r8]

     call size_dist_param_basic(props_loc, qcic, ncic, lamc)

  else
     ! pgam not calculated in this case, so set it to a value likely to
     ! cause an error if it is accidentally used
     ! (gamma function undefined for negative integers)
     pgam = -100._r8
     lamc = 0._r8
  end if

contains

  ! Use gamma function to implement rising factorial extended to the reals.
  elemental function rising_factorial(x, n)
    real(r8), intent(in) :: x, n
    real(r8) :: rising_factorial

    rising_factorial = gamma(x+n)/gamma(x)

  end function rising_factorial

end subroutine size_dist_param_liq

! Basic routine for getting size distribution parameters.
elemental subroutine size_dist_param_basic(props, qic, nic, lam, n0)
  type(MGHydrometeorProps), intent(in) :: props
  real(r8), intent(in) :: qic
  real(r8), intent(inout) :: nic

  real(r8), intent(out) :: lam
  real(r8), intent(out), optional :: n0

  if (qic > qsmall) then

     ! add upper limit to in-cloud number concentration to prevent
     ! numerical error
     if (limiter_is_on(props%min_mean_mass)) then
        nic = min(nic, qic / props%min_mean_mass)
     end if

     ! lambda = (c n/q)^(1/d)
     lam = (props%shape_coef * nic/qic)**(1._r8/props%eff_dim)

     ! check for slope
     ! adjust vars
     if (lam < props%lambda_bounds(1)) then
        lam = props%lambda_bounds(1)
        nic = lam**(props%eff_dim) * qic/props%shape_coef
     else if (lam > props%lambda_bounds(2)) then
        lam = props%lambda_bounds(2)
        nic = lam**(props%eff_dim) * qic/props%shape_coef
     end if

  else
     lam = 0._r8
  end if

  if (present(n0)) n0 = nic * lam

end subroutine size_dist_param_basic

real(r8) elemental function avg_diameter(q, n, rho_air, rho_sub)
  ! Finds the average diameter of particles given their density, and
  ! mass/number concentrations in the air.
  real(r8), intent(in) :: q         ! mass mixing ratio
  real(r8), intent(in) :: n         ! number concentration
  real(r8), intent(in) :: rho_air   ! local density of the air
  real(r8), intent(in) :: rho_sub   ! density of the particle substance

  avg_diameter = (pi * rho_sub * n/(q*rho_air))**(-1._r8/3._r8)

end function avg_diameter

real(r8) elemental function var_coef(relvar, a)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  real(r8), intent(in) :: relvar
  real(r8), intent(in) :: a

  var_coef = gamma(relvar + a) / (gamma(relvar) * relvar**a)

end function var_coef

!========================================================================
!MICROPHYSICAL PROCESS CALCULATIONS
!========================================================================

!========================================================================
! Initial ice deposition and sublimation loop.
! Run before the main loop
! This subroutine written by Peter Caldwell

elemental subroutine ice_deposition_sublimation(t, qv, qi, ni, &
                                                icldm, rho, dv,qvl, qvi, &
                                                berg, vap_dep, ice_sublim)

  !INPUT VARS:
  !===============================================
  real(r8), intent(in) :: t
  real(r8), intent(in) :: qv
  real(r8), intent(in) :: qi
  real(r8), intent(in) :: ni
  real(r8), intent(in) :: icldm
  real(r8), intent(in) :: rho
  real(r8), intent(in) :: dv
  real(r8), intent(in) :: qvl
  real(r8), intent(in) :: qvi

  !OUTPUT VARS:
  !===============================================
  real(r8), intent(out) :: vap_dep !ice deposition (cell-ave value)
  real(r8), intent(out) :: ice_sublim !ice sublimation (cell-ave value)
  real(r8), intent(out) :: berg !bergeron enhancement (cell-ave value)

  !INTERNAL VARS:
  !===============================================
  real(r8) :: ab
  real(r8) :: epsi
  real(r8) :: qiic
  real(r8) :: niic
  real(r8) :: lami
  real(r8) :: n0i

  if (qi>=qsmall) then

     !GET IN-CLOUD qi, ni
     !===============================================
     qiic = qi/icldm
     niic = ni/icldm

     !Compute linearized condensational heating correction
     ab=calc_ab(t, qvi, xxls)
     !Get slope and intercept of gamma distn for ice.
     call size_dist_param_basic(mg_ice_props, qiic, niic, lami, n0i)
     !Get depletion timescale=1/eps
     epsi = 2._r8*pi*n0i*rho*Dv/(lami*lami)

     !Compute deposition/sublimation
     vap_dep = epsi/ab*(qv - qvi)

     !Make this a grid-averaged quantity
     vap_dep=vap_dep*icldm

     !Split into deposition or sublimation.
     if (t < tmelt .and. vap_dep>0._r8) then
        ice_sublim=0._r8
     else
     !hm, make ice_sublim negative for consistency with other evap/sub processes
        ice_sublim=min(vap_dep,0._r8)
        vap_dep=0._r8
     end if

     !sublimation occurs @ any T. Not so for berg.
     if (t < tmelt) then

        !Compute bergeron rate assuming cloud for whole step.
        berg = max(epsi/ab*(qvl - qvi), 0._r8)
     else !T>frz
        berg=0._r8
     end if !T<frz

  else !where qi<qsmall
     berg=0._r8
     vap_dep=0._r8
     ice_sublim=0._r8
  end if !qi>qsmall

end subroutine ice_deposition_sublimation

!========================================================================
! autoconversion of cloud liquid water to rain
! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
! minimum qc of 1 x 10^-8 prevents floating point error

elemental subroutine kk2000_liq_autoconversion(microp_uniform, qcic, &
     ncic, rho, relvar, prc, nprc, nprc1)

  logical, intent(in) :: microp_uniform

  real(r8), intent(in) :: qcic
  real(r8), intent(in) :: ncic
  real(r8), intent(in) :: rho

  real(r8), intent(in) :: relvar

  real(r8), intent(out) :: prc
  real(r8), intent(out) :: nprc
  real(r8), intent(out) :: nprc1

  real(r8) :: prc_coef

  ! Take variance into account, or use uniform value.
  if (.not. microp_uniform) then
     prc_coef = var_coef(relvar, 2.47_r8)
  else
     prc_coef = 1._r8
  end if

  if (qcic >= icsmall) then

     ! nprc is increase in rain number conc due to autoconversion
     ! nprc1 is decrease in cloud droplet conc due to autoconversion

     ! assume exponential sub-grid distribution of qc, resulting in additional
     ! factor related to qcvar below
     ! hm switch for sub-columns, don't include sub-grid qc

     prc = prc_coef * &
          1350._r8 * qcic**2.47_r8 * (ncic/1.e6_r8*rho)**(-1.79_r8)
     nprc = prc/droplet_mass_25um
     nprc1 = prc/(qcic/ncic)

  else
     prc=0._r8
     nprc=0._r8
     nprc1=0._r8
  end if

end subroutine kk2000_liq_autoconversion

!========================================================================
! Autoconversion of cloud ice to snow
! similar to Ferrier (1994)

elemental subroutine ice_autoconversion(t, qiic, lami, n0i, prci, nprci)

  real(r8), intent(in) :: t
  real(r8), intent(in) :: qiic
  real(r8), intent(in) :: lami
  real(r8), intent(in) :: n0i

  real(r8), intent(out) :: prci
  real(r8), intent(out) :: nprci

  ! Assume autoconversion timescale of 180 seconds.
  real(r8), parameter :: ac_time = 180._r8

  if (t <= tmelt .and. qiic >= qsmall) then

     nprci = n0i/(lami*ac_time)*exp(-lami*dcs)

     prci = pi*rhoi*n0i/(6._r8*ac_time)* &
          (dcs**3/lami+3._r8*dcs**2/lami**2+ &
          6._r8*dcs/lami**3+6._r8/lami**4)*exp(-lami*dcs)

  else
     prci=0._r8
     nprci=0._r8
  end if

end subroutine ice_autoconversion

! immersion freezing (Bigg, 1953)
!===================================

elemental subroutine immersion_freezing(microp_uniform, t, pgam, lamc, &
     cdist1, qcic, relvar, mnuccc, nnuccc)

  logical, intent(in) :: microp_uniform

  ! Temperature
  real(r8), intent(in) :: t

  ! Cloud droplet size distribution parameters
  real(r8), intent(in) :: pgam
  real(r8), intent(in) :: lamc
  real(r8), intent(in) :: cdist1

  ! MMR of in-cloud liquid water
  real(r8), intent(in) :: qcic

  ! Relative variance of cloud water
  real(r8), intent(in) :: relvar

  ! Output tendencies
  real(r8), intent(out) :: mnuccc ! MMR
  real(r8), intent(out) :: nnuccc ! Number

  ! Coefficients that will be omitted for sub-columns
  real(r8) :: dum, dum1


  if (.not. microp_uniform) then
     dum = var_coef(relvar, 2._r8)
     dum1 = var_coef(relvar, 1._r8)
  else
     dum = 1._r8
     dum1 = 1._r8
  end if

  if (qcic >= qsmall .and. t < 269.15_r8) then

     mnuccc = dum * &
          pi*pi/36._r8*rhow* &
          cdist1*gamma(7._r8+pgam)* &
          bimm*(exp(aimm*(tmelt - t))-1._r8)/lamc**3/lamc**3

     nnuccc = dum1 * &
          pi/6._r8*cdist1*gamma(pgam+4._r8) &
          *bimm*(exp(aimm*(tmelt - t))-1._r8)/lamc**3

  else
     mnuccc = 0._r8
     nnuccc = 0._r8
  end if ! qcic > qsmall and t < 4 deg C

end subroutine immersion_freezing

! contact freezing (-40<T<-3 C) (Young, 1974) with hooks into simulated dust
!===================================================================
! dust size and number in multiple bins are read in from companion routine

pure subroutine contact_freezing (microp_uniform, t, p, rndst, nacon, &
     pgam, lamc, cdist1, qcic, relvar, mnucct, nnucct)

  logical, intent(in) :: microp_uniform

  real(r8), intent(in) :: t(:)            ! Temperature
  real(r8), intent(in) :: p(:)            ! Pressure
  real(r8), intent(in) :: rndst(:,:)      ! Radius (for multiple dust bins)
  real(r8), intent(in) :: nacon(:,:)      ! Number (for multiple dust bins)

  ! Size distribution parameters for cloud droplets
  real(r8), intent(in) :: pgam(:)
  real(r8), intent(in) :: lamc(:)
  real(r8), intent(in) :: cdist1(:)

  ! MMR of in-cloud liquid water
  real(r8), intent(in) :: qcic(:)

  ! Relative cloud water variance
  real(r8), intent(in) :: relvar(:)

  ! Output tendencies
  real(r8), intent(out) :: mnucct(:) ! MMR
  real(r8), intent(out) :: nnucct(:) ! Number

  real(r8) :: tcnt                  ! scaled relative temperature
  real(r8) :: viscosity             ! temperature-specific viscosity (kg/m/s)
  real(r8) :: mfp                   ! temperature-specific mean free path (m)

  ! Dimension these according to number of dust bins, inferred from rndst size
  real(r8) :: nslip(size(rndst,2))  ! slip correction factors
  real(r8) :: ndfaer(size(rndst,2)) ! aerosol diffusivities (m^2/sec)

  ! Coefficients not used for subcolumns
  real(r8) :: dum, dum1

  integer  :: i

  do i = 1,size(t)

     if (qcic(i) >= qsmall .and. t(i) < 269.15_r8) then

        if (.not. microp_uniform) then
           dum = var_coef(relvar(i), 4._r8/3._r8)
           dum1 = var_coef(relvar(i), 1._r8/3._r8)
        else
           dum = 1._r8
           dum1 = 1._r8
        endif

        tcnt=(270.16_r8-t(i))**1.3_r8
        viscosity = 1.8e-5_r8*(t(i)/298.0_r8)**0.85_r8    ! Viscosity (kg/m/s)
        mfp = 2.0_r8*viscosity/ &                         ! Mean free path (m)
                     (p(i)*sqrt( 8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i)) ))

        ! Note that these two are vectors.
        nslip = 1.0_r8+(mfp/rndst(i,:))*(1.257_r8+(0.4_r8*exp(-(1.1_r8*rndst(i,:)/mfp))))! Slip correction factor

        ndfaer = 1.381e-23_r8*t(i)*nslip/(6._r8*pi*viscosity*rndst(i,:))  ! aerosol diffusivity (m2/s)

        mnucct(i) = dum *  &
             dot_product(ndfaer,nacon(i,:)*tcnt)*pi*pi/3._r8*rhow* &
             cdist1(i)*gamma(pgam(i)+5._r8)/lamc(i)**4

        nnucct(i) =  dum1 *  &
             dot_product(ndfaer,nacon(i,:)*tcnt)*2._r8*pi*  &
             cdist1(i)*gamma(pgam(i)+2._r8)/lamc(i)

     else

        mnucct(i)=0._r8
        nnucct(i)=0._r8

     end if ! qcic > qsmall and t < 4 deg C
  end do

end subroutine contact_freezing

! snow self-aggregation from passarelli, 1978, used by reisner, 1998
!===================================================================
! this is hard-wired for bs = 0.4 for now
! ignore self-collection of cloud ice

elemental subroutine snow_self_aggregation(t, rho, asn, rhosn, qsic, nsic, nsagg)

  real(r8), intent(in) :: t     ! Temperature
  real(r8), intent(in) :: rho   ! Density
  real(r8), intent(in) :: asn   ! fall speed parameter for snow
  real(r8), intent(in) :: rhosn ! density of snow

  ! In-cloud snow
  real(r8), intent(in) :: qsic ! MMR
  real(r8), intent(in) :: nsic ! Number

  ! Output number tendency
  real(r8), intent(out) :: nsagg

  if (qsic >= qsmall .and. t <= tmelt) then
     nsagg = -1108._r8*asn*eii* &
          pi**((1._r8-bs)/3._r8)*rhosn**((-2._r8-bs)/3._r8)* &
          rho**((2._r8+bs)/3._r8)*qsic**((2._r8+bs)/3._r8)* &
          (nsic*rho)**((4._r8-bs)/3._r8) /(4._r8*720._r8*rho)
  else
     nsagg=0._r8
  end if

end subroutine snow_self_aggregation

! accretion of cloud droplets onto snow/graupel
!===================================================================
! here use continuous collection equation with
! simple gravitational collection kernel
! ignore collisions between droplets/cloud ice
! since minimum size ice particle for accretion is 50 - 150 micron

elemental subroutine accrete_cloud_water_snow(t, rho, asn, uns, mu, qcic, ncic, qsic, &
     pgam, lamc, lams, n0s, psacws, npsacws)

  real(r8), intent(in) :: t   ! Temperature
  real(r8), intent(in) :: rho ! Density
  real(r8), intent(in) :: asn ! Fallspeed parameter (snow)
  real(r8), intent(in) :: uns ! Current fallspeed   (snow)
  real(r8), intent(in) :: mu  ! Viscosity

  ! In-cloud liquid water
  real(r8), intent(in) :: qcic ! MMR
  real(r8), intent(in) :: ncic ! Number

  ! In-cloud snow
  real(r8), intent(in) :: qsic ! MMR

  ! Cloud droplet size parameters
  real(r8), intent(in) :: pgam
  real(r8), intent(in) :: lamc

  ! Snow size parameters
  real(r8), intent(in) :: lams
  real(r8), intent(in) :: n0s

  ! Output tendencies
  real(r8), intent(out) :: psacws  ! Mass mixing ratio
  real(r8), intent(out) :: npsacws ! Number concentration

  real(r8) :: dc0 ! Provisional mean droplet size
  real(r8) :: dum
  real(r8) :: eci ! collection efficiency for riming of snow by droplets

  ! ignore collision of snow with droplets above freezing

  if (qsic >= qsmall .and. t <= tmelt .and. qcic >= qsmall) then

     ! put in size dependent collection efficiency
     ! mean diameter of snow is area-weighted, since
     ! accretion is function of crystal geometric area
     ! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)

     dc0 = (pgam+1._r8)/lamc
     dum = dc0*dc0*uns*rhow/(9._r8*mu*(1._r8/lams))
     eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))

     eci = max(eci,0._r8)
     eci = min(eci,1._r8)

     ! no impact of sub-grid distribution of qc since psacws
     ! is linear in qc

     psacws = pi/4._r8*asn*qcic*rho*n0s*eci*gamma_bs_plus3 / lams**(bs+3._r8)
     npsacws = pi/4._r8*asn*ncic*rho*n0s*eci*gamma_bs_plus3 / lams**(bs+3._r8)
  else
     psacws = 0._r8
     npsacws = 0._r8
  end if

end subroutine accrete_cloud_water_snow

! add secondary ice production due to accretion of droplets by snow
!===================================================================
! (Hallet-Mossop process) (from Cotton et al., 1986)

elemental subroutine secondary_ice_production(t, psacws, msacwi, nsacwi)
  real(r8), intent(in) :: t ! Temperature

  ! Accretion of cloud water to snow tendencies
  real(r8), intent(inout) :: psacws ! MMR

  ! Output (ice) tendencies
  real(r8), intent(out) :: msacwi ! MMR
  real(r8), intent(out) :: nsacwi ! Number

  if((t < 270.16_r8) .and. (t >= 268.16_r8)) then
     nsacwi = 3.5e8_r8*(270.16_r8-t)/2.0_r8*psacws
     msacwi = min(nsacwi*mi0, psacws)
  else if((t < 268.16_r8) .and. (t >= 265.16_r8)) then
     nsacwi = 3.5e8_r8*(t-265.16_r8)/3.0_r8*psacws
     msacwi = min(nsacwi*mi0, psacws)
  else
     nsacwi = 0.0_r8
     msacwi = 0.0_r8
  endif

  psacws = max(0.0_r8,psacws - nsacwi*mi0)

end subroutine secondary_ice_production

! accretion of rain water by snow
!===================================================================
! formula from ikawa and saito, 1991, used by reisner et al., 1998

elemental subroutine accrete_rain_snow(t, rho, umr, ums, unr, uns, qric, qsic, &
     lamr, n0r, lams, n0s, pracs, npracs )

  real(r8), intent(in) :: t   ! Temperature
  real(r8), intent(in) :: rho ! Density

  ! Fallspeeds
  ! mass-weighted
  real(r8), intent(in) :: umr ! rain
  real(r8), intent(in) :: ums ! snow
  ! number-weighted
  real(r8), intent(in) :: unr ! rain
  real(r8), intent(in) :: uns ! snow

  ! In cloud MMRs
  real(r8), intent(in) :: qric ! rain
  real(r8), intent(in) :: qsic ! snow

  ! Size distribution parameters
  ! rain
  real(r8), intent(in) :: lamr
  real(r8), intent(in) :: n0r
  ! snow
  real(r8), intent(in) :: lams
  real(r8), intent(in) :: n0s

  ! Output tendencies
  real(r8), intent(out) :: pracs  ! MMR
  real(r8), intent(out) :: npracs ! Number

  ! Collection efficiency for accretion of rain by snow
  real(r8), parameter :: ecr = 1.0_r8

  if (qric >= icsmall .and. qsic >= icsmall .and. t <= tmelt) then

     pracs = pi*pi*ecr*(((1.2_r8*umr-0.95_r8*ums)**2 + &
          0.08_r8*ums*umr)**0.5_r8 *  &
          rhow * rho * n0r * n0s * &
          (5._r8/(lamr**6 * lams)+ &
          2._r8/(lamr**5 * lams**2)+ &
          0.5_r8/(lamr**4 * lams**3)))

     npracs = pi/2._r8*rho*ecr* (1.7_r8*(unr-uns)**2 + &
          0.3_r8*unr*uns)**0.5_r8 * &
          n0r*n0s* &
          (1._r8/(lamr**3 * lams)+ &
          1._r8/(lamr**2 * lams**2)+ &
          1._r8/(lamr * lams**3))

  else
     pracs = 0._r8
     npracs = 0._r8
  end if

end subroutine accrete_rain_snow

! heterogeneous freezing of rain drops
!===================================================================
! follows from Bigg (1953)

elemental subroutine heterogeneous_rain_freezing(t, qric, nric, lamr, mnuccr, nnuccr)

  real(r8), intent(in) :: t    ! Temperature

  ! In-cloud rain
  real(r8), intent(in) :: qric ! MMR
  real(r8), intent(in) :: nric ! Number
  real(r8), intent(in) :: lamr ! size parameter

  ! Output tendencies
  real(r8), intent(out) :: mnuccr ! MMR
  real(r8), intent(out) :: nnuccr ! Number

  if (t < 269.15_r8 .and. qric >= qsmall) then

     ! Division by lamr**3 twice is old workaround to avoid overflow.
     ! Probably no longer necessary
     mnuccr = 20._r8*pi*pi*rhow*nric*bimm* &
          (exp(aimm*(tmelt - t))-1._r8)/lamr**3 &
          /lamr**3

     nnuccr = pi*nric*bimm* &
          (exp(aimm*(tmelt - t))-1._r8)/lamr**3
  else
     mnuccr = 0._r8
     nnuccr = 0._r8
  end if
end subroutine heterogeneous_rain_freezing

! accretion of cloud liquid water by rain
!===================================================================
! formula from Khrouditnov and Kogan (2000)
! gravitational collection kernel, droplet fall speed neglected

elemental subroutine accrete_cloud_water_rain(microp_uniform, qric, qcic, &
     ncic, relvar, accre_enhan, pra, npra)

  logical, intent(in) :: microp_uniform

  ! In-cloud rain
  real(r8), intent(in) :: qric ! MMR

  ! Cloud droplets
  real(r8), intent(in) :: qcic ! MMR
  real(r8), intent(in) :: ncic ! Number

  ! SGS variability
  real(r8), intent(in) :: relvar
  real(r8), intent(in) :: accre_enhan

  ! Output tendencies
  real(r8), intent(out) :: pra  ! MMR
  real(r8), intent(out) :: npra ! Number

  ! Coefficient that varies for subcolumns
  real(r8) :: pra_coef

  if (.not. microp_uniform) then
     pra_coef = accre_enhan * var_coef(relvar, 1.15_r8)
  else
     pra_coef = 1._r8
  end if

  if (qric >= qsmall .and. qcic >= qsmall) then

     ! include sub-grid distribution of cloud water
     pra = pra_coef * 67._r8*(qcic*qric)**1.15_r8

     npra = pra/(qcic/ncic)

  else
     pra = 0._r8
     npra = 0._r8
  end if
end subroutine accrete_cloud_water_rain

! Self-collection of rain drops
!===================================================================
! from Beheng(1994)

elemental subroutine self_collection_rain(rho, qric, nric, nragg)

  real(r8), intent(in) :: rho  ! Air density

  ! Rain
  real(r8), intent(in) :: qric ! MMR
  real(r8), intent(in) :: nric ! Number

  ! Output number tendency
  real(r8), intent(out) :: nragg

  if (qric >= qsmall) then
     nragg = -8._r8*nric*qric*rho
  else
     nragg = 0._r8
  end if

end subroutine self_collection_rain

! Accretion of cloud ice by snow
!===================================================================
! For this calculation, it is assumed that the Vs >> Vi
! and Ds >> Di for continuous collection

elemental subroutine accrete_cloud_ice_snow(t, rho, asn, qiic, niic, qsic, &
     lams, n0s, prai, nprai)

  real(r8), intent(in) :: t    ! Temperature
  real(r8), intent(in) :: rho  ! Density

  real(r8), intent(in) :: asn  ! Snow fallspeed parameter

  ! Cloud ice
  real(r8), intent(in) :: qiic ! MMR
  real(r8), intent(in) :: niic ! Number

  real(r8), intent(in) :: qsic ! Snow MMR

  ! Snow size parameters
  real(r8), intent(in) :: lams
  real(r8), intent(in) :: n0s

  ! Output tendencies
  real(r8), intent(out) :: prai  ! MMR
  real(r8), intent(out) :: nprai ! Number

  if (qsic >= qsmall .and. qiic >= qsmall .and. t <= tmelt) then

     prai = pi/4._r8 * asn * qiic * rho * n0s * eii * gamma_bs_plus3/ &
          lams**(bs+3._r8)

     nprai = pi/4._r8 * asn * niic * rho * n0s * eii * gamma_bs_plus3/ &
          lams**(bs+3._r8)
  else
     prai = 0._r8
     nprai = 0._r8
  end if

end subroutine accrete_cloud_ice_snow

! calculate evaporation/sublimation of rain and snow
!===================================================================
! note: evaporation/sublimation occurs only in cloud-free portion of grid cell
! in-cloud condensation/deposition of rain and snow is neglected
! except for transfer of cloud water to snow through bergeron process

elemental subroutine evaporate_sublimate_precip(t, rho, dv, mu, sc, q, qvl, qvi, &
     lcldm, cldmax, arn, asn, qcic, qiic, qric, qsic, lamr, n0r, lams, n0s, &
     pre, prds)

  real(r8), intent(in) :: t    ! temperature
  real(r8), intent(in) :: rho  ! air density
  real(r8), intent(in) :: dv   ! water vapor diffusivity
  real(r8), intent(in) :: mu   ! viscosity
  real(r8), intent(in) :: sc   ! schmidt number
  real(r8), intent(in) :: q    ! humidity
  real(r8), intent(in) :: qvl  ! saturation humidity (water)
  real(r8), intent(in) :: qvi  ! saturation humidity (ice)
  real(r8), intent(in) :: lcldm  ! liquid cloud fraction
  real(r8), intent(in) :: cldmax ! precipitation fraction (maximum overlap)

  ! fallspeed parameters
  real(r8), intent(in) :: arn  ! rain
  real(r8), intent(in) :: asn  ! snow

  ! In-cloud MMRs
  real(r8), intent(in) :: qcic ! cloud liquid
  real(r8), intent(in) :: qiic ! cloud ice
  real(r8), intent(in) :: qric ! rain
  real(r8), intent(in) :: qsic ! snow

  ! Size parameters
  ! rain
  real(r8), intent(in) :: lamr
  real(r8), intent(in) :: n0r
  ! snow
  real(r8), intent(in) :: lams
  real(r8), intent(in) :: n0s

  ! Output tendencies
  real(r8), intent(out) :: pre
  real(r8), intent(out) :: prds

  real(r8) :: qclr   ! water vapor mixing ratio in clear air
  real(r8) :: ab     ! correction to account for latent heat
  real(r8) :: eps    ! 1/ sat relaxation timescale

  real(r8) :: dum

  ! set temporary cloud fraction to zero if cloud water + ice is very small
  ! this will ensure that evaporation/sublimation of precip occurs over
  ! entire grid cell, since min cloud fraction is specified otherwise
  if (qcic+qiic < 1.e-6_r8) then
     dum = 0._r8
  else
     dum = lcldm
  end if

  ! only calculate if there is some precip fraction > cloud fraction

  if (cldmax > dum) then

     ! calculate q for out-of-cloud region
     qclr=(q-dum*qvl)/(1._r8-dum)

     ! evaporation of rain
     if (qric >= qsmall) then

        ab = calc_ab(t, qvl, xxlv)
        eps = 2._r8*pi*n0r*rho*Dv* &
             (f1r/(lamr*lamr)+ &
             f2r*(arn*rho/mu)**0.5_r8* &
             sc**(1._r8/3._r8)*gamma_half_br_plus5/ &
             (lamr**(5._r8/2._r8+br/2._r8)))

        pre = eps*(qclr-qvl)/ab

        ! only evaporate in out-of-cloud region
        ! and distribute across cldmax
        pre=min(pre*(cldmax-dum),0._r8)
        pre=pre/cldmax
     else
        pre = 0._r8
     end if

     ! sublimation of snow
     if (qsic >= qsmall) then
        ab = calc_ab(t, qvi, xxls)
        eps = 2._r8*pi*n0s*rho*Dv* &
             (f1s/(lams*lams)+ &
             f2s*(asn*rho/mu)**0.5_r8* &
             sc**(1._r8/3._r8)*gamma_half_bs_plus5/ &
             (lams**(5._r8/2._r8+bs/2._r8)))
        prds = eps*(qclr-qvi)/ab

        ! only sublimate in out-of-cloud region and distribute over cldmax
        prds=min(prds*(cldmax-dum),0._r8)
        prds=prds/cldmax
     else
        prds = 0._r8
     end if

  else
     prds = 0._r8
     pre = 0._r8
  end if

end subroutine evaporate_sublimate_precip

! bergeron process - evaporation of droplets and deposition onto snow
!===================================================================

elemental subroutine bergeron_process_snow(t, rho, dv, mu, sc, qvl, qvi, asn, &
     qcic, qsic, lams, n0s, bergs)

  real(r8), intent(in) :: t    ! temperature
  real(r8), intent(in) :: rho  ! air density
  real(r8), intent(in) :: dv   ! water vapor diffusivity
  real(r8), intent(in) :: mu   ! viscosity
  real(r8), intent(in) :: sc   ! schmidt number
  real(r8), intent(in) :: qvl  ! saturation humidity (water)
  real(r8), intent(in) :: qvi  ! saturation humidity (ice)

  ! fallspeed parameter for snow
  real(r8), intent(in) :: asn

  ! In-cloud MMRs
  real(r8), intent(in) :: qcic ! cloud liquid
  real(r8), intent(in) :: qsic ! snow

  ! Size parameters for snow
  real(r8), intent(in) :: lams
  real(r8), intent(in) :: n0s

  ! Output tendencies
  real(r8), intent(out) :: bergs

  real(r8) :: ab     ! correction to account for latent heat
  real(r8) :: eps    ! 1/ sat relaxation timescale

  if (qsic >= qsmall.and. qcic >= qsmall .and. t < tmelt) then
     ab = calc_ab(t, qvi, xxls)
     eps = 2._r8*pi*n0s*rho*Dv* &
          (f1s/(lams*lams)+ &
          f2s*(asn*rho/mu)**0.5_r8* &
          sc**(1._r8/3._r8)*gamma_half_bs_plus5/ &
          (lams**(5._r8/2._r8+bs/2._r8)))
     bergs = eps*(qvl-qvi)/ab
  else
     bergs = 0._r8
  end if

end subroutine bergeron_process_snow

!========================================================================
!UTILITIES
!========================================================================

pure function no_limiter()
  real(r8) :: no_limiter

  no_limiter = transfer(limiter_off, no_limiter)

end function no_limiter

pure function limiter_is_on(lim)
  real(r8), intent(in) :: lim
  logical :: limiter_is_on

  limiter_is_on = transfer(lim, limiter_off) /= limiter_off

end function limiter_is_on

end module micro_mg_utils

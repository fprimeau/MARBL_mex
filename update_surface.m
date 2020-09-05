function [surface, interior] = update_surface ( surface, interior, skip )

% global marbl_log

% use midpoint of time step to update surface

surface.tracer = (interior.tracer_old(:,1)' +interior.tracer(:,1)') ./2;
surface.state  = (interior.state_old (:,1)  +interior.state (:,1) ) ./2;

mex_marbl_driver ( 'restore_surface_flux_forcings',    surface.forcing );
mex_marbl_driver ( 'restore_surface_flux_saved_state', surface.state   );
mex_marbl_driver ( 'restore_tracers_at_surface',       surface.tracer  );

% compute change using MARBL.
% sign of flux is such that negative values are OUT water layer.

% marbl_log = mex_marbl_driver('surface_flux_compute');
mex_marbl_driver('surface_flux_compute');

surface.flux  = mex_marbl_driver ( 'surface_fluxes' );
surface.sfo   = mex_marbl_driver ( 'sfo' );
surface.diag  = mex_marbl_driver(  'surface_flux_diags' );
surface.state = mex_marbl_driver ( 'surface_flux_saved_state' );


% FIXME: check flux for NaN and other errors...
% FIXME:    ...record (bad) tracer and surface_flux_forcing?
% FIXME: update global averages?

% Some updates are just startup spikes, like when CISO=1 and n=1.
% To avoid lots off by one bugs, just zero flux 0, but keepeverything else
% so we have same number of samples of flux, diags, etc.

if skip
    surface.flux = surface.flux *0;
end

% Need to use depth to convert surface flux to volume rate of change.
%
% Units of -METERS- are used in matlab domain.
%   convert depth here to units (cm?) used in MARBL

global MARBL_depth_unit
surface.tendency = surface.flux' /( interior.domain.delta_z(1) *MARBL_depth_unit);

end % update_surface
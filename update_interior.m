function interior = update_interior ( interior )


% global marbl_log

mex_marbl_driver ( 'restore_interior_tendency_forcings',    interior.forcing );
mex_marbl_driver ( 'restore_tracers',                       interior.tracer  );
mex_marbl_driver ( 'restore_interior_tendency_saved_state', interior.state   );

% compute tendency using MARBL

% marbl_log = mex_marbl_driver('interior_tendency_compute');
mex_marbl_driver('interior_tendency_compute');

% FIXME: check change for NaN and other errors...
% FIXME:    ...record record (bad) tracer and surface_flux_forcing
% FIXME: update global averages?

interior.tendency = mex_marbl_driver ( 'interior_tendencies' );
interior.diag     = mex_marbl_driver ( 'interior_tendency_diags');
interior.state    = mex_marbl_driver ( 'interior_tendency_saved_state' );

end % update_interior
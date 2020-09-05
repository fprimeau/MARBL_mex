function [surface, interior]= default_states ( surface, interior )

% Note: surface and interior have "level" and tracer indices swapped...
% Read correct sized array of garbge from MARBL, initialize that, 
% write it back...

surface.state  = mex_marbl_driver('surface_flux_saved_state');
interior.state = mex_marbl_driver('interior_tendency_saved_state');

% surface_flux_saved_state      has 2 states but just one level; ph, alt_ph
% interior_tendency_saved_state has 2 states for each level

surface.state(:)    = 8.045; % (pH)
interior.state(:,:) = 8.2; % (pH)

% initialize MARBL

mex_marbl_driver ( 'restore_surface_flux_saved_state',      surface.state );    
mex_marbl_driver ( 'restore_interior_tendency_saved_state', interior.state );

end % init_states.m

function [tracer_cnt, surface, interior, ioerr] = init_marbl (s, dt, nstep, restart)

global MARBL_depth_unit
global marbl_log   % This is log that MARBL would write to stdout
% global time_series % This is time series of tracers, diags and states
global lciso_on

% read settings file of choice (if any)

disp(' ')
disp('init_marbl.m: ')
ioerr = mex_marbl_driver('read_settings_file', 'default_chemistry.input');
% FIXME: writing the chemistry file fails becasue varcount = 0 in F90 code...
% ioerr = mex_marbl_driver('write_settings_file', 'new.input');

% FIXME: something as simple as changing a setting... sigh
% FIXME: Must hack out "ciso_on" line in defaults file if we set it here...
% FIXME: should read file or better ask MARBL for value of lciso_on
if lciso_on 
    mex_marbl_driver('put setting', 'ciso_on = .true.'); end

% change any pre-init values from those in settings file (if any)

% number of interior levels
%
% FIXME: if roughly 1,000 or more levels, when printing diags in MARBL F90
% "subroutine jj_print_diag", F90 variable "msg" gets so long that Matlab
% truncates it in cmd window. However, data in diag matrix appears to
% transfer correctly. Hence, only a factor if printing diags with 386x1000
% columns from F90. Not realistic issue IMHO...

% use a really simply ocean depth layer model; aka MARBL "domain"
% nz              = round(bottom_depth /surface_thickness);
nz              = 1000;
nz              = 100
bottom_depth    = 4370; % (m) euphotic depth, roughly
bottom_depth    = 2*4370; % (m) euphotic depth, roughly
interior.domain = init_domain (s, nz, bottom_depth, 'ocean');

% FIXME: comments in MARBL code appear to be wrong about what  units are
% initialize a single MARBL instance with a single column in it
%
% ---> MARBL source code says it uses units of km for depths
% But results indicate that only cm works!! Not (m), not (km)! Look at
% interior diag #151 to be sure there is no light below about 1,000 feet...
%
% e.g. small phyto growing at depth of 10km is obviously wrong.
%
% Whatever is correct, in Matlab code we use meters!


% convert from (m) used here to MARBL units (cm?)

MARBL_depth_unit = 1e+2;    % 1 meter here in units used by MARBL

[ marbl_log, tracer_cnt, ...
    interior.forcing_cnt, interior.diag_cnt, ...
    surface.forcing_cnt, surface.diag_cnt ] ...
    = mex_marbl_driver ( 'init', ...
    interior.domain.delta_z *MARBL_depth_unit, ...
    interior.domain.zw      *MARBL_depth_unit, ...
    interior.domain.zt      *MARBL_depth_unit );

mex_marbl_driver('set_depth', interior.domain.kmt)


% initialze the forcing first so we can use the CISO values to init tracers
[surface, interior]    = default_forcings ( surface, interior );

% initialize everything to something very roughly like SMOW
% 
% FIXME: try to create useful IC for interior, surface tracers, states...
%   run 1 YEAR simulation starting with defaults. Then use final values as 
%   a "more accurate" initial condition, and then read those in rather than
%   spin up sim for 100K iterations every run

if restart == 0
    disp(' '); disp('init_marbl.m: using default tracers for IC...');disp(' '); 
    [surface, interior] = default_states   ( surface, interior );
    [surface, interior] = default_tracers  ( surface, interior );
else
    disp(' ');disp('Reading restart files for tracer IC...');
    my_domain = interior.domain; % DEBUG only
    my_diag_cnt = interior.diag_cnt; %DEBUG only
    % FIXME: share non CISO parts of tracers, or put both in 1 file
    if lciso_on == 1
%         load(strcat('ciso.','surface.mat') , 'surface');
%         load(strcat('ciso.','interior.mat'), 'interior');
        load('ciso.mat', 'surface');
        load('ciso.mat', 'interior');
    else
%         load('surface.mat' , 'surface');
%         load('interior.mat', 'interior');
        load('no_ciso.mat', 'surface');
        load('no_ciso.mat', 'interior');
    end
    % make sure stored values at least are on same grid, etc...
    if ~isequal(my_domain, interior.domain) 
        error('Stored intial condition does not match current grid size');
    elseif ~isequal(my_diag_cnt,interior.diag_cnt)
        error('Stored intial condition does not match current CISO');
    end
end

% move values over to MARBL

mex_marbl_driver ( 'restore_tracers_at_surface', surface.tracer  );
mex_marbl_driver ( 'restore_tracers',            interior.tracer );

% No initialization needed for tendency, diags, and fluxes which are MARBL
% outputs but we read them to get dimensions, then pre-allocate log, so we
% can save results quickly...

interior.tracer_name   = tracer_names ( lciso_on );
surface. tracer_name   = tracer_names ( lciso_on );
interior.tracer_unit   = tracer_units ( lciso_on );
surface. tracer_unit   = tracer_units ( lciso_on );

interior.diag_name     = diag_names   ( 'interior_tendency_diags', interior.diag_cnt );
surface. diag_name     = diag_names   ( 'surface_flux_diags'     , surface. diag_cnt );
% FIXME: these are hacked to be matrix, but diags in MARBL are complex structs...
interior.diag          = mex_marbl_driver ( 'interior_tendency_diags' );
surface. diag          = mex_marbl_driver ( 'surface_flux_diags' );
interior.tendency      = mex_marbl_driver ( 'interior_tendencies' );
surface.flux           = mex_marbl_driver ( 'surface_fluxes' );
surface.sfo            = mex_marbl_driver ( 'sfo' );

interior.forcing_name  = interior_forcing_names ();
surface.forcing_name   = surface_forcing_names  ();

% keep time series of values, everything for every time step

init_time_series(nstep, dt, lciso_on, interior, surface);


% save setting to a file, (if desired)
% [ioerr] = mex_marbl_driver('write_settings_file', 'defaults.input');


% print_from_marbl_to_matlab(); Useful but huge amount of text


end % init_marbl.m

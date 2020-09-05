clear variables
% clear mex % MATLAB "analyze code" says this is slow, but seems to avoid
% strange hangs and crashes if MEX doesnt end gracefully on previous run.
clear

% cd '~/Desktop/UCI/MARBL/1D_MARBL_demo'
addpath ("plotting", "utils")

% Big picture:
%
% The MARBL biogeochemistry model is a 22,000 line FORTRAN 90 package that
% computes marine chemical and biological concentrations and is used for
% climate simulation. FORTRAN is fast but uncomfortable environment for
% software development.
%
% This code is a trivial one dimensional ocean simulation model used mainly
% to debug the very opaque Matlab and F90 code required to create a Matlab
% interface to MARBL. It is not a good GCM, but it does demonstrate the
% initialization, update, and logging needed to use MARBL in a more
% realistic GCM.
%
% This code is setup to use MARBL with a single water column. MARBL can be
% setup to use a single instance with multiple water columns. This
% interface or "MEX" code could (probably) be extended in a straightforward
% way to do that, but my opinion is would probably be just as efficient to
% deal with the water columns here and leave the difficult to understand
% and debug MARBL as is. The trade off is this: in a multithreaded
% multiprocessor environment, MARBL runs fast. However most updates require
% the result to be transferred to the GCM (presumably in Matlab) every time
% step. It would be slightly more efficient to transfer the data for
% multiple columns, but I doubt it will be much. IMHO.
%
% The most confusing things about MARBL is (IMHO)
%
% 1.
%
% The MARBL source code contains many, many references to the "surface
% layer". I was thinking, "surface film". That is completely wrong. The
% correct way to think of MARBL is a layered ocean which is updated using
% a couple of functions. THERE iS NO SURFACE LAYER. The references to the
% surface layer reflect (I guess) someone adding a calculation for air-sea
% gas flux to what was an ocean model. They wanted to do this mid cycle in
% the time steps. The limited documentation adds to the confusion by IMHO
% continuing this idea. It is very simple, the update to the ocean layers
% is calculated every time step. The top most layer of the updated ocean is
% used as the "surface layer". OCIM appears to use the time average of
% current and previous time step of top most layer.
%
% 2.
%
% The MARBL code doesn't have a valid initial/default condition. So as many
% as 46 tracer values have to be initialized with the "correct" tracers.
% The MARBL code while fast is pretty brittle and will crash, or worse,
% just give incorrect results if e.g. the dust flux is zero, biota isn't
% initialized non-zero, and seemingly a million other things. 
% IMPORTANT: some advection (mixing) is required or the results will be 
% both wrong but close enough to be really confusing. The values in 
% default_tracers.m are completely ad hoc, but not unphysical. The entire 
% ocean is initialized with them.
%
% The -INTENTION- of the defaults is just to prove the MEX for MARBL code
% does work correctly and init every tracer necessary. If the default
% tracers, states, toy forcing, and advection are used to spin up the model
% for 100 yr with a 15 min time step, the results are plausible, not
% correct!

global lciso_on             % set this to 1 for isotope to be included

% FIXME: Matlab does NOT do call by reference, do NOT pass HUGE log as arg
% global time_series          % Time series of tracers, diags and states
global const
const.sec_h = 60 * 60;
const.sec_d = const.sec_h * 24;
const.sec_y = const.sec_d * 365;


lciso_on = 1;               % set this to 1 for isotope to be included
restart  = 0;


% Run simulation of 100 years with 60 minute time step. Takes about 20m.
dt = const.sec_h /3;
tot_t = round ( 1 *const.sec_y );
nstep = round ( tot_t /dt );
disp(['nstep: ', num2str(nstep)]);
if (true)
    % DEBUG: just run a few thousand loops. Really short runs do not have
    % enough data for some of the plots in "smal_plots.m, so it crashes.
    % Need at least a few weeks of data to make all the plots...
%     nstep = round ( 30* const.sec_d /dt );
    nstep = round( 2 *1000 );
%     nstep = 10* 1000;
%     nstep = round ( 30* const.sec_d /dt );
    tot_t = dt*nstep;
    disp(' '); disp 'Hacked nstep!!!'; disp(nstep);
end
disp(['time step is ', num2str(dt *60/const.sec_h), ...
    ' (m), simulating ', num2str(tot_t /const.sec_y, '%1.1f'), ...
    ' (y), hence ', num2str(round(nstep/1000,1)), ' (k) iterations'])


% Use "T" the transport matrix from Francois 1_d driver demo...
%
% direction of water flow is -UP- towards surface, +z, aka "LESS deep"
%   w>0. The sign of w is very confusing, see below :-)
%
% vertical advection
a = 6371e3; % Earth radius
area = 0.71*4*pi*a^2; % surface area of ocean
f = 20e6; % (m^3/s) deep water formation rate
w = f/area; % (m/s)
% T = transport (interior.domain, w);
% vertical diffusivity
% enhanced diffusivity in mixed layer
mixed_layer_thickness = 100.
av = @(z) (z>-mixed_layer_thickness).*(0.1)+4e-4;

% need to be carful to have same "s" the stretching factor everywhere

s = 1. ;  % grid stretching factor

[~, surface, interior, ~] = init_marbl (s, dt, nstep, restart);
T = ad(s, interior.domain.zw(end), interior.domain.nz, av, w);

FT = mfactor( speye(interior.domain.nz) +dt*T);

tic
for n=1:nstep
    
    %    save tracers and state before update.
    %    use to get mid point of time step for surface flux update
    
    interior.state_old  = interior.state;
    interior.tracer_old = interior.tracer;
    
    % update forcing of interior and surface
    
    [surface, interior] = forcing ( dt*(n-1), surface, interior );
    
    % integrate interior forcing
    
    interior = update_interior ( interior );
    
    % Calculate surface flux and update the interior with it
    
    % FIXME: first call to surface update can be garbage. Uninitialized vars?
    if (n == 1)
        ignore = 1;
        [surface, interior] = update_surface ( surface, interior, ignore);
    end
    
    [surface, interior] = update_surface ( surface, interior, 0);
    
    interior.state (:,1) = surface.state;
    S = interior.tendency;
    S(:,1) = S(:,1) +surface.tendency;
    
    % advection and diffusion using transport matrix
    % FIXME: use midpoint like we did with surface flux?
    
    interior.tracer = mfactor(FT, (interior.tracer +dt*S)')';
    
    % record tracers at all levels at all times
    
    update_log (n, surface, interior);
end


elapsedTime = toc;
tot_y = tot_t/const.sec_y;
disp(' ');
disp(['Runtime: ', num2str(elapsedTime, '%1.0f'),' (s) or ', num2str(elapsedTime/60, '%1.1f'), ' (m)'])
disp(['Runtime per sim year: ', num2str(elapsedTime/60/tot_y, '%1.1f'), ' (min/y_sim)'])
disp(['Runtime per iteration: ',           num2str(elapsedTime/nstep*1000, '%1.1f'),                    ' (ms)'])
disp(['Runtime per iteration per level: ', num2str(elapsedTime/nstep/interior.domain.nz*1000, '%1.2f'), ' (ms)'])

% FIXME: as "learning tool" this plot routine makes literally 100's plots
% FIXME: It takes plots() several minutes to draw all of them...
% FIXME: edit the "plots.m" code to make a few plots that are actually of
% interest.

% small_plots( surface, interior );
% % plots( surface, interior );
% % dn = 'figs_100y';
% dn = 'junk_figs';
% [~,~] = mkdir(dn);
% saveFigs(dn);


% FIXME: do NOT shutdown MARBL until Matlab is done printing
%   -> need a few seconds for Matlab to finish updating command window if
%   printing a huge number of levels...
%
% print_from_marbl_to_matlab(); % should print to a file.
% mex_marbl_driver('print_sfo');                    % DEBUG
disp('Shutting down MEX in 3 (s)...'); pause(3);
mex_marbl_driver('shutdown');
disp('...success!');

% global time_series         % This is time series of tracers, diags and states

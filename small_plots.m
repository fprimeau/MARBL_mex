function small_plots ( surface, interior )

tic;

global const
global time_series         % This is time series of tracers, diags and states

dt    = time_series.dt;
nstep = time_series.nstep;
tot_t = dt*nstep;

my_time = 70.;        % e.g. noon day 70 or biggest we have
my_time = my_time*const.sec_d;      % e.g. day converted to sec
my_time = tot_t;

% FIXME: do NOT pick midnight (aka 0 second of day) if you graph PAR
% my_time = tot_t -0.4 *const.sec_d;

dx = const.sec_d/dt;
t = (0:size(time_series.tracer, 1)-1) /dx;

nameUnits = strcat(interior.tracer_name," ",interior.tracer_unit);

% surface tracers

fig = 1;
idx = 1:size(interior.tracer_name');        % plot -ALL- the tracers???
myTitle = 'Surface Tracers v. Time (d)';
myData = time_series.tracer;
fig = plot_log(fig, myTitle, t, myData, nameUnits(idx), idx, false);

% surface diags

idx = 1:size(surface.diag_name);
myTitle = 'Surface Diags v. Time (d)';
myData = time_series.surface_diags;        % plot -ALL- the diags???
fig = plot_log(fig, myTitle, t, myData, surface.diag_name(idx), idx, false);

% Plot interior tracer time series

plot_layer = min(interior.domain.kmt, 3);
% plot_layer = interior.domain.kmt;

idx_int = 1:size(interior.tracer_name');
fig = plot_interior_tracers(fig, tot_t, plot_layer, idx_int, interior);

idx = [ 135, 139, 151, 149, 189, 192, 196, 202, 203, 213, 212, 218, 233 ];
fig = plot_interior_diags(fig, my_time, plot_layer, idx, 'Sample of', interior);

idx = [ 30, 23, 24, 338, 340, 179, 180, 174 ];
fig = plot_interior_diags(fig, my_time, plot_layer, idx, 'POC', interior);
% fig = fig-1;close(fig); 

idx = 90:116;
fig = plot_interior_diags(fig, my_time, plot_layer, idx, 'Diaz Diags #1', interior);

idx = 249:265;
fig = plot_interior_diags(fig, my_time, plot_layer, idx, 'Diaz Diags #2', interior);

idx = [ 327:334 369:372 385:386 ];
fig = plot_interior_diags(fig, my_time, plot_layer, idx, 'Diaz Diags #3', interior);

% plot_layer = 1;
idx = [  277:288,301,303,305,306,309,311,313,316,319,321,324,327,329,332,337,338,373,374,377,379,381,383,385];
fig = plot_interior_diags(fig, my_time, plot_layer, idx, '13C', interior);

myTitle = 'Glitch in 13CO2 flux when pCO2 = atm (d)';
idx = [ 13, 28:31, 37:40];
myData = time_series.surface_diags(:,idx);
fig = plot_log(fig, myTitle, t, myData, surface.diag_name(idx), idx, false);

myTitle = 'SFO';
idx = 1:4;
myData = time_series.sfo(:,idx);
myName = surface_flux_output_names()';
fig = plot_log(fig, myTitle, t, myData, myName,idx, false);

% Make a 3D plot of a small amount of time, say 7 days, starting day 10...
n_days = 7;
n_cnt = floor(n_days* const.sec_d/dt);
n_start = round(min(dt, 10* const.sec_d)/dt);
n_range = n_start:n_start +n_cnt -1;

% -DIAG- #151 = PAR_avg = PAR at depth
% great way to prove depths are in cm
small_data = time_series.diag(n_range,:,:);
idx = 151;
fig = plot3dDiagTimeSeries  ( fig, small_data, interior,idx, n_cnt, dt);

% Interior tracer #18 zoo plankton C
% Great way to show Zoo do -NOT- move vertically during the day/night
small_data = time_series.tracer(n_range,:,:);
idx = 18;
fig = plot3dTracerTimeSeries( fig, small_data, interior, idx, n_cnt, dt);

% Interior tracer #46 diaz C14 does show diurnal cycle
idx = 46;
fig = plot3dTracerTimeSeries( fig, small_data, interior, idx, n_cnt, dt);

% Interior tracer #4
idx = 4;
fig = plot3dTracerTimeSeries( fig, small_data, interior, idx, n_cnt, dt);
% "" but legible...
figure(fig); 
plot_layer = 3;
foo = small_data(:,idx,plot_layer);
t_unit = const.sec_d;n_decimate = 1;
t = decimate ( ( n_range ) *dt/t_unit, n_decimate );
tracer_name  = interior.tracer_name(idx);
global lciso_on
unit = tracer_units(lciso_on);
plot(t,foo);
title("Tracer #"+idx+" "+tracer_name+" "+'plot_layer #'+plot_layer+ ...
    ' depth '+interior.domain.zt(plot_layer)+" (m)",'Interpreter', 'none');
xlabel('time (days)')
ylabel(unit(idx))
fig = 1+(fig); 


elapsedTime = toc;
disp(['small_plots.m runtime: ', num2str(elapsedTime, '%1.0f'), ' (s)'])

end % small_plots.m

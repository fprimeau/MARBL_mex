function plots ( surface, interior )

tic;

global const
global time_series         % This is time series of tracers, diags and states

dt    = time_series.dt;
nstep = time_series.nstep;
tot_t = dt*nstep;

my_time = min( 70.5, tot_t);     % e.g. day 6 or biggest we have
my_time = my_time*const.sec_d;  % e.g. day converted to sec
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

idx_int = 1:size(interior.tracer_name');
fig = plot_interior_tracers(fig, tot_t, 7, idx_int, interior);

% return

% Plot -ALL- interior diags at given level, great for debug...

plot_layer = min(interior.domain.kmt, 7);
% plot_depth = interior.domain.zt(plot_layer);

% ...but interior has way to many (386) to actually plot that.
% Not practical to plot -ALL- 386 interior diags, pick a few...
% select a few diags that are NOT function of depth, and many are not!

% idx = [ 135, 5, 6, 7, 20, 24, 25, 30, 210, 211, 36, 38, 69, 71, 80, 96, 119, 139, 174, 151, 224, 225, 226, 241, 242, 243]; % scaler or use surface value
idx = [ 135, 139, 151, 173, 174, 175, 149, 189, 202, 212, 213, 218, 233 ];
fig = plot_interior_diags(fig, my_time, plot_layer, idx, '', interior);

% surface and interior forcing

fig = 20;
fig = plot_forcing(fig, my_time, surface, interior );

% return

% zoo interior diag

idx_zoo = [ 51 52 80 81 107 108 119:134 226 243 260 269:276 288 300 151]; % all zoo diags except ciso
global lciso_on
if lciso_on == 1
    idx_zoo = [ idx_zoo 358 377:378 ]; % all ciso zoo diags
end
fig = 30;
fig = plot_interior_diags(fig, my_time, plot_layer, idx_zoo, 'zoo', interior);

% sp interior diag

idx_sp = [ 32:60 215:231 295:299 305:308 151 ]; % all sp diags except ciso
if lciso_on == 1
    idx_sp = [ idx_sp 309:318 359:363 379:382 ]; % all ciso sp diags
end
fig = plot_interior_diags(fig, my_time, plot_layer, idx_sp , 'sp', interior);

% diat interior diag

idx_diat = [ 61:89 232:248 ]; % all diat except ciso
if lciso_on == 1
    idx_diat = [ idx_diat 61:89 232:248 319:326 364:368 383:384 151]; % all ciso diat
end
fig = plot_interior_diags(fig, my_time, plot_layer, idx_diat , 'diaT', interior);

% diaz interior diag

% idx_diaz = [ 90:116 249:265 327:334 369:372 385:386 ];   % all diaz
idx_diaz = [ 90:116 249:265 ];               % all diaz that change except ciso
if lciso_on == 1
    idx_diaz = [ 327:334 151 ];               % all ciso diaz that change
end
fig = plot_interior_diags(fig, my_time, plot_layer, idx_diaz , 'diaZ', interior);


% CISO permil interior diags

if lciso_on == 1
    fig = 70;
    fig = plot_CISO_diags(fig, tot_t, plot_layer, surface, interior);
end

% Plot selected tracers and diags as function of Time and Depth
% list to plot is in plot_tracers_3D.m

firstFig = 100;
fig = plot_3D(firstFig, interior);


elapsedTime = toc;
disp(['plots.m runtime: ', num2str(elapsedTime, '%1.0f'), ' (s)'])

end % plots.m
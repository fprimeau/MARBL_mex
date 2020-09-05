function fig = plot_CISO_diags(fig, my_time, plot_layer, surface, interior)
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

idx_surf = 31:43;
idx_int = [ 286, 287, 288, 298, 299, 300, 315, 316, 317, 323, 324, 325, 331, 332, 333, 334, 335, 336];

global lciso_on             % set this to 1 for isotope to be included
if lciso_on == 0            % can not plot what we don't have: CISO vals
    return
end


global const
global time_series         % This is time series of tracers, diags and states

dt      = time_series.dt;
nstep   = time_series.nstep;
% tot_t   = dt*nstep;
dx      = const.sec_d/dt;
t       = (0:size(time_series.tracer, 1)-1) /dx;

plot_depth = interior.domain.zt(plot_layer);



% plot data v. time
myTitle = sprintf('CISO Interior Diags v. Time(d) @level %d, depth = %d(m)', ...
    plot_layer, round(plot_depth));
myData = time_series.diag(:, idx_int, plot_layer);
fig = plot_log(fig, myTitle, t, myData, interior.diag_name(idx_int), idx_int, false);

% plot data v. depth
iter = min(round(my_time/dt),nstep);
myTitle = sprintf('CISO Interior Diags v. Depth (m) @day %d, iteration = %d', round(my_time/const.sec_d), iter);
myData = squeeze(time_series.diag(iter, idx_int, :))';
myDepth = interior.domain.zt;
fig = plot_log(fig, myTitle, myDepth, myData, interior.diag_name(idx_int), idx_int, true);

% plot data v. time
myTitle = 'CISO Surface Diags v. Time (d)';
myData = time_series.surface_diags(:, idx_surf);        % plot -ALL- the diags???
fig = plot_log(fig, myTitle, t, myData, surface.diag_name(idx_surf), idx_surf, false);

end


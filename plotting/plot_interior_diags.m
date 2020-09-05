function myFig = plot_interior_diags(myFig, my_time, plot_layer, idx, name, interior)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

global const
global time_series         % This is time series of tracers, diags and states

dt      = time_series.dt;
nstep   = time_series.nstep;
% tot_t   = dt*nstep;
dx      = const.sec_d/dt;
t       = (0:size(time_series.tracer, 1)-1) /dx;

plot_depth = interior.domain.zt(plot_layer);

iter = max( 1, min(round(my_time/dt),nstep));

% plot data v. time

myTitle = sprintf(append(name,' Interior Diags v. Time(d) @level %d, depth = %d(m)'), ...
    plot_layer, round(plot_depth));
myData = time_series.diag(:, idx, plot_layer);
plot_log(myFig, myTitle, t, myData, interior.diag_name(idx), idx, false);

myFig = myFig +1;

% plot data v. depth

myTitle = sprintf(append(name,' Interior Diags v. Depth (m) @day %G, iteration = %d'),...
    round(my_time/const.sec_d,2), iter);
myData = squeeze(time_series.diag(iter, idx, :))';
myDepth = interior.domain.zt;
plot_log(myFig, myTitle, myDepth, myData, interior.diag_name(idx), idx, true);

myFig = myFig +1;

end

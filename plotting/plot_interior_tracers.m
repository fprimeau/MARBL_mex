function myFig = plot_interior_tracers(myFig, my_time, plot_layer, idx, interior)
%UNTITLED4 Summary of this function goes here
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

nameUnits = strcat(interior.tracer_name," ",interior.tracer_unit);

% plot data v. time

myTitle = sprintf('Interior Tracers v. Time(d) @level %d, depth = %d(m)', ...
    plot_layer, round(plot_depth));
myData = time_series.tracer(:, idx, plot_layer);
myFig = plot_log(myFig, myTitle, t, myData, nameUnits(idx), idx, false);


% plot data v. depth

myTitle = sprintf('Interior Tracers v. Depth (m) @day %G, iteration = %d', ...
    round(my_time/const.sec_d,2), iter);
myData = squeeze(time_series.tracer(iter, idx, :))';
myDepth = interior.domain.zt;
myFig = plot_log(myFig, myTitle, myDepth, myData, nameUnits(idx), idx, true);

end

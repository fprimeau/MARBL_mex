function myFig = plot_forcing(myFig, my_time, surface, interior)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

% plot data v. depth

global const
global time_series         % This is time series of tracers, diags and states

dt      = time_series.dt;
nstep   = time_series.nstep;
% tot_t   = dt*nstep;
dx      = const.sec_d/dt;
t       = (0:size(time_series.tracer, 1)-1) /dx;

iter    = max( 1, min(round(my_time/dt),nstep));

idx = 1:size(surface.forcing,2);
myTitle = 'Surface Forcing v. Time (d)';
myData = time_series.s_forcing;
myFig  = plot_log(myFig, myTitle, t, myData, surface.forcing_name(idx), idx, false);

idx = 1:6;
myTitle = sprintf('Interior Forcing v. Depth (m) @day %d, iteration = %d', round(my_time/const.sec_d), iter);
myData  = squeeze(time_series.i_forcing(iter, idx, :))';
myDepth = interior.domain.zt;
myFig  = plot_log(myFig, myTitle, myDepth, myData, interior.forcing_name(idx), idx, true);

myTitle = 'Interior Forcing v. Time (d)';
myData = time_series.i_forcing;
myFig = plot_log(myFig, myTitle, t, myData, interior.forcing_name(idx), idx, false);

end

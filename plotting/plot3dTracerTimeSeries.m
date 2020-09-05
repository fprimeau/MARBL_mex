
function fig = plot3dTracerTimeSeries(fig, tracer, interior, idx, nstep, dt)

global lciso_on

tracer_name  = interior.tracer_name(idx);
domain       = interior.domain;

figure(fig);
clf(fig)

% plot shows days

t_unit = 60*60*24; % s/day

% Decimate time from seconds into "day". 

% n_decimate = max( 1, round(nstep /t_unit) );

% That still might not be enough to make plot legible
% Decimate ~ "nz"

n_decimate = 1;
% n_decimate = max( 1, round(nstep/domain.nz) );

t = decimate ( ( 1:nstep ) *dt/t_unit, n_decimate );

% depths from "middle of layer" to top. Dimension "nz"

depth = ( domain.zt ); % (m)

% Make the "meshgrid" that "surf" plot needs

[X,Y] = meshgrid ( t, depth );

% Selcted the tracer of interest

data = squeeze ( tracer(:, idx,:) )';
unit = tracer_units(lciso_on);

% decimate,  data to match time
% 
% decimation filters the data, so it is a little slow
%
% downsample does not filter, it is fast, could alias
% 
% Z = downsample ( data', n_decimate )';

Z = zeros(domain.nz, nstep);
for i = 1:domain.nz
    Z(i,:) = decimate ( data (i, :), n_decimate );
end

surf(X,Y,Z, 'EdgeColor', 'none');

hold on
view(3)
xlabel('time (days)')
ylabel('depth(m)')
title("Tracer #"+idx+" "+tracer_name+" "+unit(idx), 'Interpreter', 'none');
colorbar;

fig = fig+1;

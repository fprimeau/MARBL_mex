function interior = advec_naive(dt, interior, skip)
%UNTITLED Summary of this function goes here

% direction of water flow is -UP- towards surface, +z, aka "LESS deep"
%   w>0. The sign of w is can get very confusing, see below :-)

% vertical advection
a = 6371e3; % Earth radius
area = 0.71*4*pi*a^2; % surface area of ocean
f = 20e6; % (m^3/s) deep water formation rate
w = f/area; % (m/s)

% first time step results are undefined. code is simpler if just hack w.
if skip
    w = 0;
end

% material derivitave D()/dt = d()/dt +dot(v,gradient(tracer))

% FIXME: is there explicit time dependency, e.g. interior forcing?
partial_wrt_time = 0;

% need gradient, but Matlab function replicates endpoints because endpints 
% of gradient.m are not from periodic boundary condition.

R  = interior.tracer(:,:);

% Matlab diff is almost what we need. 
dTr = diff(R, 1, 2);     

% oh no! we just lost the last column... Use periodic boundary condition.
%
% That lost column is actually very convienent. To simulate deep water
% formation from surface water, just use surface water in a first
% difference.
%
%   The "trick" is just use the diff of top and bottom as last column...

dTr = [ dTr R(:,1)-R(:,end) ]; 

% This is material derivative we wanted...

dz = interior.domain.delta_z;   % thickness of layer

tendency = partial_wrt_time +w*dt*dTr./dz;
% dMass_naive = dz*tendency'

interior.tracer = interior.tracer +tendency;


end % advec

function domain = init_domain(s, nz, max_depth, name)

% number of ocean levels not necessarily equally spaced in ocean interior

domain.nz = nz;     % fiddle this to get nice layer thickness...

% This is level of the bottom, not necessarily the max in grid because in
% Marble it possible to have multiple water columns in one MARBL instance.

domain.kmt = nz;

% Whew, now define MARBL layers: thickness, depth, center

% FIXME:
% src/marbl_interface_public_types.F90
%
% "delta z - different values for partial bottom cells"
%
% what does that mean...

[zt,zw,dzt,dzw] = zgrid_fp(nz,max_depth,s);
% Need to be careful with sign. Francois uses z, we want depth...
% Also we want bottom of layer, he uses top of layer
domain.zt  = -zt;        % mid point of layer
domain.zw  = -zw(2:end); % MARBL wants BOTTOM;
domain.delta_z = dzt;    % thickness of layer
domain.dzw = dzw(2:end); % distance from center to center;

grd.zt = zt;
grd.zw = zw;
grd.dzw = dzw;
grd.dzt = dzt;
domain.grd = grd;

disp(' ')
disp(['The ', name, ' has ', num2str(domain.nz), ' layers'])
disp(['Max ', name, ' depth is ', num2str(domain.zw(domain.nz),'%1.1f'), '(m)'])
disp(['Mean ', name, ' layer is ', num2str(mean(dzt),'%1.1f'), '(m) thick'])
disp(['Bottom at this lat/lon is layer #', num2str(domain.kmt), ...
    ', with max depth of ', num2str(domain.zw(domain.kmt),'%1.1f'), '(m)'])
disp(['Bottom layer thickness:  ', num2str(dzt(domain.kmt),'%1.1f'), ' (m)'])
disp(['Surface layer thickness: ', num2str(dzt(1),'%1.1f'), ' (m)'])

% ---> MARBL uses km for depths, we use (m) here in Matlab

end


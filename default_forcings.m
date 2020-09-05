function [ surface, interior ] = default_forcings ( surface, interior )

global lciso_on

% Note: surface and interior have "level" and tracer indices swapped...

% transfer from MARBL correct sized array of garbge, zero that...

% FIXME: interior_tendency_forcings is complicated struct, use matrix
% FIXME: till we can actually do the right thing

% mex_marbl_driver('print_surface_flux_forcings');
% mex_marbl_driver('print_interior_tendency_forcings');

surface.forcing  = zeros( 1, surface.forcing_cnt);
interior.forcing = zeros( interior.forcing_cnt, interior.domain.nz);

surface.forcing(1, :) = 1e-15;      % avoid divide by zero

% FIXME: hack some forcing to see if we can get a time series; e.g. CO2(t)

u10 = 3.3; % m/s
% convert u10 from m/s to MARBL units (cm/sec?)
global MARBL_depth_unit
u10 = u10 *MARBL_depth_unit;
surface.forcing(1, 1) =    (u10)^2; % u10_sqr(cm^2/s^2)  

sss = 35.5;
surface.forcing(1, 2) =        sss; % SSS(psu)

% Trichodesmium need warm, really warm water. Hack that in a few layers
surface.forcing(1, 3) =        27.; % SST(C) Trichodesmium need warm water

surface.forcing(1, 4) =          0; % Ice Fraction (unitless)   % FIXME

dust_flux             =        1.0; % Fe g/m^2/y
% convert dust flux from g/m^2/y to g/cm^2/s
global const
dust_flux = dust_flux/1e4/const.sec_y;

surface.forcing(1, 5) =  dust_flux; % Dust Flux (g/cm^2/s)

% MARBL assumes 3.5% Fe from dust. This forcing(:,6) is other Fe flux(es)
% FIXME: from where does this Fe come from???
surface.forcing(1, 6) =          0; % Iron Flux	unit: nmol/cm^2/s
surface.forcing(1, 7) = 4e-3/60/60; % NOx Flux (nmol/cm^2/s)    % doi:10.1016/j.dsr2.2006.05.008
surface.forcing(1, 8) = 5e-2/60/60; % NHy Flux (nmol/cm^2/s)    % doi:10.1016/j.dsr2.2006.05.008
surface.forcing(1, 9) =        1.0; % Air pressure (bar)
surface.forcing(1,10) =        420; % xco2(ppmv)
surface.forcing(1,11) = surface.forcing(1,10); % alt xco2

if lciso_on == 1
    surface.forcing(1,12) =  -9.0;  % d13c per mil
    surface.forcing(1,13) = 100.0;  % d14c per mil
end


% Interior forcing are surprinsingly complex. Some are scalers, some are
% vectors, one for each level and some are duplicated in the sruface
% forcings.
%
% The scalers forcings are simply repeated thru the ugly code to interface
% with MARBL. Someday, the interface should trasnfer a structure with fancy
% fields, but for now the interface is just a rectangular materix.
%
% Note: first 2 forcings are scaler, and actually surface properties!

% Dust Flux (g/cm^2/s)
interior.forcing(1, :) = surface.forcing(1, 5);


% Furthermore, the PAR or sunlight forcing is time varying and calculated
% everytime step. For simple debug, just have constant daylight.
interior.forcing(2, :) =   450; % Surface Shortwave (W/m^2)
% DEBUG: do NOT "turn on" sunlight until MARBL has spun up, That is done in
% "update_interior.m". Assume Time of Day is midnight
interior.forcing(2, :) =     0; % Surface Shortwave (W/m^2)


% Note: next 2 forcings, are set using surface forcing

% Trichodesmium need warm, really warm water. Hack that in a few layers
% near surface that are at SST, then use a smooth tanh shape in deep water
sst = surface.forcing(1, 3);
surface_thickness = 30; % (m) thermocline, roughly
botT = 3; % Potential T(C) at bottom

interior.forcing(3, interior.domain.zt <  surface_thickness) = sst;
idx = find( interior.domain.zt >= surface_thickness);
s = 0.3;
interior.forcing(3, idx) = sst +zgrid_fp(interior.domain.nz-idx(1)+1,sst-botT,s);

% Salinity
% FIXME: make this more realistic
interior.forcing(4, :) = 34.0 + (1.0)*(interior.domain.zt/1000); 
surface_thickness = 30; % (m) thermocline, roughly
interior.forcing(4, interior.domain.zt <  surface_thickness) = sss;
idx = find( interior.domain.zt >= surface_thickness);
min_sal = 34.78;
del_sal = 1;        % max - min...
% s = 0.3;
interior.forcing(4, idx) = min_sal + (del_sal)*(interior.domain.zt(idx)/1000); 

% water pressure
% FIXME: I think this is absolute pressure, so add air pressure???
interior.forcing(5, :) = surface.forcing(1, 9) + interior.domain.zt/10; 

% FIXME: from where is this Fe coming from if NOT at bottom???
% FIXME: this is NOT the 3.5% of dust that is assumed to be Fe...
% FIXME: and BTW, what isw the bottom flux of Fe from sed
interior.forcing(6, :) =       0 ;              % Iron Sediment Flux (nmol/cm^2/s)

mex_marbl_driver ( 'restore_surface_flux_forcings',      surface.forcing );
mex_marbl_driver ( 'restore_interior_tendency_forcings', interior.forcing );

end % init_forcings.m

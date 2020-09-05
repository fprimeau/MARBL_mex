function [my_cell] = surface_flux_output_names()

% size(sfo): 4
%  surface_flux_output(1) name: flux_o2     name: Oxygen Flux	unit:   nmol/cm^2/s
%  surface_flux_output(2) name: flux_co2	name: Carbon Dioxide Flux	unit: nmol/cm^2/s
%  surface_flux_output(3) name: flux_nhx	name: NHx Surface Emissions	unit: nmol/cm^2/s
%  surface_flux_output(4) name: totalChl	name: Total Chlorophyll Concentration	unit: mg/m^3

my_cell{ 1} = 'flux_o2 nmol/cm^2/s';    % O2 flux from sea to air = pv_o2 * (o2-o2_sat)
my_cell{ 2} = 'flux_co2 nmol/cm^2/s';   % CO2 "" = pv_co2 * CO2STAR
my_cell{ 3} = 'flux_nhx nmol/cm^2/s';   % NHX "" = "nhx_surface_emis" = FIXME = what the heck?
my_cell{ 4} = 'totalChl mg/m^3';   % Cholorphyl "" = "totalChl_loc" = FIXME surface -concentration-?

end

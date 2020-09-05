function init_time_series(nstep, dt, lciso_on, interior, surface)

global time_series % This is time series of tracers, diags and states

tracer_cnt = size(interior.tracer,1);

time_series.nstep         = nstep;
time_series.dt            = dt;
time_series.tracer_name   = tracer_names(lciso_on);
time_series.tracer_unit   = tracer_units(lciso_on);
time_series.diag_name     = interior.diag_name;
time_series.forcing_name  = interior.forcing_name;
time_series.surface_diag_name = surface.diag_name;
time_series.forcing_name  = interior.forcing_name;
time_series.surface_forcing_name = surface.forcing_name;

time_series.diag          = zeros(nstep, interior.diag_cnt, interior.domain.nz);
time_series.surface_diags = zeros(nstep, surface.diag_cnt);

time_series.tracer        = zeros(nstep, tracer_cnt, interior.domain.nz);
time_series.tendency      = zeros(nstep, tracer_cnt, interior.domain.nz);

time_series.surface_flux  = zeros(nstep, tracer_cnt);
time_series.sfo           = zeros(nstep, size(surface.sfo, 2)); % e.g. 4 air-sea-flux

time_series.s_forcing     = zeros(nstep, surface.forcing_cnt);
time_series.i_forcing     = zeros(nstep, interior.forcing_cnt, interior.domain.nz);

end


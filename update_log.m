function update_log (n, surface, interior)

global time_series         % This is time series of tracers, diags and states

time_series.tracer        (n, :, 1:interior.domain.kmt) = interior.tracer  (:, 1:interior.domain.kmt);
time_series.tendency      (n, :, 1:interior.domain.kmt) = interior.tendency(:, 1:interior.domain.kmt);
% this is tendency of top layer of interior, because there is a flux with 
% air, but is NOT air-sea flux.
time_series.surface_flux  (n, :)                        = surface.flux' ;   

time_series.sfo           (n, :)                        = surface.sfo;      % -this- is sea->air gas flux

% The diags are large, hence slow to transfer. Saving these monsters adds
% 50% to run time!

time_series.diag          (n, :, 1:interior.domain.kmt) = interior.diag(:, 1:interior.domain.kmt);
time_series.surface_diags (n, :)                        = surface.diag';

time_series.s_forcing     (n, :)                        = surface .forcing;
time_series.i_forcing     (n, :, 1:interior.domain.kmt) = interior.forcing(:, 1:interior.domain.kmt);

end

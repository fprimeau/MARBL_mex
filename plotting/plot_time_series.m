function plot_time_series(tracer, name, level, dt, depth)

t = 1:size(tracer,1);
t = t*dt/60/60/24;

% close all;

% ****** Plot -ALL- tracers at given level, hard to read but great for debug 

figure(3)

tl = tiledlayout('flow','TileSpacing','compact','Padding','compact'); % Requires R2019b or later

for i=1:size(tracer,2)
    tile = nexttile(tl); 
    plot(t, tracer(:,i, level)); 
    ylabel (i+". "+name(i), 'Interpreter', 'none')
end

%  Add the surface flux at the end 

% Add title and x axis label, but they waste a lot of space

linkaxes(tile,'x')
myTitle = sprintf('Ocean Interior Layer #%d, depth = %d(m)', level, round(depth));
title(tl, myTitle);
xlabel(tl,"Time Steps (day)");
ylabel(tl,"Typical Units ( mmol/m^3, meq/m^3, or mg/m^3 )");

end


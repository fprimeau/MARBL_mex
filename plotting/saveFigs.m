function saveFigs(dname)

% These files tend to be -HUGE- because they have -ALL- time steps for 100s
% of years in them


FigList = findobj(allchild(0), 'flat', 'Type', 'figure');

for iFig = 1:length(FigList)
    
    FigHandle = FigList(iFig);
    FigName   = num2str(get(FigHandle, 'Number'));
    set(0, 'CurrentFigure', FigHandle);
    
    savefig(fullfile(dname, [FigName '.fig']));
    
end

end

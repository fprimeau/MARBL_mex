function [zt,zw,dzt,dzw] = zgrid_fp(nz,H,s)
% [zt,zw,dzt,dzw] = zgrid_fp(km);
% MATLAB script to create a smooth vertical grid for the OGCM
% nz wet points  
% Author: Francois Primeau 13/10/2000
% 
%
% imput parameter------------------------------------------------------
%
    
%s = 0.6;     %stretching factor
%H = 4500;  %maximum depth 
    formula = 'tanh((1-eta)/s)/tanh(1/s)-1'; %smooth transformation
    result = '2*(2*cosh(((eta-1)^2)/s)-3)/(s^2*cosh((eta-1)/s)^2)';
    % ---------------------------------------------------------------------
    
    %
    % compute the zt,zw,dzt,dzw and metric factor in truncation error------
    %
    etaw = -1/nz:1/nz:1+1/nz;
    f = inline(vectorize(formula),'eta','s');
    ef = inline(vectorize(result),'eta','s');
    zw = H*(f(etaw,s));
    etat = etaw+0.5/nz;
    zt = H*(f(etat,s));
    dzt = zw(1:end-2)-zw(2:end-1);
    dzw = zt(1:end-2)-zt(2:end-1);
    zt = zt(2:end-2);
    zw = zw(2:end-1);
    dzt = dzt(2:end);
    dzw(1) = dzw(1);
    dzw(end) = dzw(end);
end

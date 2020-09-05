function [T,grd] = ad(s, H, nz,av,w)
%
% [T,grd] = ad(Av,nz)
%
% OUTPUTS:
% T: nz x nz advection diffusion transport matrix for a 1-d column
%    model with nz levels
%
% grd: structure with the grid definition
%
%     H = 3700; % maximum detph of water column
%     s = 0.6;  % grid stretching factor

    [zt,zw,dzt,dzw] = zgrid_fp(nz,H,s);
    grd.zt = zt;
    grd.zw = zw;
    grd.dzw = dzw;
    grd.dzt = dzt;
    
    % inline function to make a sparse diagonal matrix from vector x
    d0 = @(x) spdiags(x(:),0,length(x(:)),length(x(:)));

    % difference operator
    I = speye(nz+1);
    D = I-I([2:end,1],:); 
    D = D(:,2:end);
    % grad operator (vertical component)
    grad = d0(1./dzw)*D; 
    % div operator (1-d)                                  
    div =  -d0(1./dzt)*D.';  % one-d divergence operator

    % vertical diffusivity
    K = d0(av(zw));    

    % operator for divergence of advective flux
    e = ones(nz,1)/nz;
    c0 = flipud(cumsum(e));
    A = spdiags([c0,-c0],[0,1],nz,nz);
    A(:,1) = A(:,1)-e;
    A = d0(1./dzt)*A;  

    % diffusion flux operator
    F =  -K*grad;
    % enforce the no flux b.c. at the top and bottom
    F(1,:) = 0;
    F(end,:) = 0;
    T = div*F+w*A;
end

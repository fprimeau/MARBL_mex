function [T] = transport(domain, w)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

nz = domain.nz;
dzw = domain.grd.dzw;
dzt = domain.grd.dzt;
zw  = domain.grd.zw;

% inline function to make a sparse diagonal matrix from vector x
d0 = @(x) spdiags(x(:),0,length(x(:)),length(x(:)));

% difference operator
I = speye(nz+1);
D = I-I([2:end,1],:);
D = D(:,2:end);
% grad operator (vertical component)
grad = d0(1./dzw)*D;
% div operator (1-d)
div =  d0(1./dzt)*D.';  % one-d divergence operator

% vertical diffusivity
% enhanced diffusivity in mixed layer
av = @(z) (z>-100).*(0.1)+4e-4;

K = d0(av(zw));
% K = K*0;

% operator for divergence of advective flux
e = ones(nz,1)/nz;
c0 = flipud(cumsum(e));
A = spdiags([c0,-c0],[0,1],nz,nz);
A(:,1) = A(:,1)-e;
A = d0(1./dzt)*A;
% diffusion flux operator
F =  K*grad;
% enforce the no flux b.c. at the top and bottom
F(1,:) = 0;
F(end,:) = 0;
T = div*F+w*A;

end


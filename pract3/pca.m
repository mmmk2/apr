function [m,W]=pca(X)
m = mean(X')'; # se calcula la media global
[filas,columnas] = size(X); # columnas es el numero de muestras
A = X - m; # A = datos menos la media
cov = 1/columnas * (A * A'); #para calcular la matriz de covarianzas 
[eigvec, eigval] = eig(cov); #se calculan los vectores y valores propios
[sort_eigval, indices] = sort(-diag(eigval)); #se ordenan los vectores propios por valores propios (descendente)
W = eigvec(:,indices); #Devuelve los vectores propios en la direcci´on opuesta a la del pdf
endfunction

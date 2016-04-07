'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Optimizació por Gradiente

Juan Esteban Mejía Velásquez

'''

if(!require(animation, quietly=TRUE))install.packages("animation")

library(animation)

## Funcion objetivo por defecto

par(mar = c(4, 4, 2, 0.1))
grad.desc()

## Fallo de búsqueda

ani.options(nmax = 70)
par(mar = c(4, 4, 2, 0.1))
f2 = function(x, y) sin(1/2 * x^2 - 1/4 * y^2 + 3) * cos(2 * x + 1 - exp(y))
grad.desc(f2, c(-2, -2, 2, 2), c(-1, 0.5), gamma = 0.2, tol = 1e-04)







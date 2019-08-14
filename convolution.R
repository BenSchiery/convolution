rm(list = ls())

conv <- function(f, g){
  E <- new.env()
  E$f <- f
  E$g <- g
  function(t){
    sapply(X = t,
           FUN = function(t.){
             tau <- seq(0, t., length.out = 1000)
             y <- E$f(tau) * E$g(1 - tau)
             t. * mean(y)
           })
  }
}

f <- function(x){
  cos(10 * x) / (1 + x^2)
}

g <- function(x){
  (x + sin(x^2)) / (1 + x + x^2)
}

h <- conv(f = f, g = g)

curve(f, from = -10, to = 10, n = 1001)
curve(g, from = -10, to = 10, n = 1001)
curve(h, from = -10, to = 10, n = 1001)

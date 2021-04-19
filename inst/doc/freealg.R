## ----set-options, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#", dev = "png", fig.width = 7, fig.height = 3.5, message = FALSE, warning = FALSE)
options(width = 80, tibble.width = Inf)

## -----------------------------------------------------------------------------
library("freealg")
X <- freealg(words = list(1, c(24,25), c(25,24), c(1,1,1,2)), coeffs = c(5, 43, 6, -17))
dput(X)
X

## -----------------------------------------------------------------------------
(X <- as.freealg("3aab -2abbax"))  # caret ("^") not yet implemented
(Y <- as.freealg("2 -3aab -aBBAA"))  # uppercase letters are inverses
(Z <- as.freealg(1:3))

## -----------------------------------------------------------------------------
X^2        # powers are implemented
X+Y        #  'aab' term cancels
1000+Y*Z   # algebra multiplication and addition works as expected

## -----------------------------------------------------------------------------
subs("1+4a",a="xx")
p <- as.freealg("1+aab+4aba")
subs(p,a="1+x",b="y+xx")

## -----------------------------------------------------------------------------
library("magrittr")
"1+aaab+4abaa" %>% subs(b="1+x+3aa")

## -----------------------------------------------------------------------------
k <- as.freealg("3 + 4aa + 7aaaaaa") # 3 + 4a^2 + 7a^6
k
deriv(k,1)   # dk/da  = 8a+42a^4

## -----------------------------------------------------------------------------
deriv(as.freealg("abaaaa"),1)  # d(aba^4)/da = ba^4 + 4aba^3

## -----------------------------------------------------------------------------
deriv(as.freealg("A"),1)    # d(a^-1)/da = -a^-2

## -----------------------------------------------------------------------------
deriv(as.freealg("Aba"),1)    # d(a^-1ba)/da = -a^-2ba + a^-1b

## -----------------------------------------------------------------------------
X <- as.freealg("aaabAcbbbbb")
X
deriv(X,c(1,1,2))

## -----------------------------------------------------------------------------
(X <- rfalg(maxsize=10,include.negative=TRUE))
d <- deriv(X,1)
deriv(X^3,1) == X^2*d + X*d*X + d*X^2

## -----------------------------------------------------------------------------
deriv(X,1:2) == deriv(X,2:1)

## -----------------------------------------------------------------------------

deriv(X,c(1,2,3,1,2,3)) == deriv(X,c(3,3,2,1,2,1))

## -----------------------------------------------------------------------------
(X <- rfalg(maxsize=10,include.negative=TRUE))
(Y <- rfalg(maxsize=10,include.negative=TRUE))
deriv(X*Y,1) == deriv(X,1)*Y + X*deriv(Y,1)

## -----------------------------------------------------------------------------
f1 <- function(x){deriv(x,1)}
f2 <- function(x){deriv(x,2)}
f1(f2(X)) == f2(f1(X))  # Young
f1(f2(X*Y)) == X*f1(f2(Y)) + f1(X)*f2(Y) + f2(X)*f1(Y) + f1(f2(X))*Y # Leibniz

## -----------------------------------------------------------------------------
phi <- rfalg(n=5,inc=TRUE)
phi
options("usecaret" = TRUE)
phi
options("usecaret" = FALSE)  # reset to default

## -----------------------------------------------------------------------------
X <- as.freealg("x+y+X+Y")
X^2
constant(X^4)

## -----------------------------------------------------------------------------
f <- function(n){constant(as.freealg("x+y+X+Y")^n)}
sapply(c(0,2,4,6,8,10),f)


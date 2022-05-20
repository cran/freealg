## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("freealg")
set.seed(1)

## ----label=defineAandB--------------------------------------------------------
(A <- as.freealg("xxyx + 2zy"))
(B <- as.freealg("3z + 4yyyy"))
A*B
B*A

## ----useuppercase-------------------------------------------------------------
(C <- as.freealg("3 + 5X - 2Xyx"))
A*C
C*A

## ----checklaws----------------------------------------------------------------
A*(B+C) == A*B + A*C
(A+B)*C == A*C + B*C
A*(B*C) == (A*B)*C

## ----label=usecommutator------------------------------------------------------
.[as.freealg("x"),as.freealg("y")]

## ----label=verifyjacobi-------------------------------------------------------
x <- rfalg()
y <- rfalg()
z <- rfalg()

.[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]]

## ----showsubstitution---------------------------------------------------------
subs("aabccc",b="1+3x")  # aa(1+3x)ccc

## ----sequentialsubstitution---------------------------------------------------
subs("abccc",b="1+3x",x="1+d+2e")

## ----showderiv----------------------------------------------------------------
deriv(as.freealg("aaaxaa"),"a")


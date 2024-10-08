## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)
library("freealg")
set.seed(1)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/freealg.png", package = "freealg"))

## ----label=defineAandB--------------------------------------------------------
(A <- as.freealg("xxyx + 2zy"))
(B <- as.freealg("-2zy + 3yyyy"))
A+B
A*B
B*A

## ----useuppercase-------------------------------------------------------------
A*as.freealg("X") # X = x^{-1}

## ----uppercaseC---------------------------------------------------------------
(C <- as.freealg("3 + 5X - 2Xyx"))
A*C
C*A

## ----checklaws----------------------------------------------------------------
A*(B+C) == A*B + A*C
(A+B)*C == A*C + B*C
A*(B*C) == (A*B)*C

## ----label=usecommutator------------------------------------------------------
a <- as.freealg("a")
b <- as.freealg("b")
.[a,b] # returns ab-ba

## ----label=verifyjacobi-------------------------------------------------------
x <- rfalg()
y <- rfalg()
z <- rfalg()

.[x,.[y,z]] + .[y,.[z,x]] + .[z,.[x,y]]

## ----showsubstitution---------------------------------------------------------
subs("aabccc",b="1+3x")  # aa(1+3x)ccc

## ----sequentialsubstitution---------------------------------------------------
subs("abccc",b="1+3x",x="1+d+2e")

## ----showaccessor-------------------------------------------------------------
(a <- as.freealg("aaa + 2*aaba + 3*abbbba + 9*xyzabc - 3*abc"))
a[coeffs(a) > 2]
a[coeffs(a) < 0] <- 99
a

## ----showderiv----------------------------------------------------------------
deriv(as.freealg("aaaxaa"),"a")


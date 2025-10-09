
## My Comments

Changes from 0.1.5.2:

* Replaced `abs` function with `labs` function when using long integers in src/rcpp_bcFact_int.

I've done what I can for the Fedora clang error:

 - I've significantly reduced (by approx 35%) the installation size of the compiled library.
 - The installation time of the compiled library is shorter (though the exact time depends on the computer used of course).
 - I've re-written some of the smaller 'C++' scripts in 'C', and split some of the larger 'C++' scripts into smaller scripts.
 - I've used rhub to install the package on several Operating Systems (including Fedora clang) to check if installs without errors (it did).

So I REALLY hope it passes fedora clang on CRAN this time!


## R CMD check results

0 errors | 0 warnings | 0 notes

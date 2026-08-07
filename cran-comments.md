
Update to version 0.1.9.5.
Thank you for all your hard work people of CRAN!

I've seen the VALGRIND issues on the CRAN checks page.
I could not reproduce them using `rhub`,
but I've done a thorough analysis of all C/C++ functions involved in the tests that give the issues,
including an AI-assisted analysis of my code.
And although I'm new to the world of VALGRIND, I think I've fixed the issues.


## R CMD check results

0 errors | 0 warnings | 0 notes

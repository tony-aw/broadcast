
## My Comments

* This is a continuation of my initial submission.
* One warning was found on the Debian system regarding the usage of the 'abs()' function with long int input; I've fixed this.

* As always, I want to sincerely thank you for taking your time to review R-packages!
I appreciate your hard, voluntary work!
* I have created thousands (sorry!) of tests, many using for-loops (to reduce the amount of repetitive coding/typing).
The coverage of the tests is approximately 95%. No errors found.
* I personally use Windows 11 as my operating system.
But I have checked my package against various Operating Systems via GitHub actions;
no errors found on any other OS either.
* There are currently no references describing my package.
* This package has been inspired by the "broadcasting" mechanism from the 'Numpy' module for the 'Python' programming language.
However, this package does not depend on, vendor, link to, include, or otherwise use 'Numpy' or any external libraries.
All 'C'/'C++' code in this package has been written by me from scratch.
* A lot of effort has been made to make this package efficient, both in terms of memory usage and speed,
because more efficient code is better for the environment. I trust CRAN can appreciate this effort.


## R CMD check results

0 errors | 0 warnings | 3 notes

There are 3 NOTES given by the R CMD check results:

 * The first Note is for the fact that this is a new submission (this is to be expected).
 * The second Note states that 'R' was unable to verify the current time.
 I think this is an issue on the side of 'R', and not my package.
 * The third Note says that the 'libs' sub-directory has a size of 4.8Mb.
This is due to the rather large amount of 'C' and 'C++' code.
I don't think this 'R' package has an especially exceptional amount of compiled code.
Indeed, famous packages like 'data.table' have far more compiled code than this package.
I trust this isn't an issue I have to worry about.


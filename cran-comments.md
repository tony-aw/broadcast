
Update to version 0.1.9.6.

This is in response to the undefined behaviour Prof Brian Ripley found.

I found the mistake.
There was a small mistake in a couple of my new unit tests.
The unit test created random dimensions (no actual array, just a vector of dimensions), but those dimensions could, on rare occasions, have a product` > 2^52 - 1`.
This naturally caused undefined behaviour.

I fixed it; sorry for the inconvenience!

Just in case I re-checked all my new unit-tests that involved randomly generating numbers for possibilities of numeric overflow.
All should be fine now.


## R CMD check results

0 errors | 0 warnings | 0 notes

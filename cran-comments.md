## Resubmission

This is a resubmission. In this submission I:

* quote software names in title and description (e.g. SQL --> 'SQL')
* confirm that there are no references describing this package to add to the 
  description
* have removed all instances of if(FALSE) from function examples. All affected
  code has been replaced with runnable examples.
* have ensured no functions write to the users home filespace either by default
  or in vignettes/examples/tests. One function, config_examples(), will write a
  file only if supplied with a character filename argument, which is NA by 
  default.
* have removed all instances of print() and cat() from functions. The relevant
  information is now returned as a string object or transmitted by calls to
  message()/warning()/stop().

## References

There are no references describing this package

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

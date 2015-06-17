## Test environments
* Ubuntu 14.04
  * R 3.2.0 (2015-04-16)
* win-builder (devel and release): http://win-builder.r-project.org/
  * R Under development (unstable) (2015-06-17 r68528)
  * R version 3.2.0 (2015-04-16)

## R CMD check results
There were no ERRORs or WARNINGs.

  * I get one NOTE that 'Intraclass' may not be spelled correctly in the Description. This is the correct spelling. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of `ICC`.
  * One package (`abd`) Suggests `ICC` and passes the checks.

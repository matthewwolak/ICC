## Test environments

  - Ubuntu 20.04
    - R 4.2.0 (2022-04-22) x86_64-pc-linux-gnu (64-bit)
  
  - [win-builder](http://win-builder.r-project.org/)
    - R version 4.2.0 (2022-04-22 ucrt), platform: x86_64-w64-mingw32 (64-bit)
    - R version 4.1.3 (2022-03-10), platform: x86_64-w64-mingw32 (64-bit)
    - R Under development (unstable) (2022-05-18 r82376 ucrt), platform: x86_64-w64-mingw32 (64-bit)

  - R-hub (`devtools::check_rhub(".", interactive = FALSE)`)
    - Windows Server 2022, R-devel, 64 bit
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.


## Downstream dependencies
I have also run R CMD check on downstream dependencies of `ICC`.

  - Two packages (`correctedAUC` and `SimTimeVar`) Import `ICC` and do not cause any ERRORs, WARNINGs, or NOTEs related to `ICC.
  - One package (`abd`) Suggests `ICC` and passes the checks.

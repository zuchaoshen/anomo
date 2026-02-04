## Initial Submission

This is a resubmission to update the package. 
The updates include (a) updating the mcci
function for the plot; (b) updating the power.1.eq 
function, and (c) updating 
 examples to illustrate the use of functions.

Across operational systems we tested, there are no errors or warnings, 
there is one note can be ignored (see below).

Please help process the package at your convenience! Thanks a lot! 

## Test environments
* local Windows 11, R 4.5.2
* win-builder (devel and release)
* Rhub (windows, macos, linux)

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √ (local Windows 11)
0 errors √ | 0 warnings √  | 0 notes √ (win-builder )
0 errors √ | 0 warnings √  | 1 notes X (Rhub, windows/macos/linux)

## Reverse dependencies

There are no issues on reverse dependencies.

---
* Found the following hidden files and directories:
  .github
These were most likely included in error. See section 'Package
structure' in the 'Writing R Extensions' manual.

Response: The .github folder has been added to .gitignore and .Rbuildignore. 
 This note seems to be platform specific on Rhub.

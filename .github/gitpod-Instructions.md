# Instructions for working in gitpod

## DON'T TOUCH ANYTHING YET, README FIRST

It is possible that at this moment gitpod is setting up the project for the first time.

Let it install all dependencies first.

The project will be ready when the `R Interactive` terminal shows up.

## First Run

Checklist:

-   [ ] Ignore a warning about "X extensions are suggested but...".
-   [ ] The R terminal is started.
-   [ ] In the status bar (down-right), check for the status `R xxx` where xxx is a number. If no terminal is attached `R: (not attached)`, close the current R terminal (if any) using the command `q()`, open the command palette and select: `R: Create R terminal`.
-   [ ] Confirm that `renv` is activated: `Project '/workspace/false.alarm' loaded. [renv 0.13.2]`.
-   [ ] On the first run, the folder inst/extdata will be missing/empty. You need to run the `run.R` script first to download the dataset and start the first `targets` run.

## Next

Below the Explorer View, you'll see the Task Explorer, there I added the most used commands I used during the development (under the 'vscode' branch, not the 'bash').

The most important ones:

-  **R: test units**: This checks if the basic functions are working as supposed. If all runs without errors, it is properly working. You can run the tests individually using the Testing view on the left bar (an Erlenmeyer tube).
-  **R: targets**: Run the targets pipeline in the background, using a summary report.

> Obs: **R: ASAN/USBAN** was used only for sanity check, not available in the project.

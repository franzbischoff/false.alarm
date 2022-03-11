# Instructions for working in gitpod

## DON'T TOUCH ANYTHING YET, README FIRST

It is possible that at this moment, gitpod is setting up the project for the first time.

Let it install all dependencies first, start the extensions, etc.

## First Run

Checklist:

-  [ ] Ignore a warning about "X extensions are suggested but...".
-  [ ] In the status bar (down-right), check for the status `R: xxx` or `R: (not attached)`. This means the R extension is prepared. Use Ctrl+Shift+P to open the command palette and select: `R: Create R terminal`.
-  [ ] Confirm that `renv` is activated. Example: `Project '/workspace/false.alarm' loaded. [renv 0.13.2]`.
-  [ ] On the first run, the folder inst/extdata will be missing/empty. You need to run the `run.R` script first to download the dataset and start the first `targets` run.

## Next

If gitpod didn't mess up, you'd see the Task Explorer below the Explorer View. There I added the most used commands I used during the development (under the 'vscode' branch, not the 'bash').

If there is no Task Explorer, use the command palette and select `Tasks: Run Task`.

The most important ones:

-  **R: test units**: This checks if the basic functions are working as supposed. If all runs without errors, it is properly working. You can run the tests individually using the Testing view on the left bar (an Erlenmeyer tube).
-  **R: targets**: Run the targets pipeline in the background, using a summary report.

> Obs: **R: ASAN/USBAN** was used only for a sanity check, not available in the project.

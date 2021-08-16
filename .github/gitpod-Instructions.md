# Instructions for working in gitpod

## DON'T TOUCH ANYTHING YET, README FIRST

It is possible that at this moment gitpod is setting up the project for the first time.

Let it install all dependencies first.

The project will be ready when the workspace is reloaded and you see the title **FALSE.ALARM (WORKSPACE)** in the Explorer View (Ctrl+Shift+E).

## First Run

Checklist:

-   [ ] Ignore a warning about "X extensions are suggested but..."
-   [ ] The R terminal is started.
-   [ ] In the status bar (down-right), check for the status "R xxx" where xxx is a number. If no terminal is attached, close the terminal using the command `q()` and click on the "R xxx" message to start a new and attached terminal (or use the command palette "R: Create R terminal").
-   [ ] Confirm that `renv` is activated: `Project '/workspace/false.alarm' loaded. [renv 0.13.2]`
-   [ ] On the first run, the folder inst/extdata will be missing/empty. You need to run the `run.R` script first to download the dataset and start the first `targets` run.

# use this in the `setup` chunk or in .Rprofile
# Check if "here" is installed system-wide
rlang::check_installed("here")

# Get the current working directory
current_folder <- getwd()

# Get project root directory
project_root <- here::here()

# change the working directory to the project root
## The cwd will be restored to the original in the next chunks
## See the Note section in ?knitr::knit
setwd(project_root)

# load the renv library
source("renv/activate.R")

# setwd(current_folder) # if needed

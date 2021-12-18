#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "tidyverse", # for wrangling
  "ggrepel", "DT"
)
purrr::walk(packages, library, character.only = TRUE)



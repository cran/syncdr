## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(syncdr)
#devtools::load_all(".")

## ----setup--------------------------------------------------------------------

# Create .syncdrenv with left and right directories
.syncdrenv =toy_dirs()

# Get left and right directories' paths 
left  <- .syncdrenv$left
right <- .syncdrenv$right


## -----------------------------------------------------------------------------

# example
search_duplicates(right, verbose = TRUE)

## ----ssfile, eval = FALSE-----------------------------------------------------
# save_sync_status(dir_path = right)


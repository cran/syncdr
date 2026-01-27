## ----include = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

old_options <- options(width = 200)
options(width = 200)
#devtools::load_all(".")

## ----setup--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(syncdr)

# Create syncdr env with left and right directories
.syncdrenv =toy_dirs()

# Get left and right directories' paths 
left  <- .syncdrenv$left
right <- .syncdrenv$right


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sync_status <- compare_directories(left, 
                                   right,
                                   by_content = TRUE)


## ----dtable-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

display_sync_status(sync_status$common_files,
                    left_path  = left,
                    right_path = right)
display_sync_status(sync_status$non_common_files,
                    left_path  = left,
                    right_path = right)


## -----------------------------------------------------------------------------

# Tree structure or right directory
display_dir_tree(path_left = left)

# Tree structure of left directory
display_dir_tree(path_right = right)

# Tree structure of both 
display_dir_tree(path_left = left, path_right = right, )

# Restore options
options(old_options)


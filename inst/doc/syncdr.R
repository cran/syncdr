## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

#devtools::load_all(".")
library(syncdr)

## ----toy-dirs-----------------------------------------------------------------

# Create syncdr env with left and right directories
.syncdrenv =toy_dirs()

# Get left and right directories' paths 
left  <- .syncdrenv$left
right <- .syncdrenv$right


## ----display-toy-dirs---------------------------------------------------------

# Visualize left and right directories' tree structure 
display_dir_tree(path_left  = left,
                 path_right = right)


## ----by-date------------------------------------------------------------------

# Compare by date only -the Default
sync_status_date <- compare_directories(left, 
                                        right)

sync_status_date

## ----by-date-cont-------------------------------------------------------------

# Compare by date and content 
sync_status_date_content <- compare_directories(left, 
                                                right,
                                                by_content = TRUE)

sync_status_date_content

## ----by-content---------------------------------------------------------------

# Compare by date and content 
sync_status_content <- compare_directories(left, 
                                            right,
                                            by_date    = FALSE,
                                            by_content = TRUE)

sync_status_content

## ----verbose-example----------------------------------------------------------

compare_directories(left,
                    right,
                    by_date    = FALSE,
                    by_content = TRUE,
                    verbose    = TRUE)


## -----------------------------------------------------------------------------

display_sync_status(sync_status_date$common_files,
                    left_path  = left,
                    right_path = right)

## -----------------------------------------------------------------------------

display_sync_status(sync_status_date$non_common_files,
                    left_path  = left,
                    right_path = right)

## ----asym-sync-example--------------------------------------------------------

# Compare directories

sync_status <- compare_directories(left,
                                   right,
                                   by_date = TRUE)

# Synchronize directories 
full_asym_sync_to_right(sync_status = sync_status)




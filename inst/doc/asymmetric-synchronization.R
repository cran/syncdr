## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
library(syncdr)
#devtools::load_all(".")

# Create .syncdrenv
.syncdrenv = toy_dirs()

# Get left and right directories' paths 
left  <- .syncdrenv$left
right <- .syncdrenv$right


## -----------------------------------------------------------------------------

# With leader/master directory being the left directory 
# Option 1 
full_asym_sync_to_right(left_path  = left,
                        right_path = right,
                        by_content = TRUE)
# Option 2
sync_status <- compare_directories(left_path  = left,
                                   right_path = right,
                                   by_content = TRUE)

full_asym_sync_to_right(sync_status = sync_status)

# With leader/master directory being the right directory 
sync_status <- compare_directories(left_path  = right,  #notice args changing here
                                   right_path = left,
                                   by_content = TRUE)

full_asym_sync_to_right(sync_status = sync_status)

## ----include=FALSE------------------------------------------------------------

.syncdrenv.2 <- syncdr:::copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.2$left
right <- .syncdrenv.2$right

## -----------------------------------------------------------------------------

sync_status <- compare_directories(left_path  = left,
                                   right_path = right)

common_files_asym_sync_to_right(sync_status = sync_status)

## ----include=FALSE------------------------------------------------------------

.syncdrenv.3 <- copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.3$left
right <- .syncdrenv.3$right


## -----------------------------------------------------------------------------

sync_status <- compare_directories(left_path  = left,
                                   right_path = right)

update_missing_files_asym_to_right(sync_status = sync_status)


## ----include=FALSE------------------------------------------------------------

.syncdrenv.4 <- copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.4$left
right <- .syncdrenv.4$right


## -----------------------------------------------------------------------------

sync_status <- compare_directories(left_path  = left,
                                   right_path = right)

partial_update_missing_files_asym_to_right(sync_status = sync_status)



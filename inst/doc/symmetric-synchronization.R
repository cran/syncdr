## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

#devtools::load_all(".")


## ----setup--------------------------------------------------------------------
library(syncdr)

# Create .syncdrenv
.syncdrenv = toy_dirs()

# Get left and right directories' paths 
left  <- .syncdrenv$left
right <- .syncdrenv$right

## -----------------------------------------------------------------------------

sync_status <- compare_directories(left_path  = left,
                                   right_path = right,
                                   by_content = TRUE)

# Providing left and right paths object
# full_symmetric_sync(left, right)

# Providing sync_status object
full_symmetric_sync(sync_status = sync_status)

## ----include = FALSE----------------------------------------------------------

.syncdrenv.1 <- copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.1$left
right <- .syncdrenv.1$right


## -----------------------------------------------------------------------------

sync_status <- compare_directories(left_path  = left,
                                   right_path = right)

# Example with left and right paths 
full_symmetric_sync(left_path = left,
                    right_path = right)

## ----include = FALSE----------------------------------------------------------

.syncdrenv.2 <- copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.2$left
right <- .syncdrenv.2$right


## -----------------------------------------------------------------------------

sync_status <- compare_directories(left_path  = left,
                                   right_path = right)

partial_symmetric_sync_common_files(sync_status = sync_status)


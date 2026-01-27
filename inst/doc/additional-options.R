## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(syncdr)

## ----setup2-------------------------------------------------------------------

# generate toy directories
e     <- toy_dirs()

.syncdrenv.1 <- syncdr:::copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.1$left
right <- .syncdrenv.1$right

sync_status <- compare_directories(left, right)

# call sync function
full_asym_sync_to_right(sync_status = sync_status,
                        backup      = TRUE)



## ----include = FALSE----------------------------------------------------------

.syncdrenv.2 <- copy_temp_environment()

# Get left and right directories' paths 
left  <- .syncdrenv.2$left
right <- .syncdrenv.2$right

sync_status <- compare_directories(left, right)



## ----force--------------------------------------------------------------------

full_asym_sync_to_right(sync_status = sync_status,
                        force      = FALSE)

#You will need to type `no` or 'cancel' to stop the synchronization




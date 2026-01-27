
<!-- README.md is generated from README.Rmd. Please edit that file -->

# syncdr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RossanaTat/syncdr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RossanaTat/syncdr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

<!-- badges: end -->

{syncdr} is an R package designed to facilitate the process of directory
comparison and synchronization. This package provides essential tools
for users who need to manage and synchronize their directories
effectively.

With {syncdr}, users can:

- Visualize Directory Structures: Gain a comprehensive view of directory
  contents, including the tree structure, common files, and files unique
  to each directory.

- Manage Files with Ease: Perform content-based and modification
  date-based file comparisons, and handle tasks like identifying
  duplicates, copying, moving, and deleting files seamlessly within the
  R environment. By incorporating {syncdr} into their workflow, users
  can achieve a more organized and up-to-date file system, simplifying
  the overall management and synchronization of directories.

## Installation

You can install the development version of syncdr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RossanaTat/syncdr")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo RossanaTat/syncdr@HEAD
#> joyn       (0.3.0        -> e2dfd68ec...) [GitHub]
#> xfun       (0.50         -> 0.54        ) [CRAN]
#> rlang      (1.1.4        -> 1.1.6       ) [CRAN]
#> cli        (3.6.3        -> 3.6.5       ) [CRAN]
#> Rcpp       (1.0.13-1     -> 1.1.0       ) [CRAN]
#> magrittr   (2.0.3        -> 2.0.4       ) [CRAN]
#> later      (1.3.2        -> 1.4.4       ) [CRAN]
#> fastmap    (1.1.1        -> 1.2.0       ) [CRAN]
#> digest     (0.6.37       -> 0.6.39      ) [CRAN]
#> htmltools  (0.5.8.1      -> 0.5.9       ) [CRAN]
#> fs         (1.6.5        -> 1.6.6       ) [CRAN]
#> sass       (0.4.9        -> 0.4.10      ) [CRAN]
#> mime       (0.12         -> 0.13        ) [CRAN]
#> cachem     (1.0.8        -> 1.1.0       ) [CRAN]
#> tinytex    (0.54         -> 0.58        ) [CRAN]
#> bslib      (0.8.0        -> 0.9.0       ) [CRAN]
#> evaluate   (1.0.3        -> 1.0.5       ) [CRAN]
#> yaml       (2.3.10       -> 2.3.12      ) [CRAN]
#> rmarkdown  (2.29         -> 2.30        ) [CRAN]
#> knitr      (1.49         -> 1.50        ) [CRAN]
#> collapse   (569a4a513... -> 69ce87bc4...) [GitHub]
#> data.table (1.17.0       -> 1.17.8      ) [CRAN]
#> promises   (1.2.1        -> 1.5.0       ) [CRAN]
#> crosstalk  (1.2.1        -> 1.2.2       ) [CRAN]
#> DT         (0.33         -> 0.34.0      ) [CRAN]
#> Installing 23 packages: xfun, rlang, cli, Rcpp, magrittr, later, fastmap, digest, htmltools, fs, sass, mime, cachem, tinytex, bslib, evaluate, yaml, rmarkdown, knitr, data.table, promises, crosstalk, DT
#> Installing packages into 'C:/Users/wb621604/AppData/Local/Temp/RtmpWaTr9Q/temp_libpath27ec7ad3308a'
#> (as 'lib' is unspecified)
#> 
#>   There are binary versions available but the source versions are later:
#>             binary source needs_compilation
#> xfun          0.52   0.54              TRUE
#> rlang        1.1.5  1.1.6              TRUE
#> cli          3.6.4  3.6.5              TRUE
#> Rcpp        1.0.14  1.1.0              TRUE
#> magrittr     2.0.3  2.0.4              TRUE
#> later        1.4.1  1.4.4              TRUE
#> digest      0.6.37 0.6.39              TRUE
#> htmltools  0.5.8.1  0.5.9              TRUE
#> fs           1.6.5  1.6.6              TRUE
#> sass         0.4.9 0.4.10              TRUE
#> tinytex       0.56   0.58             FALSE
#> evaluate     1.0.3  1.0.5             FALSE
#> yaml        2.3.10 2.3.12              TRUE
#> rmarkdown     2.29   2.30             FALSE
#> data.table  1.17.0 1.17.8              TRUE
#> promises     1.3.2  1.5.0              TRUE
#> crosstalk    1.2.1  1.2.2             FALSE
#> DT            0.33 0.34.0             FALSE
#> 
#> package 'fastmap' successfully unpacked and MD5 sums checked
#> package 'mime' successfully unpacked and MD5 sums checked
#> package 'cachem' successfully unpacked and MD5 sums checked
#> package 'bslib' successfully unpacked and MD5 sums checked
#> package 'knitr' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\downloaded_packages
#> installing the source packages 'xfun', 'rlang', 'cli', 'Rcpp', 'magrittr', 'later', 'digest', 'htmltools', 'fs', 'sass', 'tinytex', 'evaluate', 'yaml', 'rmarkdown', 'data.table', 'promises', 'crosstalk', 'DT'
#> Downloading GitHub repo randrescastaneda/joyn@DEV
#> collapse (569a4a513... -> 69ce87bc4...) [GitHub]
#> Downloading GitHub repo SebKrantz/collapse@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458068ea27ec\fastverse-collapse-69ce87b/DESCRIPTION' ...     checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458068ea27ec\fastverse-collapse-69ce87b/DESCRIPTION' ...   ✔  checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458068ea27ec\fastverse-collapse-69ce87b/DESCRIPTION' (660ms)
#>       ─  preparing 'collapse': (19.8s)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#> ─  cleaning src
#>       ─  checking for LF line-endings in source and make files and shell scripts (974ms)
#>       ─  checking for empty or unneeded directories
#>       ─  building 'collapse_2.1.5.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/wb621604/AppData/Local/Temp/RtmpWaTr9Q/temp_libpath27ec7ad3308a'
#> (as 'lib' is unspecified)
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458027d42e3f\randrescastaneda-joyn-e2dfd68/DESCRIPTION' ...     checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458027d42e3f\randrescastaneda-joyn-e2dfd68/DESCRIPTION' ...   ✔  checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458027d42e3f\randrescastaneda-joyn-e2dfd68/DESCRIPTION' (585ms)
#>       ─  preparing 'joyn': (9s)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (440ms)
#>       ─  checking for empty or unneeded directories
#>   Removed empty directory      Removed empty directory 'joyn/inst/tmp'
#>       ─  building 'joyn_0.3.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/wb621604/AppData/Local/Temp/RtmpWaTr9Q/temp_libpath27ec7ad3308a'
#> (as 'lib' is unspecified)
#> Skipping install of 'collapse' from a github remote, the SHA1 (69ce87bc) has not changed since last install.
#>   Use `force = TRUE` to force installation
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458038992106\RossanaTat-syncdr-62ce8df/DESCRIPTION' ...     checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458038992106\RossanaTat-syncdr-62ce8df/DESCRIPTION' ...   ✔  checking for file 'C:\Users\wb621604\AppData\Local\Temp\Rtmp6vU5MR\remotes458038992106\RossanaTat-syncdr-62ce8df/DESCRIPTION' (748ms)
#>       ─  preparing 'syncdr': (21.3s)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (606ms)
#>       ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'syncdr_0.0.2.9001.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/wb621604/AppData/Local/Temp/RtmpWaTr9Q/temp_libpath27ec7ad3308a'
#> (as 'lib' is unspecified)
```

## Usage example

``` r

library(syncdr)

# Generate toy directories to show package usage

# --- Create .syncdrenv --- #
.syncdrenv <- toy_dirs()
#> ■■■■■■■ 20% | ETA: 8s■■■■■■■■■■■■■■■ 47% | ETA: 5s■■■■■■■■■■■■■■■■■ 53% | ETA:
#> 5s■■■■■■■■■■■■■■■■■■■ 60% | ETA: 4s■■■■■■■■■■■■■■■■■■■■■■■■■■■ 87% | ETA: 1s
left       <- .syncdrenv$left
right      <-  .syncdrenv$right

# --- Compare synchronization status of the two directories --- #
display_dir_tree(path_left  = left,
                 path_right = right)
#> (←)Left directory structure:
#> C:/Users/wb621604/AppData/Local/Temp/Rtmp6vU5MR/left
#> ├── A
#> │   ├── A1.Rds
#> │   ├── A2.Rds
#> │   └── A3.Rds
#> ├── B
#> │   ├── B1.Rds
#> │   ├── B2.Rds
#> │   └── B3.Rds
#> ├── C
#> │   ├── C1.Rds
#> │   ├── C2.Rds
#> │   └── C3.Rds
#> ├── D
#> │   ├── D1.Rds
#> │   └── D2.Rds
#> └── E
#> (→)Right directory structure:
#> C:/Users/wb621604/AppData/Local/Temp/Rtmp6vU5MR/right
#> ├── A
#> ├── B
#> │   ├── B1.Rds
#> │   └── B2.Rds
#> ├── C
#> │   ├── C1.Rds
#> │   ├── C1_duplicate.Rds
#> │   ├── C2.Rds
#> │   └── C3.Rds
#> ├── D
#> │   ├── D1.Rds
#> │   ├── D2.Rds
#> │   └── D3.Rds
#> └── E
#>     ├── E1.Rds
#>     ├── E2.Rds
#>     └── E3.Rds

# comparing by date of last modification
compare_directories(left_path   = left,
                    right_path  = right)
#> 
#> ── Synchronization Summary ─────────────────────────────────────────────────────
#> • Left Directory: 'C:/Users/wb621604/AppData/Local/Temp/Rtmp6vU5MR/left'
#> • Right Directory: 'C:/Users/wb621604/AppData/Local/Temp/Rtmp6vU5MR/right'
#> • Total Common Files: 7
#> • Total Non-common Files: 9
#> • Compare files by: date
#> 
#> ── Common files ────────────────────────────────────────────────────────────────
#>             path modification_time_left modification_time_right  modified
#> 1 /left/B/B1.Rds    2025-12-11 11:00:54     2025-12-11 11:00:55     right
#> 2 /left/B/B2.Rds    2025-12-11 11:00:57     2025-12-11 11:00:58     right
#> 3 /left/C/C1.Rds    2025-12-11 11:00:55     2025-12-11 11:00:55 same date
#> 4 /left/C/C2.Rds    2025-12-11 11:00:58     2025-12-11 11:00:59     right
#> 5 /left/C/C3.Rds    2025-12-11 11:01:00     2025-12-11 11:01:01     right
#> 6 /left/D/D1.Rds    2025-12-11 11:00:57     2025-12-11 11:00:56      left
#> 7 /left/D/D2.Rds    2025-12-11 11:01:00     2025-12-11 11:00:59      left
#> 
#> ── Non-common files ────────────────────────────────────────────────────────────
#> 
#> ── Only in left ──
#> # A tibble: 4 × 1
#>   path_left     
#>   <fs::path>    
#> 1 /left/A/A1.Rds
#> 2 /left/A/A2.Rds
#> 3 /left/A/A3.Rds
#> 4 /left/B/B3.Rds
#> ── Only in right ──
#> # A tibble: 5 × 1
#>   path_right               
#>   <fs::path>               
#> 1 /right/C/C1_duplicate.Rds
#> 2 /right/D/D3.Rds          
#> 3 /right/E/E1.Rds          
#> 4 /right/E/E2.Rds          
#> 5 /right/E/E3.Rds

# --- Perform synchronization action --- #

# asymmetric snchronization from left to right 
 full_asym_sync_to_right(left_path  = left,
                         right_path = right,
                         force      = FALSE)
#> These files will be DELETED in right
#> 
#> |Files               |Action        |
#> |:-------------------|:-------------|
#> |/C/C1_duplicate.Rds |To be deleted |
#> |/D/D3.Rds           |To be deleted |
#> |/E/E1.Rds           |To be deleted |
#> |/E/E2.Rds           |To be deleted |
#> |/E/E3.Rds           |To be deleted |
#> These files will be COPIED (overwriting if present) to right 
#> 
#> 
#> |Files     |Action       |
#> |:---------|:------------|
#> |/D/D1.Rds |To be copied |
#> |/D/D2.Rds |To be copied |
#> |/A/A1.Rds |To be copied |
#> |/A/A2.Rds |To be copied |
#> |/A/A3.Rds |To be copied |
#> |/B/B3.Rds |To be copied |
#> Do you want to proceed? Type your answer (Yes/no/cancel) 
#> ✔ synchronized
```

# syncdr 0.1.1

# syncdr 0.1.0

Initial CRAN release.

### Directory comparison
- Added `compare_directories()` to compare two directories by modification date, file content, or date-then-content.
- Detects newer/older/same-date files as well as files present only in one directory.
- Added `print.syncdr_status()` for formatted summaries of comparison results.

### Visualization
- Added `display_sync_status()` for interactive visualization of comparison output.
- Added `display_dir_tree()` to show directory trees for one or two paths.

### Asymmetric synchronization (left → right)
- Added `full_asym_sync_to_right()` for complete one-way synchronization.
- Added partial sync helpers:
  - `common_files_asym_sync_to_right()`
  - `update_missing_files_asym_to_right()`
  - `partial_update_missing_files_asym_to_right()`

### Symmetric synchronization
- Added `full_symmetric_sync()` for two-way synchronization with conflict handling.
- Added `partial_symmetric_sync_common_files()` for partial symmetric updates.

### Utilities
- Added `toy_dirs()` to create reproducible example directory structures.
- Added `copy_temp_environment()` for safe testing of directory operations.
- Added `search_duplicates()` for content-based duplicate detection.
- Added `save_sync_status()` to persist comparison results.

### Documentation and tests
- Added introductory and workflow vignettes.
- Added unit tests covering comparison logic, synchronization functions, directory-tree display, and duplicate detection.

# syncdr 0.1.2

## Security and robustness improvements

This release improves input validation, path handling, and error recovery across
synchronization functions following an internal review. No new user-facing features
are added.

## Path handling improvements

- Improved internal path normalization and relative path computation to ensure
  consistent behavior across platforms and working directories.
- Reduced risk of incorrect path resolution when directory names contain special
  characters.

## Input validation

- Added stricter validation for directory inputs to ensure they are valid, existing
  directories.
- Prevented use of identical or nested directories in synchronization operations.
- Added validation to ensure sync status objects are of the expected class.
- Added validation to prevent backup directories from overlapping with
  synchronization targets.

## Backup and recovery improvements

- Centralized backup handling to improve consistency across synchronization
  functions.
- Improved verification of backup completion before modifications proceed.
- Enhanced backup directory naming to avoid overwriting previous backups.
- Added warning when temporary directories are used for backups.

## Error handling improvements

- Improved handling of file permission issues with early checks before file
  operations.
- Enhanced resilience to individual file copy or deletion failures.
- Improved handling of files that change during synchronization.

## Staleness detection

- Added timestamp tracking to synchronization status objects.
- Added warning when synchronization results may be outdated (configurable via
  `options(syncdr.staleness_threshold_secs)`).

## API changes

- Default value of `force` changed from `TRUE` to `FALSE` in all sync functions.
- Default value of `delete_in_right` changed from `TRUE` to `FALSE`.
- `overwrite` is now an explicit user-controllable argument (default `TRUE`).

## Documentation

- Improved documentation of synchronization parameters.
- Added package-level note on concurrency limitations and recommended use of file
  locking packages.

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

## Test environments

- Local macOS, R 4.6.2
- win-builder, R-devel (2026-11-17 r90926)

## R CMD check results

Local macOS:
0 errors | 0 warnings | 0 notes

Win-builder (R-devel):
0 errors | 0 warnings | 0 notes

## Changes since last CRAN release

This update (0.5.1) includes improvements to RxClass relationship handling,
cache management, API error handling, documentation, and package vignettes.

Notable changes include:

- Added support for querying multiple relationships in `get_class_members()`,
preserving the originating relationship in the output.
- Improved documentation and warnings for RxClass relationship queries.
- Fixed incorrect parsing of relationship lists in `rxclass_relas()`.
- Added cache management utilities for inspecting, clearing, and configuring
API response caching.
- Improved consistency of API error handling and informative warnings.

## Internet resources

`rxref` queries the U.S. National Library of Medicine's 'RxNorm'/'RxNav' and
'RxClass' APIs.

Live API tests are skipped by default during package checks and are only run
when explicitly enabled. Package vignettes use precomputed example datasets
and can be built without live API access. Regeneration of these example
datasets is handled separately during package development.

API failures are handled through informative package-specific errors or
warnings, as appropriate.

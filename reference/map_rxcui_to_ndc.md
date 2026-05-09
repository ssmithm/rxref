# Map RxCUIs to NDCs

Convenience wrapper around
[`map_to()`](https://www.stevenmsmith.org/rxref/reference/map_to.md) for
mapping RxNorm Concept Unique Identifiers (RxCUIs) to National Drug
Codes (NDCs).

## Usage

``` r
map_rxcui_to_ndc(
  x,
  status = NULL,
  history = c("active", "direct", "all"),
  show_progress = interactive()
)
```

## Arguments

- x:

  Character vector of RxCUIs.

- status:

  Optional character vector of NDC statuses to retain, such as
  `"ACTIVE"`, `"OBSOLETE"`, `"ALIEN"`, or `"UNKNOWN"`. If `NULL`, no
  status-based filtering is applied to the retrieved NDCs. Note that
  `history = "active"` uses RxNorm's active NDC endpoint, so returned
  NDCs are expected to be active even when `status = NULL`.

- history:

  For `to = "ndc"` only, the NDC association history to retrieve. One of
  `"active"`, `"direct"`, or `"all"`.

  - `"active"` retrieves currently active NDCs associated with the
    supplied RxCUI.

  - `"direct"` retrieves NDCs ever directly associated with the supplied
    RxCUI.

  - `"all"` retrieves NDCs ever directly or indirectly associated with
    the supplied RxCUI, including associations through remapped or
    archived concepts.

  If `NULL` and `to = "ndc"`, defaults to `"active"`. Ignored when
  `to = "rxcui"`.

- show_progress:

  Logical; if `TRUE`, show a progress bar for vectorized mapping
  operations. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A tibble with one row per RxCUI/NDC mapping.

For `history = "active"`, returns columns:

- rxcui:

  Input RxCUI.

- ndc11:

  Mapped NDC.

- ndc_status:

  NDC status returned by RxNorm, when available.

For `history = "direct"` or `"all"`, the output may also include:

- related_rxcui:

  RxCUI associated with the historical NDC record. This may differ from
  the input RxCUI when historical or indirect associations are
  retrieved.

- ndc_start_date:

  Start date of the NDC association, when available.

- ndc_end_date:

  End date of the NDC association, when available.

## Details

By default, this function retrieves currently active NDCs associated
with the supplied RxCUIs. Use `history = "direct"` or `history = "all"`
to retrieve historical NDC associations.

`status = NULL` means that no status filter is applied after NDCs are
retrieved. It does not, by itself, request historical NDCs. To retrieve
historical NDCs, set `history = "direct"` or `history = "all"`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Current active NDCs
map_rxcui_to_ndc("1049630")

# NDCs ever directly associated with this RxCUI
map_rxcui_to_ndc("1049630", history = "direct")

# All historical NDCs, retaining obsolete NDCs only
map_rxcui_to_ndc(
  "1049630",
  history = "all",
  status = "OBSOLETE"
)
} # }
```

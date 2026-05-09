# Map between NDCs and RxCUIs

`map_to()` maps identifiers between National Drug Codes (NDCs) and
RxNorm Concept Unique Identifiers (RxCUIs).

## Usage

``` r
map_to(
  x,
  to = c("rxcui", "ndc"),
  status = NULL,
  history = NULL,
  show_progress = interactive()
)
```

## Arguments

- x:

  Character vector of NDCs or RxCUIs.

- to:

  Direction of mapping. One of `"rxcui"` or `"ndc"`. Use `"rxcui"` to
  map NDCs to RxCUIs and `"ndc"` to map RxCUIs to NDCs.

- status:

  For `to = "ndc"` only, optional character vector of NDC statuses to
  retain, such as `"ACTIVE"`, `"OBSOLETE"`, `"ALIEN"`, or `"UNKNOWN"`.
  If `NULL`, no status-based filtering is applied to the retrieved NDCs.
  Note that when `history = "active"`, RxNorm's active NDC endpoint is
  used, so returned NDCs are expected to be active even when
  `status = NULL`.

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

A tibble.

For `to = "rxcui"`, returns one row per input NDC/RxCUI mapping with
columns:

- input:

  Original input NDC.

- ndc11:

  Normalized 11-digit NDC.

- rxcui:

  Mapped RxCUI, if found.

For `to = "ndc"` and `history = "active"`, returns one row per RxCUI/NDC
mapping with columns:

- rxcui:

  Input RxCUI.

- ndc11:

  Mapped NDC.

- ndc_status:

  NDC status returned by RxNorm, when available.

For `to = "ndc"` and `history` equal to `"direct"` or `"all"`, the
output may also include:

- related_rxcui:

  RxCUI associated with the historical NDC record. This may differ from
  the input RxCUI when historical or indirect associations are
  retrieved.

- ndc_start_date:

  Start date of the NDC association, when available.

- ndc_end_date:

  End date of the NDC association, when available.

## Details

When mapping from NDC to RxCUI, NDCs may be supplied as 10-digit,
11-digit, or hyphenated values. Input NDCs are normalized to 11-digit
format before querying RxNorm.

When mapping from RxCUI to NDC, the `history` argument controls whether
only currently active NDCs are retrieved or whether historical NDC
associations are also included.

RxNorm distinguishes between currently active NDCs and historical NDC
associations. The default behavior, `history = "active"`, uses RxNorm's
active NDC endpoint and preserves the earlier behavior of `map_to()`.

To retrieve obsolete, discontinued, or otherwise historical NDCs, use
`history = "direct"` or `history = "all"`. Setting `status = NULL` does
not by itself request historical NDCs; it only means that no status
filter is applied after NDCs are retrieved.

## Examples

``` r
if (FALSE) { # \dontrun{
# Map an NDC to RxCUI
map_to("00093-7424-56", to = "rxcui")

# Map an RxCUI to currently active NDCs
map_to("1049630", to = "ndc")

# Map an RxCUI to all directly associated historical NDCs
map_to("1049630", to = "ndc", history = "direct")

# Map an RxCUI to all historical NDCs and retain obsolete NDCs only
map_to("1049630", to = "ndc", history = "all", status = "OBSOLETE")
} # }
```

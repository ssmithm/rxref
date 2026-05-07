.onLoad <- function(libname, pkgname) {
  op <- options()

  op.rxref <- list(
    rxref.base_url = "https://rxnav.nlm.nih.gov/REST",
    rxref.rxclass_base_url = "https://rxnav.nlm.nih.gov/REST/rxclass",
    rxref.user_agent = sprintf(
      "rxref/%s (+https://github.com/ssmithm/rxref)",
      utils::packageVersion("rxref")
    ),
    rxref.cache = cachem::cache_mem(),
    rxref.rate_delay = 0.1
  )

  toset <- !(names(op.rxref) %in% names(op))
  if (any(toset)) options(op.rxref[toset])

  invisible()
}

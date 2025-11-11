.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rxref <- list(
    rxref.base_url  = "https://rxnav.nlm.nih.gov/REST",
    rxref.user_agent = sprintf("rxref/%s (+https://github.com/ssmithm/rxref)", utils::packageVersion("rxref")),
    # FIX: use path=
    rxref.cache = memoise::cache_filesystem(path = tools::R_user_dir("rxref", which = "cache")),
    rxref.rate_delay = 0.1
  )
  toset <- !(names(op.rxref) %in% names(op))
  if (any(toset)) options(op.rxref[toset])
  invisible()
}

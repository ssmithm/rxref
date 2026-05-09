---
title: "cran-comments"
output: html_document
---

## Test environments

* local macOS, R 4.5.3
* win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

This is a new submission.

The DESCRIPTION contains domain-specific terms related to RxNorm and National
Drug Codes, including RxNorm, RxNav, RxCUIs, NDC, and NDCs.

## Internet resources

rxref queries the NLM RxNorm/RxNav and RxClass APIs. Live API tests and
vignette rebuilding are skipped by default and only run when explicitly enabled
with environment variables. Offline examples use precomputed data included in
inst/extdata. API failures are handled with informative package-specific errors.

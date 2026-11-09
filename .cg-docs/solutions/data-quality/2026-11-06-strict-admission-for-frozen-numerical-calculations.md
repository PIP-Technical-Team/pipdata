---
date: 2026-11-06
title: "Strict admission before frozen numerical calculations"
category: "data-quality"
language: "R"
tags: [admission, factors, partial-matching, underflow, fingerprints]
root-cause: "Type-only checks and partial list lookup admitted malformed representations, hidden S3 dispatch, row recycling and underflow into a supposedly frozen calculation."
severity: "P0"
plan: ".cg-docs/plans/2026-11-05-m2-deflate-one-survey-approved-contract.md"
reviewed-in: ".cg-docs/reviews/2026-11-05-m2-deflate-one-survey-approved-contract-review.md"
---

# Strict Admission Before Frozen Numerical Calculations

## Problem
The private M2 microdata route passed its ordinary numerical and persistence
tests, yet review found four ways to publish incorrect values or consume an
undeclared method. A frozen function environment did not make its inputs safe.

| Trigger | Why the original checks missed it |
|---|---|
| Character matrix with two vintages for one year | Character type did not establish vector shape; duplicate detection compared matrix rows, allowing the same output column to be overwritten. |
| Factor codes containing zero | Indexing levels dropped zero positions, shortened area vectors and silently recycled CPI values across retained rows. |
| Classed factor levels | Subsetting the levels attribute invoked a registered `[` method before the result was validated. Its body was absent from request proof. |
| Missing `cpi`, `ppp`, `pop` or `surveyid_year` | Base list `$` could partially match a suffixed backup field rather than reject missing exact metadata. |
| Positive welfare with extreme finite factors | The retained PPP-first division could underflow to zero, which a nonnegative-output check accepted. |

## Root Cause
Admission checked outer classes and numerical signs without fully checking
shape, factor representation or exact required names before generic operations.
Finite input and output checks also missed a positive value silently becoming
zero. Dependency closure therefore requires both fingerprinted code and an
input boundary that prevents unreviewed dispatch.

## Solution
Keep the one package-owned adapter in `R/pd_deflation.R`; do not broaden the
engine's namespace or S3 whitelist. The full implementation, not these excerpts
alone, remains authoritative.

1. Require all seven exact metadata names: `country_code`, `surveyid_year`,
   `survey_acronym`, `welfare_type`, `cpi`, `ppp`, `pop`. Reject malformed or
   duplicate names before lookup. A backup field must not satisfy membership.
2. Reject selection dimensions before year extraction or duplicate detection.
   One selected vintage per year is a vector contract, not just a type check.
3. Before decoding any factor, require canonical attributes, plain
   attribute-free character levels with no duplicates/missing labels, and
   integer codes within bounds. Missing area codes fail the later coverage
   check. Require decoded area length to equal welfare row count.
4. Preserve genuine welfare zeros, but reject positive-input/zero-output
   underflow before publication. Keep the retained arithmetic unchanged.

The numerical trigger is independently reproducible without loading packages:

```r
welfare <- 1e-300
ppp <- 1e100
cpi <- 1e-100
stopifnot(welfare / ppp / cpi == 0)
stopifnot(welfare / cpi / ppp > 0)
```

The private base-only boundary applies this additional check after its existing
finite/nonnegative result validation:

```r
if (any(data$welfare > 0 & value == 0)) {
  stop("bounded positive welfare underflowed to zero")
}
```

`pd_deflation_adapter()` in `R/code_fingerprint.R` freezes the actual seven
calculation/admission/domain/population helpers, registry, external arithmetic
and fixed namespace boundary. Their implementations enter request components.
Changing a helper must change an actual stamp request, not merely a unit hash.

## Prevention
- Prove rejection before dispatch: the frozen-adapter factor probe must report
  zero registered-method calls, not merely an eventual exception.
- Pair malformed-input unit tests with saved no-successful-version checks.
  An error alone does not prove that publication did not already happen.
- Use independent literal oracles, including a year outside the four original
  vintages, consumed CPI changes and isolated unused-year/domain controls.
- Keep CPI, PPP and population domains independent. The bounded primary keeps
  weights unchanged; area controls use exact-year domain shares, never `hsize`.
- Preserve daily LCU and row identity. Input welfare is already LCU/day; there
  is no additional `/365` or currency/spatial multiplier.
- Scope the rule: public legacy `pd_deflation()` still has its existing output
  contract. Private M2 retains `welfare`, `welfare_lcu`, dynamic `welfare_YYYY`
  and input order; it does not supersede legacy naming or sorting rules.

## Verified Evidence
The completed November 5 implementation recorded 125 blocks / 831 passing
expectations across the six selected files, zero failures/errors/warnings/skips,
27 invalid no-save cases, and final light verification after seven review fixes.
This capture does not rerun those tests or claim production/full-package safety.
The underflow guard addresses zero loss, not every floating-point precision
problem. Exact run, request, receipt and source identities remain in the linked
execution and verification reports.

## Related
- [Completed execution report](../../work-reports/2026-11-05-m2-deflate-one-survey-approved-contract.md)
- [Final verification](../../reviews/2026-11-05-m2-deflate-one-survey-approved-contract-verify-review.md)
- [Private calculation and admission implementation](../../../R/pd_deflation.R)
- [Actual dependency proof](../../../R/code_fingerprint.R)
- [Public legacy output contract](2026-05-07-deflation-output-contract.md)
- [Result-bound provenance checkpoints](2026-08-25-authoritative-staged-provenance-checkpoints.md)
- [Contained R preflight and parent preparation](../environment-issues/2026-11-06-contained-r-preflight-and-certified-parents.md)

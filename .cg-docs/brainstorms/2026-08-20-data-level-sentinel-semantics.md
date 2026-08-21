---
date: 2026-08-20
title: "Explicit data_level sentinel semantics (column-lookup registry)"
status: decided
chosen-approach: "Approach 4 - Column-lookup registry"
participants:
  - wb384996
tags: [deflation, data-level, attribute-semantics, subnational, ppp, cpi, population]
---

# Explicit `*_data_level` sentinel semantics

## Context

The `ppp_data_level`, `cpi_data_level`, and `pop_data_level` attributes currently
carry **dual semantics** in a single string:

- The literal value `"national"` (or any other non-`"area"` string) means
  "broadcast this scalar level value across all rows of `dt`."
- The value `"area"` is an **implicit pointer** to the `area` *column* of `dt`,
  meaning "look up the per-row level value (e.g. `"rural"`, `"urban"`) in the
  `area` column."

This convention is implicit — a reader must know the magic string `"area"` to
distinguish pointer from literal. It is also error-prone: the original
`add_rep_lvl()` scalar-broadcast bug (removed 2026-05-07; see
`R/pd_deflation.R:549-553`) was precisely caused by treating `"area"` as a
literal when it was a pointer. The fast-fix that restored per-row lookup
(`add_ppp()`/`add_cpi()`/`adjust_population()` each branching on
`identical(*_lvl, "area")`) resolved the bug but kept the convention implicit.

This brainstorm decides how to make the pointer-vs-literal distinction
**explicit and enforced by code**, while minimizing changes across the
producer (`add_dom_vars()`), the auxiliary-vector builder (`pd_aux_attr()`),
the three consumers (`add_ppp()`, `add_cpi()`, `adjust_population()`), and
the test suite.

## Requirements

1. Make the `"area"` column-pointer sentinel explicit and discoverable (no
   magic strings scattered across `pd_deflation.R`).
2. Enforce the pointer-vs-literal distinction in code, not only in comments.
3. Preserve the existing scalar-broadcast fast path for `"national"` (no
   per-row overhead when all rows share one level).
4. Minimize blast radius across `add_dom_vars()`, `pd_aux_attr()`,
   `add_ppp()`, `add_cpi()`, `adjust_population()`, and their tests.
5. Keep the producer (`add_dom_vars()`) emitting the same string value
   `"area"` so existing test fixtures and the `pipdata.R` attribute whitelist
   (`pipdata.R:40-61`) do not change.
6. Generalize cleanly if a second column-pointer sentinel (e.g. `region`)
   ever appears.

## Approaches Considered

### Approach 1: Structured encoding `list(column = "area")`

Attr becomes a list; pointer-ness is structural (list vs scalar string).

**Pros**: Truly eliminates the dual semantics by type. Extensible
(`values = c("rural", "urban")` field optional).
**Cons**: Heaviest blast radius — changes the attr *type* everywhere it is
read or printed; `make_pipmd()` helper + ~8 test call sites must pass a list;
any future `identical(attr, "national")` check silently breaks.
**Effort**: Medium

### Approach 2: Always store resolved per-row level values

Drop the sentinel; store a row-aligned vector of resolved levels (or a scalar
when uniform).

**Pros**: No sentinel at all.
**Cons**: Breaks the scalar-broadcast fast path; the attribute becomes
row-aligned state that must be re-synchronized on every subset/reorder,
introducing a new class of alignment bugs. Consumers must branch on
`length() > 1` instead of `"area"`, relocating complexity rather than removing
it.
**Effort**: Large

### Approach 3: Package constant `.DATA_LEVEL_AREA`

Define `.data_level_area <- "area"` in a constants file with a roxygen block
documenting it as the sole column-pointer sentinel. Replace the 3
`identical(*, "area")` consumer sites and the 3 producer sites in
`add_dom_vars()` with the constant.

**Pros**: Smallest possible change (1 constant + 6 sites). Zero test edits
because the constant equals the existing string. Centralizes the magic
string and makes `?constants` the single source of truth.
**Cons**: **Documents** the dual semantics rather than *removing* them — a
future reader still must learn that `.data_level_area` means "look up the
`area` column." Reduces the confusion (named + documented, typo-proof) but
does not structurally eliminate it.
**Effort**: Small

### Approach 4: Column-lookup registry (CHOSEN)

A package-level registry mapping sentinel -> column name, plus one resolver:

```r
.data_level_columns <- list(area = "area")
data_level_column <- function(lvl) {
  col <- .data_level_columns[[lvl]]
  if (is.null(col)) NA_character_ else col
}
```

Consumers become:

```r
ppp_lvl <- attr(dt, "ppp_data_level")
col <- data_level_column(ppp_lvl)
if (!is.na(col)) {                            # pointer sentinel
  if (!col %in% names(dt)) cli::cli_abort(...)
  dt[, (v) := lev_map[as.character(dt[[col]])]]
} else {                                      # literal level value
  dt[, (v) := lev_map[ppp_lvl]]
}
```

Producer (`add_dom_vars()`) is unchanged — it still emits the string `"area"`.

**Pros**: Converts the implicit pointer convention into an **explicit,
code-enforced** one: only registry keys are pointers, everything else is a
literal level value. Same blast radius as Approach 3 (3 consumer sites + 1
registry; producer untouched; zero test edits because the attr value is
still the string `"area"`). Generalizes cleanly if a second sentinel
appears: add one registry line, no consumer logic changes.
**Cons**: One extra indirection (function call) for a single current sentinel
— mild YAGNI unless more sentinels are plausible. Slightly more to read
than a bare constant.
**Effort**: Small

## Decision

**Chosen: Approach 4 - Column-lookup registry.**

Approach 4 is within ~1 line of Approach 3's blast radius (both leave
`pd_aux_attr()`, the test fixtures, the `add_dom_vars()` producer, and the
legacy `data.table` merge paths untouched — only the 3
`identical(*, "area")` consumer sites in `R/pd_deflation.R` change), but
unlike Approach 3 it makes the sentinel-vs-literal distinction **enforced by
code** rather than merely documented, directly addressing the "implicit and
error-prone" complaint that prompted this brainstorm.

Approach 1 is the correct *structural* elimination but pays the medium
test/helper churn the task explicitly asked to avoid. Approach 2 is not
recommended (breaks the scalar fast path and introduces row-alignment risk).
Approach 3 is the acceptable smaller fallback if a future reviewer prefers
the absolute minimum change surface.

**Trade-offs accepted**: one indirection layer for a single sentinel today;
the registry is the extension point if sub-regional sentinels appear later.

## Consequences

- The dual semantics are now **declared** in one place
  (`.data_level_columns`) and **applied** through one resolver
  (`data_level_column()`), rather than scattered across three
  `identical(*, "area")` checks.
- Adding a future column-pointer sentinel (e.g. `region`) requires one
  registry line; no consumer logic changes.
- The producer (`add_dom_vars()`) and the auxiliary-vector builder
  (`pd_aux_attr()`) are intentionally **not** touched, so the attr value
  remains the string `"area"` and existing test fixtures pass unchanged.
- The attribute type remains `character` — no serialization or
  whitelist changes.
- A reader unfamiliar with the registry still needs to consult
  `data_level_column()` to learn the pointer convention; this is strictly
  better than the status quo (scattered magic strings) but not as
  self-evident as a structural `list(column = ...)` encoding (Approach 1).

## Next Steps

Handoff to `/cg-plan`:

1. Add `.data_level_columns` registry + `data_level_column()` resolver in a
   new `R/constants.R` (or top of `R/pd_cpfw_merge.R`). Keep internal only
   (do not `@export`).
2. Rewrite the 3 consumer branches in `R/pd_deflation.R`:
   - `:395` (`adj_pop` guard in `.deflation_pipmd_core`),
   - `:640` (pointer branch in `add_ppp()`),
   - `:716` (pointer branch in `add_cpi()`),
   to use `data_level_column()`; keep the existing
   "area column absent" aborts, now keyed off the resolved column name
   rather than the hard-coded `"area"`.
3. Leave `adjust_population()` body unchanged — it is already pre-guarded
   by the `adj_pop` check at `:395` and never inspects the sentinel itself.
4. Leave `add_dom_vars()` and `pd_aux_attr()` untouched.
5. Add a test asserting
   `data_level_column("area") == "area"` and
   `is.na(data_level_column("national"))` to lock the registry contract.
6. Run `devtools::test()` and confirm no test regressions
   (only test edits are additive; existing `ppp_data_level = "area"`
   fixtures continue to pass because the attr value is unchanged).

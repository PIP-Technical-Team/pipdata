---
plan: .cg-docs/plans/2026-05-08-verbose-option-propagation.md
date: 2026-06-01
depth: standard
findings:
  P2.1: open
  P2.2: open
  P2.3: fixed
  P2.4: fixed
  P3.1: fixed
  P3.2: fixed
  P3.3: open
---

## Review Report

**Review depth**: standard
**Files reviewed**: 13 (`R/pd_deflation.R`, `R/pd_process_data.R`, `R/pipdata_copy_dlw_meta.R`, `R/pipdata_dlw_process.R`, `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `R/save_pip.R`, `R/valid_aux_load.R`, `R/valid_dlw_load.R`, `R/pipdata-options.R`, `Pipdata_script.R`, `compound-gpid.context.md`, `tests/testthat/test-pd-deflation.R`)
**Findings**: 7 (P0: 0, P1: 0, P2: 4, P3: 3)

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `tests/testthat/test-pd-deflation.R:~587` — The `pd_deflation(verbose = FALSE)` test only covers **Mode B** (load by `pip_id`). Mode A (pass `dt` directly, metadata loaded from inventory) propagates `verbose` to `.load_deflation_aux()` independently and has no verbose-propagation test.
  **Why**: Mode A resolves `pip_id` from survey attributes and calls `.load_deflation_aux(pip_id, version, verbose = verbose)`. A future refactor could silently break Mode A propagation while Mode B tests still pass.
  **Fix**: Add a test using `pd_deflation(dt = make_pipmd(...), verbose = FALSE)` with mocked `pipload` to capture the `verbose` value received by `.load_deflation_aux()` → `load_pip_master_inventory()`.

- **[P2.2]** [cg-testing] `R/valid_aux_load.R` — No test covers that `valid_aux_load(verbose = FALSE)` propagates `verbose = FALSE` to both `pipaux::compare_aux_releases()` and `pipaux::compare_aux_vintages()`. The `valid_dlw_load()` → `valid_aux_load()` verbose fix (Phase 2) is also untested.
  **Why**: Without a test, both the Phase 2 fix and the verbose contract in `valid_aux_load()` can silently regress.
  **Fix**: In the relevant test file, add a test with `local_mocked_bindings(pipaux::compare_aux_releases = ..., pipaux::compare_aux_vintages = ...)` that captures the received `verbose` argument and asserts `FALSE`.

- **[P2.3]** [cg-documentation] `R/pd_deflation.R:~66-70` — The `@param verbose` roxygen entry for `.load_deflation_aux()` is positioned after `@return`, breaking standard roxygen2 param ordering (`@param` before `@return`).
  **Why**: Non-standard ordering sets a poor template for future promoted functions and can cause confusing rendering if the function is later exported.
  **Fix**: Move the `@param verbose` doc line to appear before `@return`.

- **[P2.4]** [cg-architecture] `R/pd_process_data.R:~128` — `build_pip_inventory()` is called without a `verbose` parameter; the function uses `verbose = FALSE` internally. The design decision (batch-internal, always silent) is not documented at the call site.
  **Why**: Future maintainers may add `verbose` to `build_pip_inventory()` without realising the silence was intentional. Silent intent should be explicit.
  **Fix**: Add a comment at the call site: `# verbose not propagated — build_pip_inventory() is batch-internal and uses verbose = FALSE unconditionally (design decision 7)`.

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-documentation] `R/pipdata-options.R` — `@aliases pipdata.verbose` generates a spurious `\alias{pipdata.verbose}` in the `.Rd`; option names are not R objects and should not be aliased this way.
  **Fix**: Remove the `@aliases pipdata.verbose` line.

- **[P3.2]** [cg-documentation] `R/pipdata_validate_gmd.R` — `@param verbose` says "downstream `[pipload::pip_write()]` calls" but the function makes other I/O calls too. Wording implies pip_write is the only downstream call.
  **Fix**: Change to: "Controls verbosity of downstream I/O calls (including [pipload::pip_write()])."

- **[P3.3]** [cg-version-control] `.cg-docs/solutions/testing-patterns/2026-05-29-expect-warning-returns-condition-not-value.md` is untracked. This solution document from the session should be committed with the other changes.
  **Fix**: `git add .cg-docs/solutions/testing-patterns/2026-05-29-expect-warning-returns-condition-not-value.md` and include in the commit.

### ✅ Passed

- **cg-code-quality**: `verbose = getOption("pipdata.verbose", default = TRUE)` pattern consistent across all 9 exported functions. No internal helper calls `getOption()` directly. `.Rbuildignore` already excludes `.cg-docs/`. No debug code, no hardcoded paths, no secrets.
- **cg-reproducibility**: No seeds, no absolute paths, no lockfile changes needed.
- **cg-performance**: Verbose resolved once at exported function boundary — no per-iteration `getOption()` calls.
- **cg-data-quality**: No data transformation code changed; verbose propagation does not affect pipeline results.

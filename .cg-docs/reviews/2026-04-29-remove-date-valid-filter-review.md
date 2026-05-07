---
plan: .cg-docs/plans/2026-04-29-remove-date-valid-filter.md
findings:
  P1.1: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P3.1: fixed
  P3.2: skipped
  P3.3: fixed
---

## Review Report

**Review depth**: standard  
**Files reviewed**: 6 (`R/update_pip_inventory.R`, `R/valid_dlw_load.R`, `R/aaa.R`, `man/update_pip_inventory.Rd`, `tests/testthat/test-logging-integration.R`, `roadmap.json`)  
**Findings**: 8 (P0: 0, P1: 1, P2: 4, P3: 3)

---

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-architecture] `R/update_pip_inventory.R:204-211` — Release write failure emits `logmeta$error = "inv_update_inf"`, the same discriminator used by the inventory verification block (lines 263–291). `log_report()` uses `"inv_update_inf"` to build the verification section — a release write failure would appear there with missing/wrong fields (`n_expected`, `n_confirmed`, etc.), breaking report rendering.  
  **Why**: `parse_log_meta()` uses the discriminator string as the sole type key. Two structurally different events sharing one key will produce malformed report sections.  
  **Fix**: Use a distinct key for the release write failure — e.g. `error = "release_write_err"` — and add it to `.log_internal_types` in `aaa.R` and the `@details` doc.

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-data-quality] `R/update_pip_inventory.R:234-262` — When `release_vid` is `NA_character_` (release write failed and `st_latest()` also failed), the column-initialization block inside the guard never runs. `new_pip_inv` is saved to master **without** `first_release_version_id` or `latest_release_version_id` columns. This produces an inconsistent schema across master inventory versions.  
  **Why**: `collapse::rowbind(..., fill = TRUE)` handles missing columns gracefully on load, but downstream consumers get varying schemas and the columns are permanently absent until the next successful release write.  
  **Fix**: Initialize both columns unconditionally (as `NA_character_`) **before** the guard, then only populate them inside it.

- **[P2.2]** [cg-testing] `tests/testthat/test-logging-integration.R:175` — `.apply_release_vid()` helper duplicates the production column-population logic from `update_pip_inventory()`. If production code changes, these tests silently pass against a stale copy.  
  **Why**: Test helpers that mirror production logic rather than calling it create false confidence.  
  **Fix**: Export or `@keywords internal` the helper into the package as a standalone function (e.g. `.populate_release_vid_cols()`), then both production code and tests call the same function. Alternatively, annotate the test with a `# mirrors: update_pip_inventory.R:L234-262` comment as a maintenance reminder.

- **[P2.3]** [cg-reproducibility] `R/update_pip_inventory.R:221` — `stamp::st_latest("pip_release_inventory.qs2", alias = "pip_inv")` hardcodes the `.qs2` extension. If the default stamp format is ever changed or the artifact is saved with a different format, this silently returns `NA` (no version found).  
  **Why**: The artifact name used in `pip_write()` is `"pip_release_inventory"` without extension — stamp normalises it. Adding `.qs2` here makes it format-dependent.  
  **Fix**: Drop the extension: `stamp::st_latest("pip_release_inventory", alias = "pip_inv")`.

- **[P2.4]** [cg-documentation] `R/update_pip_inventory.R:27-35` — The `@details` Logging section does not mention the `release_write_err` emission point (to be added with P1.1). Readers of the docs get an incomplete logging contract.  
  **Why**: Logging contract documentation in roxygen should match all emission points.  
  **Fix**: After resolving P1.1, add a bullet for `release_write_err` to the `@details` Logging list.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/update_pip_inventory.R:215` — `if (!is.null(release_vid) && !is.na(release_vid))` — the `!is.null()` check is redundant. The resolution block always produces a character scalar or `NA_character_`, never `NULL`.  
  **Fix**: Simplify to `if (!is.na(release_vid))`.

- **[P3.2]** [cg-code-quality] `R/update_pip_inventory.R:244-252` — Two separate `:=` passes for the two column assignments. Minor cosmetic note; correctness unaffected.  
  **Fix** (optional): Combine into one scoped `:=` if readability improves.

- **[P3.3]** [cg-documentation] `compound-gpid.context.md` — The canonical logmeta types list documents `"inv_update_inf"` without mentioning the new `release_write_err` type. After P1.1 is resolved, update the context doc.

---

### ✅ Passed

- **cg-code-quality**: No style, naming, or DRY issues beyond P3 notes
- **cg-version-control**: Branch correct, no secrets, `.Rbuildignore` already covers `.cg-docs/`
- **cg-performance**: `collapse::rowbind`/`funique` appropriate; no regressions
- **cg-testing**: 3 new tests cover happy path, repeat-run, and non-release scenarios; 30 tests passing

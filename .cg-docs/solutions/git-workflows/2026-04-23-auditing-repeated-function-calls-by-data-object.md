---
date: 2026-04-23
title: "Auditing repeated function calls by tracing data objects, not call counts"
category: "git-workflows"
language: "R"
tags: [refactoring, audit, call-graph, survey_id_to_vars, pipload, data-flow]
root-cause: "Counting grep matches for a function name overstates redundancy — the same function called 3 times on 3 different data objects is not redundant"
severity: "P3"
---

# Auditing repeated function calls by tracing data objects, not call counts

## Problem

A `survey_id_to_vars` audit was requested to check if the function was
called redundantly across the pipdata pipeline. A naive grep found 3+
call sites and the assumption was that some were unnecessary duplicates.

## Root Cause

Auditing by call count (grep) conflates two different questions:
- "Is this function called multiple times?" — answered by grep
- "Is this function called multiple times **on the same data**?" — answered only by tracing data flow

The actual question for redundancy is the second one. A function called
3 times on 3 independent data objects is not redundant at all.

## Solution

**Trace the data object at each call site, not just the call.**

For each call site, determine:
1. What is the data object being passed?
2. Where did that object come from (what created it)?
3. Has `survey_id_to_vars()` already been called on an upstream version
   of that same object?

**Audit results for `pipload::survey_id_to_vars()` in pipdata:**

| Call site | File | Data object source | Redundant? |
|-----------|------|--------------------|:----------:|
| `update_dlw_inventory.R:99` | DLW inventory | PowerShell directory scan output | No |
| `pipdata_validate_gmd.R:246` | `final_inv` | Newly constructed from `bind_rows(new_inv)` | No |
| `dlw_scan_and_validate.R:293` | `final_inv` | Newly constructed from `bind_rows(new_inv)` | No |

All 3 operate on **different data objects from different sources**.
Additionally, `pipload` calls `survey_id_to_vars()` internally on survey
microdata — a completely separate data path from inventory rows.

**Conclusion:** No consolidation needed. All calls are independent.

## Prevention

When auditing "is X called too many times?":

1. **Don't stop at grep count.** Always read each call site in context.
2. **Map the data object**, not just the function:
   ```
   call site → data object → data object source → overlap with other call sites?
   ```
3. **Check for early-return guards** in the function being audited.
   `survey_id_to_vars()` has one:
   ```r
   if (all(fnames %in% names(dt))) {
     cli::cli_alert_info("variables already in data frame. return same.")
     return(dt)
   }
   ```
   This means even if a redundant call existed, it would be harmless.
4. **Distinguish inventory rows from microdata** — in pipdata, these are
   always separate data paths despite being related to the same surveys.

## Related

- [`.cg-docs/plans/2026-04-23-audit-survey-id-to-vars.md`](../../plans/2026-04-23-audit-survey-id-to-vars.md) — full audit record with decision

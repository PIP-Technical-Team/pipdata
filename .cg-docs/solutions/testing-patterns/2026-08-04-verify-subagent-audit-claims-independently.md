---
date: 2026-08-04
title: "Verify subagent audit claims independently before trusting severity or schema conclusions"
category: "testing-patterns"
language: "both"
tags: [subagent, explore-agent, verification, drift-detection, compound-refresh, hallucination, false-positive, knowledge-base-audit]
root-cause: "Parallel Explore subagents dispatched to audit .cg-docs/solutions/ for code drift produced two classes of false conclusions: (1) declaring a 'CRITICAL' bug in current code based on an outdated solution doc's claim, without checking the doc's own currency against later fixes; (2) flagging a frontmatter field as 'missing/required' without reading the actual schema reference."
severity: "P2"
---

# Verify subagent audit claims independently before trusting severity or schema conclusions

## Problem

During a `/cg-compound-refresh` run auditing all 31 `.cg-docs/solutions/*.md`
files for drift, 4 parallel `Explore` subagents were dispatched (one per
category group) to compare each solution doc's claims against current code.
Two of their conclusions were wrong:

1. **False "CRITICAL" bug claim**: A subagent reported that `R/pd_deflation.R`
   has a critical bug because it uses `version_id_metadata`, which a
   2026-05-04 solution doc claimed "does not exist" in the master inventory
   schema. Taken at face value, this would have led to "fix the code" — the
   wrong remediation.
2. **False schema-compliance claim**: A different subagent flagged 13 solution
   files as "missing the required `status` field," implying they were
   malformed and needed fixing.

## Root Cause

1. The subagent compared code against a solution doc's claim, but never
   checked whether the **doc itself** was still current. The 2026-05-04 doc
   predated a later fix (`2026-05-19-version-id-vs-metadata-version-id-in-format-vrs.md`)
   that re-added the `version_id_*` columns to the master inventory. The doc,
   not the code, was stale — but the subagent asserted the opposite because it
   never traced the chronology of related solution docs.
2. The subagent asserted a schema requirement ("`status` is required") without
   reading the actual schema reference
   (`.github/skills/cg-skill-compound-docs/references/solution-schema.md`),
   which lists the required fields as `date`, `title`, `category`, `language`,
   `tags`, `root-cause`, `severity` — `status` is not among them.

Both errors share a pattern: the subagent produced a confident, specific
technical claim without grounding it in a direct read of the authoritative
source (current code / the actual schema file) — it reasoned from a single
secondary source (an old doc, or an assumption) instead.

## Solution

After collecting the 4 subagents' findings, the orchestrating agent
independently re-verified every "CRITICAL" or "high-impact" claim before
acting on it, rather than passing subagent conclusions straight through to
the user:

```text
1. Subagent claims: "R/pd_deflation.R has a CRITICAL bug — version_id_metadata
   doesn't exist."
2. Orchestrator re-checks directly: grep_search "version_id_metadata" in
   R/build_pip_inventory.R → confirms the column IS present in the current
   schema alongside content_hash_metadata.
3. Conclusion flips: the CODE is correct; the 2026-05-04 DOC is what's stale.
   Correct remediation: archive the doc (with a superseded-by note), not
   "fix the code."
```

For the schema claim:

```text
1. Subagent claims: "13 files are missing required field 'status'."
2. Orchestrator reads .github/skills/cg-skill-compound-docs/references/
   solution-schema.md directly → required fields list does not include
   'status'.
3. Conclusion flips: false positive, dismissed and noted in the audit report
   presented to the user (not silently accepted or silently dropped).
```

## Prevention

- **Never let a subagent's severity label (`CRITICAL`, `P0`, etc.) or
  compliance claim drive an action without at least one direct, independent
  check** — grep the actual current source file, or read the actual schema/
  reference doc, before proposing a fix.
- **When a solution doc's claim contradicts current code, check the doc's
  chronology first**: search for later solution docs on the same topic
  (`.cg-docs/solutions/**/*.md` cross-references, "Related" sections, dates)
  before concluding the code is wrong. An older doc is frequently what's
  stale, not the code it originally diagnosed.
- **Spot-check at minimum every "CRITICAL"/"high-impact" claim** from a
  subagent report — this scales even when dispatching many parallel agents,
  since only the highest-severity claims need the extra verification pass.
- **Present false positives to the user explicitly** (don't just silently
  drop them) — this keeps the audit trail honest and lets the user correct
  the corrector if the orchestrator itself is wrong.

## Related

- `.cg-docs/archive/data-quality/2026-05-04-master-inventory-real-column-names.md` — the concrete solution doc that was misdiagnosed as "code is wrong" when actually the doc itself was stale; archived with a superseded-by note as the correct remediation
- `.cg-docs/solutions/bugs/2026-05-19-version-id-vs-metadata-version-id-in-format-vrs.md` — the later fix that re-added `version_id_*` columns, which made the 2026-05-04 doc's claim obsolete
- `.github/skills/cg-skill-compound-docs/references/solution-schema.md` — authoritative required-fields list; consult this directly rather than assuming/inferring schema requirements

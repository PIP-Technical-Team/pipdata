---
date: 2026-08-21
title: "Validate Authors@R through a built R package"
category: "build-errors"
language: "R"
tags: [DESCRIPTION, Authors@R, R-CMD-check, R-CMD-build, package-metadata]
root-cause: "R CMD check on the raw source directory can report derived Author and Maintainer fields as missing before package build materializes them from Authors@R."
severity: "P2"
related: []
---

# Validate `Authors@R` Through a Built R Package

## Problem

The pipfun `DESCRIPTION` file contains valid `Authors@R` metadata with an
author carrying both `aut` and `cre` roles, but running `R CMD check` directly
on the source directory reported missing `Author` and `Maintainer` fields.

## Root Cause

`Author` and `Maintainer` are derived presentation fields. R permits them to
be omitted when a suitable `Authors@R` expression is present, but the derived
fields are materialized during package build or installation. A raw source
directory check can therefore produce a misleading metadata error.

## Solution

Use the build artifact as the input to the package check:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\R.exe" CMD build .
& "C:\Program Files\R\R-4.5.2\bin\R.exe" CMD check --no-manual --as-cran pipfun_1.0.1.tar.gz
```

The source metadata should use an `Authors@R` expression such as:

```text
Authors@R: person("R.Andres", "Castaneda",
                  email = "acastanedaa@worldbank.org",
                  role = c("aut", "cre"))
```

Explicit `Author` and `Maintainer` fields are optional when this expression is
valid. If a project chooses to provide them explicitly, they should be:

```text
Author: R.Andres Castaneda [aut, cre]
Maintainer: R.Andres Castaneda <acastanedaa@worldbank.org>
```

## Prevention

- Run `R CMD build` before `R CMD check` for source packages.
- Treat `Authors@R` as the authoritative machine-readable source of authorship.
- Confirm an author has the `aut` role and exactly one maintainer has the `cre`
  role.
- Do not add redundant `Author` or `Maintainer` fields merely to silence a raw
  source-directory check without first checking the built tarball.

## Related

- R Extensions: [The DESCRIPTION file](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)

---
project-name: "Pipdata package"
team: "DECDG/GPID"
created: "2026-04-06"
last-reviewed: "2026-05-04"
---

# Pipdata Package

## Objective

This project aims to run the initial steps of the new pipeline of PIP. That is, the validation of DLW data and cleaning so it can be transformed in pip data format. The pip data will be later used for estimations of regional and global poverty and inequality indicators.

## Key Deliverables

- R package improvement

## Constraints

- R packages good standards

## Current Focus

Pipeline Alignment Audit: aligning the pipdata codebase with the canonical three-wrapper architecture. Currently integrating pd_deflation.R into the active pipeline and auditing utility functions (copy_dlw_meta, dontrun examples) to ensure all code serves the pipeline wrappers or is properly documented as a standalone admin tool.

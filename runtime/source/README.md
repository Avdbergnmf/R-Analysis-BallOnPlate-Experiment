# Runtime Source Overview

The runtime layer is now organised around **feature modules** (domain logic) and **utils** (shared plumbing). Either side may call into the other, but feature modules do not depend on other features—each delivers an isolated slice of behaviour on top of the shared utilities.

## Main Pipeline

1. `runtime/index.Rmd` orchestrates the build. It drives the calculation wrappers in `utils/calculation_wrappers.R`, which expose top‑level entry points such as `calc_all_gait_params()`, `get_all_complexity_metrics()`, and friends.
2. Each wrapper wires the requested loop function (`get_data_from_loop` sequential, `get_data_from_loop_parallel` parallel) into the relevant feature module (`features/gait`, `features/complexity`, `features/simulation`, etc.) and passes through the utility helpers it needs (data loading, trimming, caching, logging).
3. Calculations run on a **loop basis**: every dataset-backed trial/participant combination is processed individually inside the loop. The sole exception is questionnaires, which aggregate directly.
4. Feature modules rely on utils for cross-cutting tasks—examples include:
   - `utils/data_loading.R` for tracker/simulation access plus `.qs` caching.
   - `utils/data_processing` for loop execution, validation, and orchestration.
   - `utils/logging.R` for structured logging.

## Loop Execution

- Loop concerns live in `utils/data_processing/loop_execution.R` and `parallel_processing.R`.
- Wrappers hand the appropriate loop function to feature modules, which produce one-row data frames per combination; utilities then bind those rows to the final output.
- Missing combinations can be backfilled via `handle_missing_combinations()` after initial load.

## Parallelism & Caching

- `parallel = TRUE` switches the loop executor to the PSOCK worker pool defined in `parallel_processing.R`; `parallel = FALSE` stays sequential.
- `force_cache_refresh = TRUE` on `load_or_calculate()` (or any wrapper passing `...`) rebuilds the `.qs` snapshots for tracker/simulation data before the loop.
- `force_recalc`, `stop_cluster`, `allow_add_missing`, `combinations_df`, and `extra_global_vars` let you control cache bypassing, cluster lifecycle, selective recalc, subset processing, and worker exports respectively; all described in `utils/data_processing/load_or_calculate.R`.

## Key Files

- `features/` — isolated feature modules (gait, complexity, simulation, questionnaires, etc.).
- `utils/` — shared helpers (data loading, caching, logging, loop execution, plotting, stats).
- `calculation_wrappers.R` — declarative entry points called from R Markdown, mapping wrappers to feature modules.
- `parallel_processing.R` — parallel loop engine with consolidated logging and cache priming.

This structure keeps feature logic modular while letting shared infrastructure be reused across the pipeline. Callers only interact with the wrappers; everything else is composed under the hood.***

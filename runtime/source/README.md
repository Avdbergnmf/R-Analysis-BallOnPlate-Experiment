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

## Caching & Parallelism

**Two cache layers**
- `load_or_calculate()` writes the final results of each wrapper to `results/*.rds` (or `.qs`) so repeated dashboard loads skip re-computation.
- Tracker and simulation primitives use `utils/cache.R`/`utils/data_caching` to persist per-dataset `.qs` snapshots. Preload hooks call helpers such as `preload_gait_tracking_data()` to warm these caches before the main loop runs.

**Force refresh controls**
- Passing `force_cache_refresh = TRUE` to `load_or_calculate()` tells preload hooks to rebuild dataset snapshots; if callers omit it, the function now falls back to the global `CACHE_FORCE_REWRITE` flag.
- `FORCE_RECALC` (from config) forces `load_or_calculate()` to ignore the existing result file and recompute everything.
- `force_recalc = TRUE` on a single call skips the saved result just for that invocation.

**Parallel execution**
- By default `parallel = USE_PARALLEL`, which wires the loop through the PSOCK cluster in `utils/data_processing/parallel_processing.R`. Setting `parallel = FALSE` keeps everything sequential.
- `THRESHOLD_PARALLEL` guards the overhead of spinning up workers when only a handful of combinations are requested. `MAX_CORES` caps the worker count (with `-1` meaning “auto-detect all cores minus one”).
- When you need reusable workers across several calls, keep `stop_cluster = FALSE`; pass `stop_cluster = TRUE` on the last call to shut the shared cluster down cleanly.

**Other knobs**
- `allow_add_missing` backfills combinations missing from the cached result.
- `combinations_df` scopes work to a subset of participant/trial rows.
- `extra_global_vars` ensures specific globals are exported to workers beyond the default environment snapshot.

## Configuration Touchpoints

All toggles live in `runtime/config.yml` and are surfaced as globals in `runtime/global.R`. Key settings:

- `cache.force_rewrite` → populates `CACHE_FORCE_REWRITE`; drives the default `force_cache_refresh` behaviour for both dataset snapshots and result caching.
- `cache.compression_level` & `cache.format` → control `qs::qsave` compression and whether caches use `.qs` or `.rds`.
- `performance.use_parallel` → becomes `USE_PARALLEL`, the default for `load_or_calculate()` and the loop wrappers.
- `performance.parallel_threshold` → exported as `THRESHOLD_PARALLEL`, setting the break-even point for enabling workers.
- `performance.force_recalc` → mapped onto `FORCE_RECALC`, forcing fresh result calculations on every run when true.
- `performance.max_cores` → used when sizing the shared PSOCK cluster.
- `performance.enable_file_logging`, plus `logging.*` → determine whether module loggers write to disk and at which level, useful when diagnosing cache/parallel behaviour.

Adjust these values per environment (`default`, `development`, `production`) and reload the app to have the runtime pick up the new defaults automatically.

## Key Files

- `features/` — isolated feature modules (gait, complexity, simulation, questionnaires, etc.).
- `utils/` — shared helpers (data loading, caching, logging, loop execution, plotting, stats).
- `calculation_wrappers.R` — declarative entry points called from R Markdown, mapping wrappers to feature modules.
- `parallel_processing.R` — parallel loop engine with consolidated logging and cache priming.

This structure keeps feature logic modular while letting shared infrastructure be reused across the pipeline. Callers only interact with the wrappers; everything else is composed under the hood.***

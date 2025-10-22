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

This structure keeps feature logic modular while letting shared infrastructure be reused across the pipeline. Callers only interact with the wrappers; everything else is composed under the hood.

## Derived Metrics System

The system includes a sophisticated **derived metrics** feature that allows users to create reference variables from specific phases or trials. This is particularly useful for statistical analysis where you need to include baseline performance or specific trial data as fixed effects in linear mixed models.

### **How Derived Metrics Work**

1. **Definition**: Users define derived metrics through the UI interface, specifying:
   - **Variable**: The source variable to extract (e.g., `task_drop_risk_1s_mean`)
   - **Scope**: Whether to extract from `phase` or `trial`
   - **Level**: The specific phase/trial to use as reference (e.g., `baseline_task`, `trial_5`)
   - **Label**: Human-readable name for the metric
   - **Column Name**: The final variable name in the dataset

2. **Processing**: The system automatically:
   - Loads the source variable from the full dataset
   - Filters to the specified phase/trial level
   - Extracts the values for each participant
   - Merges the result back into the main dataset as a new column

3. **Caching**: Derived metrics are cached efficiently:
   - **Memory cache**: Stored in global variables for fast access
   - **File persistence**: Saved to `data_extra/derived_metrics.csv`
   - **Smart invalidation**: Only reloads when the CSV file changes
   - **Session persistence**: Cache survives within the same R session

### **Example Use Cases**

- **Baseline Performance**: Create `task_drop_risk_1s_mean_phase_baseline_task` to include baseline performance as a fixed effect in LMMs
- **Trial-Specific Reference**: Create `complexity_hipPos_raw_sd_trial_5` to use specific trial data as a covariate
- **Statistical Modeling**: Include participant-specific reference values as fixed effects in linear mixed models
- **Cross-Condition Analysis**: Use baseline or training performance to control for individual differences

### **Technical Implementation**

- **UI Integration**: Derived metrics are managed through the sidebar interface
- **Reactive Updates**: Changes automatically trigger data refresh and cache invalidation
- **Formula Compatibility**: Variable names are automatically quoted for R formula compatibility
- **Error Handling**: Robust error handling with detailed logging for debugging
- **Performance**: Efficient caching prevents repeated CSV loading and processing

### **File Structure**

```
runtime/
├── data_extra/
│   └── derived_metrics.csv          # Persistent storage of metric definitions
├── source/utils/filter_manager.R     # Core processing logic
└── pages/sidebar_dynamicDataFiltering.Rmd  # UI interface
```

This system provides a flexible way to extend the dataset with custom variables while maintaining performance and data integrity.***

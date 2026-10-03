# Execution guide

This document describes how to reproduce the computational workflow for this project from a clean checkout of the repository. It covers the expected working directory, R environment, project initialization, execution order, inputs and generated outputs, logging, stochastic analyses, and dependency maintenance.

The scientific rationale for individual analyses belongs in the relevant manuscript or analysis documentation. This document is concerned with **how the code is executed reproducibly**.

## Canonical entry point

The canonical way to execute the complete analysis workflow is from the repository root:

```bash
Rscript run_project.R
```

`run_project.R` is responsible for restoring the R package environment, loading shared project code, initializing outputs, running the main analysis scripts in order, recording timing and environment information, and returning a success or failure exit status.

All project-wide execution should use this entry point unless a script is explicitly documented as standalone.

## Working-directory requirement

The workflow assumes that the current working directory is the **repository root**. Many project paths are relative, including:

```text
project_support.R
R_functions/
R_scripts/
data/
figures/
logs/
renv.lock
```

Running `run_project.R` from another directory will cause path resolution to fail or may create outputs in the wrong location.

A typical repository layout is therefore:

```text
.
├── run_project.R
├── project_support.R
├── renv.lock
├── R_functions/
├── R_scripts/
├── data/
├── docs/
├── figures/          # generated; recreated by run_project.R
└── logs/             # generated
```

## R package environment

The project uses [`renv`](https://rstudio.github.io/renv/) to record and restore its R package environment.

`run_project.R` checks for `renv` and installs it from CRAN if necessary. It then requires `renv.lock` to be present in the project root and runs:

```r
renv::restore(
  project = project_root,
  prompt = FALSE,
  retry = FALSE
)
```

The lockfile is therefore part of the executable specification of the project and should be committed to version control.

For a manual environment check, use:

```r
renv::status()
```

For dependency maintenance after adding or removing package usage, use:

```r
renv::dependencies()
renv::snapshot()
renv::clean()
renv::status()
```

Do not manually remove packages from `renv.lock` merely because they are no longer called directly. Packages required transitively by retained dependencies must remain in the lockfile.

## External software requirements

`renv` reproduces the project's R package environment, but it does **not** manage all system-level software needed by those packages or by the analysis itself. A fresh machine therefore requires several external components in addition to `renv::restore()`.

### CmdStan and the C++ toolchain

The speed-evolution analysis uses `cmdstanr`. The R package `cmdstanr` is managed by `renv`, but the external **CmdStan installation is not**. Running the Stan analyses therefore requires:

- a working CmdStan installation;
- a supported C++ compiler;
- GNU Make or the corresponding platform build tools.

The installation can be checked from R with:

```r
cmdstanr::check_cmdstan_toolchain()
cmdstanr::cmdstan_version()
```

If the toolchain is available but CmdStan itself is not installed, it can be installed with:

```r
cmdstanr::install_cmdstan()
```

On Linux, the required build tools normally include `g++` and `make`. On macOS, the Xcode command-line tools provide the compiler toolchain. On Windows, the corresponding RTools installation provides the required compiler and build utilities.

Because CmdStan is outside the R package library, its version should be treated as part of the computational environment. `run_project.R` records both the installed `cmdstanr` package version and the external CmdStan version in the run log when available.

### ImageMagick

The project's R dependency graph includes the `magick` package. On Linux, `magick` depends on the system-level ImageMagick Magick++ library, which is not installed or managed by `renv`.

On Debian or Ubuntu, install the required development library with:

```bash
sudo apt-get update
sudo apt-get install -y libmagick++-dev
```

Without this library, `renv::restore()` may successfully install the R package files but fail when testing or loading `magick`, with an error indicating that an ImageMagick shared library such as `libMagick++-6.Q16.so` cannot be found.

On macOS and Windows, CRAN binary installations generally provide a simpler installation path, but ImageMagick remains an external system dependency when `magick` is built from source.

The GitHub Actions smoke-test workflow installs `libmagick++-dev` explicitly before restoring the `renv` environment. It does **not** install CmdStan because the smoke-test CI job does not compile or sample Stan models; a full end-to-end project run still requires CmdStan separately.

### Graphics capabilities

The project also requires R graphics support for PNG and Cairo output. `project_support.R` checks these capabilities at startup:

```r
stopifnot(capabilities("png"))
stopifnot(capabilities("cairo"))
```

### Git

Git is used by `run_project.R` to record repository metadata when available, but the analysis itself does not depend on Git operations.

## Project initialization

After restoring the package environment, `run_project.R` sources:

```r
source("project_support.R")
```

`project_support.R` performs shared initialization for the workflow. In particular, it:

- loads the packages used by the project;
- verifies PNG and Cairo graphics support;
- records the machine name;
- sets the project random seed to `2025`;
- enables warnings for partial `$` matching;
- sources all files in `R_functions/`;
- defines shared MCMC settings such as the number of chains, iteration count, and `adapt_delta`.

Analysis scripts executed by `run_project.R` therefore assume that this shared environment has already been initialized.

## Output initialization and destructive behavior

At the beginning of each complete run, `run_project.R` calls:

```r
dir_init("figures")
```

`dir_init()` removes the existing directory recursively and recreates it empty. Consequently, **all existing contents of `figures/` are deleted at the start of a canonical run**.

Files that must be preserved should not be stored manually in `figures/`.

The runner does not wipe the `data/` or `logs/` directories.

## Required data inputs

The current analysis scripts expect the following core CSV files under `data/`:

```text
data/games.csv
data/eras.csv
data/move12s.csv
data/players.csv
```

Not every script uses every file, but a complete project run requires the collection as a whole.

These files are treated as inputs to the main workflow. Their provenance and construction should be documented separately if they are generated by an upstream data-processing pipeline.

## Main execution order

After initialization, `run_project.R` executes the following scripts sequentially:

```text
1. R_scripts/plot_openings.R
2. R_scripts/plot_opening_trees.R
3. R_scripts/plot_database_coverage.R
4. R_scripts/calc_game_distances.R
5. R_scripts/calc_match_networks.R
6. R_scripts/analyze_opening_diversity.R
7. R_scripts/analyze_speed_evolution.R
```

Each script is sourced into the shared project environment. Execution is therefore sequential rather than isolated into separate R sessions.

If a script fails, subsequent scripts are not executed. The runner records the error, closes the log, prints a failed-run summary, and exits with a nonzero status.

A successful run exits with status `0`.

## Country-specific diversity analyses

The repository also contains:

```text
R_scripts/analyze_opening_diversity_CN.R
R_scripts/analyze_opening_diversity_JP.R
R_scripts/analyze_opening_diversity_KR.R
```

These scripts are **not currently part of the canonical `run_project.R` workflow**.

They assume that the shared project environment has already been initialized. If they are intended to be reproducible project outputs, they should either be added explicitly to `run_project.R` or given their own documented initialization procedure. Their omission from the runner should not be interpreted as automatic execution.

## Generated data and cached calculations

Most graphical and summary outputs are written to `figures/`, but the workflow also creates derived files under `data/`.

### Game-distance cache

`calc_game_distances.R` creates:

```text
data/distance_mds.RDS
```

only if that file does not already exist. If it exists, the distance calculation is skipped and the existing file is retained.

This means that `distance_mds.RDS` acts as a cache. To force the multidimensional-scaling calculation to be regenerated, delete the file before running the project.

The calculation uses the project random seed when it is regenerated.

### Match-network edge lists

`calc_match_networks.R` writes period-specific files of the form:

```text
data/edgelist_<period>.csv
```

These files are regenerated by the script and subsequently read back into the network analyses.

Because `data/` contains both primary inputs and generated derivatives, generated files should be clearly distinguished from source data in version-control and data-provenance documentation.

## Figure and summary outputs

Analysis scripts write PNG figures and supporting summary files to `figures/`. In addition to figures, some analyses produce files such as:

```text
figures/calcs_game_distances.yaml
figures/calcsGameDistances.tex
figures/calcs_opening_diversity.yaml
figures/calcsOpeningDiversity.tex
```

Because `figures/` is recreated at the start of every canonical run, these files should be treated as reproducible outputs rather than permanent source material.

`calc_match_networks.R` also creates:

```text
figures/match_network/
```

for period-specific network figures.

## Randomness and reproducibility

The shared project seed is defined in `project_support.R`:

```r
project_seed <- 2025
set.seed(project_seed)
```

This controls ordinary R random-number generation after initialization. Individual scripts may reset the same seed for specific calculations; for example, the game-distance calculation resets `project_seed` before subsampling games when its cache is regenerated.

The Stan analyses use CmdStan's own sampling machinery. The current calls do not explicitly supply a Stan `seed` argument. Consequently, a repeated run should reproduce the same model specification and inferential procedure, but posterior draws are not guaranteed to be bit-for-bit identical across runs.

If exact repeatability of the Stan draws is required, an explicit seed should be passed to each `model$sample()` call.

Shared model-control values currently include:

```r
n_chains <- 4
n_iter <- 1000
adapt_delta <- 0.95
```

The speed-evolution script uses these settings for its principal sampling calls, together with additional sampler arguments defined within the script.

## Logging and run metadata

Each invocation of `run_project.R` creates a timestamped log under:

```text
logs/run-YYYY-MM-DD_HH-MM-SS.log
```

The runner also updates:

```text
logs/latest.log
```

to contain a copy of the most recent run log.

The log records, where available:

- project root;
- start and finish times;
- machine and operating-system information;
- R architecture and version;
- Git branch, commit, remote, and dirty-tree status;
- locked and installed package versions;
- `renv` status;
- per-script execution timing;
- the `tictoc` timing log;
- `sessionInfo()`;
- final `SUCCESS` or `FAILED` status;
- any error message and available call-stack information.

A clean reproducibility record should retain the relevant log together with the Git commit corresponding to the run.

## Running individual analysis scripts

The main analysis scripts are designed to be sourced after `project_support.R` has been loaded. They should not be assumed to work correctly as bare commands such as:

```bash
Rscript R_scripts/plot_openings.R
```

unless the script has been explicitly made standalone.

For normal reproducible execution, use:

```bash
Rscript run_project.R
```

For interactive development, an R session can initialize the shared environment with:

```r
source("project_support.R")
```

before sourcing an individual analysis script.

The legacy `0_init_project.R` script should only be used if it remains an intentional interactive entry point; it is not used by `run_project.R`.

## Adding a new analysis script

When extending the project:

1. Place reusable functions in `R_functions/` and analysis code in `R_scripts/`.
2. Avoid hidden assumptions about the interactive workspace.
3. Use project-root-relative paths consistently.
4. Add the new script explicitly to `run_project.R` in the required execution position.
5. Ensure that any upstream files needed by the script are generated before it runs.
6. Write reproducible outputs to an appropriate generated-output location.
7. Use package namespaces explicitly where practical, especially for packages that do not need to be attached globally.
8. If a new package is introduced, update the dependency record with `renv::snapshot()`.
9. Run `renv::status()` afterward and confirm that the environment is synchronized.
10. Execute the complete project from a clean session before treating the new workflow as reproducible.

If a script is intentionally excluded from the canonical runner, document why and state how it should be executed.

## Dependency maintenance

When package usage changes, first inspect what `renv` detects:

```r
renv::dependencies()
```

Then update the lockfile:

```r
renv::snapshot()
```

Remove installed project-library packages that are no longer needed:

```r
renv::clean()
```

Finally verify synchronization:

```r
renv::status()
```

A package can disappear as a **direct dependency** while remaining in `renv.lock` because another retained package requires it transitively. That is expected behavior.

## Clean-run checklist

A reproducible full run should satisfy the following conditions:

- execution begins from the repository root;
- `renv.lock` is present;
- the required files under `data/` are present;
- the required R version and package environment can be restored;
- required system libraries for compiled R packages are available, including ImageMagick/Magick++ where needed;
- CmdStan and the C++ toolchain are available for the Stan analysis;
- PNG and Cairo graphics capabilities are available;
- `Rscript run_project.R` completes with exit status `0`;
- the expected files are regenerated under `figures/` and derived data files are created or reused as documented;
- the run log reports `SUCCESS`;
- the Git commit and dirty-tree status recorded in the log identify the exact source state used for the run.

For the strongest reproducibility check, perform the workflow in a fresh checkout or otherwise clean environment rather than relying on objects, packages, caches, or generated files left over from an earlier interactive session.

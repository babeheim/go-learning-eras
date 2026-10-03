# go-learning-eras

[![Project integrity tests](https://github.com/OWNER/go-learning-eras/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/OWNER/go-learning-eras/actions/workflows/ci.yml)
![renv](https://img.shields.io/badge/environment-renv-blue)

Long-term cultural evolution of opening strategies in professional Go.

```text
            ┌─────────────────────────────────────────────┐
            │ ┌─────────────────────────────────────────┐ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . ● . . . . .  │ │
            │ │  . . . ● . . . . . ● . . . . . ○ . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . ○ . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . + . . . . . + . . . . . ○ . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . ● . . . . . + . . . . . ○ . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ │  . . . . . . . . . . . . . . . . . . .  │ │
            │ └─────────────────────────────────────────┘ │
            └─────────────────────────────────────────────┘
```

## Overview

This repository contains the data and analysis workflow for:

> Beheim, B. (2025). [Opening strategies in the Game of Go from feudalism to superhuman AI](https://doi.org/10.1017/ehs.2025.10016). *Evolutionary Human Sciences*, 7, e28.

The project analyzes long-term change in professional Go opening strategies. The workflow quantifies opening diversity and divergence across historical eras, examines relationships among players and opening strategies, and models changes in the pace of opening evolution through time.

## Reproducing the analysis

From the repository root, run:

```bash
Rscript run_project.R
```

The project uses [`renv`](https://rstudio.github.io/renv/) to restore the required R package environment. A working CmdStan installation is also required for the Bayesian analyses.

The canonical workflow generates figures and numerical summaries under `figures/`, derived intermediate files under `data/`, and timestamped execution logs under `logs/`.

Runtime depends on hardware, CmdStan compilation state, and whether cached intermediate results are already present.

For full execution details, see [`docs/execution-guide.md`](docs/execution-guide.md).

## Documentation

- [`docs/execution-guide.md`](docs/execution-guide.md) — software requirements, environment restoration, execution order, outputs, logging, and reproducibility.
- [`docs/algorithmic-workflow.md`](docs/algorithmic-workflow.md) — detailed description of the algorithms, statistical calculations, network analyses, and models implemented by the analysis scripts.

## Data

The `data/` directory contains the processed analytical data used by the project, derived from the GoGod 2024 database.

Key files include:

- `games.csv` — one row per game, including player IDs, date, and the first 50 moves. Games are uniquely identified by `hash_id`.
- `players.csv` — one row per player, including full name and biographical information. Players are uniquely identified by `player_id`.
- `eras.csv` — definitions of the six historical eras used in the study.
- `move12s.csv` — reference data used in the opening-sequence analyses.
- `move13s.csv` — companion reference data used in the opening-sequence analyses.

The workflow may also create derived or cached files under `data/`; these are documented in [`docs/execution-guide.md`](docs/execution-guide.md).

## Repository structure

```text
data/              Processed analytical data and derived intermediate files
R_functions/       Shared project-specific R functions
R_scripts/         Analysis and figure-generation scripts
docs/              Technical documentation
figures/           Generated analysis outputs
logs/              Generated execution logs
project_support.R  Shared project initialization and workflow parameters
run_project.R      Canonical workflow entry point
renv/              Project-local renv infrastructure
renv.lock          Locked R package environment
LICENSE.md         Project license
README.md          This file
```

`project_support.R` loads shared project code and defines workflow parameters such as the project random seed. `run_project.R` restores the package environment, initializes the project, executes the main analysis scripts in sequence, and records run metadata.

## Citation

If you use this repository, please cite:

Beheim, B. (2025). Opening strategies in the Game of Go from feudalism to superhuman AI. *Evolutionary Human Sciences*, 7, e28. https://doi.org/10.1017/ehs.2025.10016

## License

All materials in this repository are provided under the Creative Commons BY-NC-SA 4.0 license. See [`LICENSE.md`](LICENSE.md) for details.

If any included or derived source data are subject to separate upstream licensing terms, those terms take precedence for the affected data.

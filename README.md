# go-learning-eras

```
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

Analysis code and data for Beheim, B. (2025) [Opening strategies in the Game of Go from feudalism to superhuman AI](https://doi.org/10.1017/ehs.2025.10016) *Evolutionary Human Sciences* 7(e28).

All materials are under the Creative Commons BY-NC-SA 4.0 license. See `LICENSE.md` for details.

## files

- `R_functions/` - folder with project-specific R functions for analysis
- `R_scripts/` - individual analysis scripts that perform calculations and generate figures
- `assets/` - a cool figure I made that's (not in the publication)
- `data/` - a processed version of the GoGod 2024 database
   - `games.csv` - one row per game, containing player ID for each player, date, and first 50 moves. Games are uniquely identified by `hash_id`.
   - `players.csv` - one row per player, including full name, biographical details, etc. Players are uniquely identified by `player_id`.
   - `eras.csv` - list of the six eras used in this study
   - `move12s.csv` - list of the relevant opening two moves
   - `move13s.csv` - list of the relevant opening two moves
- `project_support.R` - script that loads all packages and functions needed for this workflow and sets a few workflow parameters, e.g. `project_seed`
- `0_init_project.R` - script to wipe folder of created assets in the `figures/` folder
- `1_analyze_diversity.R` - executes analysis scripts in `R_scripts/` and puts output into `figures/`
- `LICENSE.md` - text of the Creative Commons BY-NC-SA 4.0 license
- `README.md` - this file!

## instructions

To reproduce the content of the script, run `0_init_project.R` in an R environment with the working directory and then `1_analyze_diversity.R`. Running step 0 again will delete all derived assets and reset the repository. The project should take ~20 minutes and produce all analysis assets (images, calculations) in the `figures/` folder.

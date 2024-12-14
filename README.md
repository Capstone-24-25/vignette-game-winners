# vignette-game-winners
Vignette on using Statistical Modeling to predict the winning team of an NFL game; created as a class project for PSTAT197A in Fall 2024

# Contributors

Anshi Arora, Joshua Charfauros, Christina Cui, Sean Reagan

# Abstract

The objective of this vignette is to use a multitude of variables to predict binary win/loss outcomes of a game.

To determine which variables have strong correlations with game win, and thereby likely will serve as strong predictors, we will be conducting some exploratory data analysis. Then, we will train a random forest model on the data. After making the model, we can evaluate its accuracy on the test set and account for any issues that arise. We will also calculate variable importance scores to determine which predictors serve the largest roles in determining the prediction. This model is further developed by adding training controls.

# Repository Contents
**The following files are in the root directory:**

- `README (.md/.html)` this overview document
- `Vignette (.qmd/.html)` final compiled report
- `Vignette_cache` supplementary folder aids in rendering of vignette.qmd
- `Vignette_files` supplementary folder aids in rendering of vignette.qmd
- `Scripts`
  - `Drafts` subdirectory with drafts from each contributer
  - `Vignette.R` final compiled script
- `RDS files` includes RDS files created in Exploratory Data Analysis, aid in rendering of graphs in vignette.qmd
- `Figures` graphs and figures stored as png's
- `Data` raw preprocessed csv


# Reference List
https://www.nflfastr.com/index.html
https://nflreadr.nflverse.com/
https://www.nflfastr.com/articles/beginners_guide.html#real-life-example-lets-make-a-win-total-model
www.nflfastr.com/articles/stats_variables.html

# data-analysis-europe
Data Analysis of Influenzanet at european level

# Repository organization

This repository follows the workspace organization described in [ifnBase package](https://github.com/cturbelin/ifnBase/blob/master/vignettes/workspace.Rmd)

This repository is organized with a set of projects, each in subdirectory. A *project* is just a way to group scripts with a common purpose. They share a common init script ("conf.R") and can share project specific scripts in a subfolder lib/ and a output directory.

A project must be independent from others and the only scripts known outside it must be "system.R" in its upper level.

The `share` directory contains common libraries & data shared accross projects. These libraries must be loaded with `share.lib` function & data using `share.data.path()` function.

## Configuration

Configuration is expected to be in a `location.R` file in the root of this repository. It contains local configuration specific to this project. It can overrides system-wide or user profile config (.Rprofile file). We use this *location.R* because several repositories with differents projects can be hosted in the same user space.

## Installation

Dependencies:
 - ifnBase
 - swMisc
 - dplyr
 - reshape2
 - ggplot2

Launch R (this repository root dir as working directory) and run
```R
source("share/install.R")

```

## Run scripts

To run a script, the working directory **MUST** be it's project directory (not the repository root directory but the subfolder).

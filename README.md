# Anthro Survey Analyser 

The Anthro Survey Analyser is an online tool developed by the Department of Nutrition for Health and Development at the World Health Organization (WHO), that allows the user to perform comprehensive analysis of anthropometric survey data.

## App structure

* app.R The general entry point of the application.
* code In this folder, all functions can be found that are not wrapped as shiny modules.
* modules A lot of components have been extracted into shiny modules. They are in this directory.
* reports Contains the docx reporting templates.
* static Static content for the shiny app. E.g. the license text or the WHO logo.
* tests Some tests for newly developed functions.
* devel A directory for development scripts that are not part of the shiny app.

## General application design

The app allows the user to upload a csv dataset and then map dataset columns to certain concepts (e.g. Age, Sex, Cluster, etc.) that are needed for the various analyses. Each of these mappings has a validation rule that determines what columns can be used.

Each analysis module (e.g. Distribution, Z-Scores, Prevalence) is a separate shiny module and works independently of the other modules. Each module checks for valid input and other preconditions to perform the respective analysis. However all modules operate on a central, in memory, dataset.

The prevalence calculation is the most computation heavy part.

In addition static content has been added that can be found in the static folder.

## Deployment

The application is optimized to run on [shinylive](https://posit-dev.github.io/r-shinylive/), where the code is interpreted using a webassembly version of the R interpreter.

## Running the app locally in R

* Install the dependencies using `renv::restore()`
* Run `shiny::runApp()`

## Renv

The app uses [renv](https://rstudio.github.io/renv/articles/renv.html) to manage dependencies. The top-level `renv.lock` file contains the dependencies for the production release.

In order to use the pinned version of R consider using [rig](https://github.com/r-lib/rig).

At the moment the shiny app uses R 4.5.1 and has package versions pinned to specific versions matching their WebR counter parts.

## Formatting

The project uses [air](https://github.com/posit-dev/air).

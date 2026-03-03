# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **bookdown** project for the course "PS9591: Regressions and Causal Inference" at Western University. It compiles R Markdown files into an HTML book hosted at https://svallejovera.github.io/regression_ci/.

## Build Commands

**Build the book** (in R console or RStudio):
```r
bookdown::render_book("index.Rmd")
```

Or use RStudio's Build menu → "Build Book".

**Output location**: `/docs/` directory (configured for GitHub Pages)

**Supported output formats**: HTML (bs4_book), PDF (pdflatex), EPUB

## Project Structure

- `index.Rmd` - Main book configuration and course overview
- `XX-lectureN.Rmd` - Lecture content (numbered for ordering)
- `XX-tutorialN.Rmd` - Interactive learnr tutorials
- `XX-exercisesN.Rmd` - Exercise sets
- `/slide/` - PowerPoint and PDF lecture slides
- `/docs/` - Generated HTML output (do not edit directly)
- `/Sample_data/` - Datasets for final exam replication exercises
- `/tutorialN/` - Individual tutorial directories with .Rmd files
- `_bookdown.yml` - Bookdown configuration
- `_output.yml` - Output format settings

## Key R Packages

Essential packages used throughout:
- `bookdown`, `knitr`, `rmarkdown` - Book compilation
- `learnr` - Interactive tutorial framework
- `tidyverse`, `tidylog` - Data wrangling
- `wooldridge` - Econometrics datasets (heavily used)
- `modelsummary`, `sjPlot`, `stargazer` - Model output tables
- `marginaleffects` - Marginal effects calculations
- `causaldata` - Causal inference datasets

## Running Interactive Tutorials

Tutorials use the `learnr` package with Shiny runtime:
1. Open tutorial `.Rmd` file in RStudio
2. Click "Run Document" button
3. Tutorial opens in browser with interactive exercises

## Content Conventions

- Chapter numbering is controlled by filename prefixes (e.g., `01-`, `02-`)
- Cross-references use `{#label}` syntax
- Bibliography entries in `book.bib` use `@key` notation
- LaTeX customization in `preamble.tex`

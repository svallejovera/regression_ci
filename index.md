# Hello bookdown 

All chapters start with a first-level heading followed by your chapter title, like the line above. There should be only one first-level heading (`#`) per .Rmd file.

## A section

All chapter sections start with a second-level (`##`) or higher heading followed by your section title, like the sections above and below here. You can have as many as you want within a chapter.

### An unnumbered section {-}

Chapters and sections are numbered by default. To un-number a heading, add a `{.unnumbered}` or the shorter `{-}` at the end of the heading, like in this section.

<!--chapter:end:01-intro.Rmd-->


# Cross-references {#cross}

Placeholder


## Chapters and sub-chapters
## Captioned figures and tables

<!--chapter:end:02-cross-refs.Rmd-->

# Parts

You can add parts to organize one or more book chapters together. Parts can be inserted at the top of an .Rmd file, before the first-level chapter heading in that same file. 

Add a numbered part: `# (PART) Act one {-}` (followed by `# A chapter`)

Add an unnumbered part: `# (PART\*) Act one {-}` (followed by `# A chapter`)

Add an appendix as a special kind of un-numbered part: `# (APPENDIX) Other stuff {-}` (followed by `# A chapter`). Chapters in an appendix are prepended with letters instead of numbers.




<!--chapter:end:03-parts.Rmd-->

# Footnotes and citations 

## Footnotes

Footnotes are put inside the square brackets after a caret `^[]`. Like this one ^[This is a footnote.]. 

## Citations

Reference items in your bibliography file(s) using `@key`.

For example, we are using the **bookdown** package [@R-bookdown] (check out the last code chunk in index.Rmd to see how this citation key was added) in this sample book, which was built on top of R Markdown and **knitr** [@xie2015] (this citation was added manually in an external file book.bib). 
Note that the `.bib` files need to be listed in the index.Rmd with the YAML `bibliography` key.


The RStudio Visual Markdown Editor can also make it easier to insert citations: <https://rstudio.github.io/visual-markdown-editing/#/citations>

<!--chapter:end:04-citations.Rmd-->


# Blocks

Placeholder


## Equations
## Theorems and proofs
## Callout blocks

<!--chapter:end:05-blocks.Rmd-->


# Sharing your book

Placeholder


## Publishing
## 404 pages
## Metadata for sharing

<!--chapter:end:06-share.Rmd-->


# References {-}


<!--chapter:end:07-references.Rmd-->

--- 
title: "PS9591: Regressions and Causal Inference"
author: "Dr. Sebastián Vallejo Vera | Western University"
date: "2025-02-19"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib]
# url: your book url like https://bookdown.org/yihui/bookdown
# cover-image: path to the social sharing image like images/cover.jpg
description: "Code, exercises, and slides for PS9591: Regressions and Causal Inference"
link-citations: yes
github-repo: svallejovera/regression_ci
---
  
# "Regressions and Causal Inference" {.unnumbered}
  
&nbsp;

Welcome to the site for the course PS9591: "Regressions and Causal Inference" at Western University, taught by Sebastián Vallejo Vera. In each week, you will find the code, exercises, and slides for the corresponding topic. 

Before you start, check the required software and packages below. Also, don't forget to read the [Syllabus](https://svallejovera.github.io/files/9594A__Computational_Text_Analysis.pdf) and check Perusall for the readings for the course. This site will be corrected/updated throughout the semester(s).

## Software and Packages

For this course, we will be using R. I will assume that you are minimally familiar with R language, RStudio, and R packages. However, the course includes labs on coding in R. These are the main packages you will need to have installed:

- tidyverse (we will be piping)
- tidylog (helps keep track of what your are pipin')

## Datasets

Throughout the class, we will be using a number of sample datasets. Access to these datasets will be provided directly on the code. For the class For your Final Exam, I will post the required datasets for the replication exercise here:
  
  - COMING SOON... 

## Assignments


| Weeks          | Assignment?           |
|---------------|-------------|
| 1: A Primer on Using Text as Data      | Optional |
| 2: Tokenization and Word Frequency      | No      |
| 3: Dictionary-Based Approaches | Homework 1      |
| 4: Complexity and Similarity | Optional     |
| 5: Scaling Techniques and Topic Modeling | Homework 2 |
| 6: Word Embeddings | No   |
| 7: Getting Data | Optional   |
| 8: Introduction to Supervised Machine-Learning | Homework 3 |
| 9: Transformers | Optional |
| 10: Do We Even Need Theory Anymore? | No |
| 11: Llama 2, ChatGPT, and Generative Modeling | Optional |


## Final Exam

The final exam will require students to replicate the findings from a some papers, and interpret the results. 

## Acknowledgments 

The organization of this course is based on the great textbook 'The Effect: An Introduction to Research Design and Causality' by Nick Huntington-Klein, freely available at [here](https://theeffectbook.net/). The code used throughout for the main lectures is a patchwork of my own code, but my own code borrows heavily from the internet (but that's true for all code). I try my best to give credit to the original authors of the code (when and if possible). The code for the labs was created and revised by two amazing doctoral students[^1] at Western University, Hugo Machado and John Santos (and are posted here with their permission). 


[^1]: I will update their status as they advance on their academic journey.

<!--chapter:end:index.Rmd-->

# Hello bookdown 

All chapters start with a first-level heading followed by your chapter title, like the line above. There should be only one first-level heading (`#`) per .Rmd file.

## A section

All chapter sections start with a second-level (`##`) or higher heading followed by your section title, like the sections above and below here. You can have as many as you want within a chapter.

### An unnumbered section {-}

Chapters and sections are numbered by default. To un-number a heading, add a `{.unnumbered}` or the shorter `{-}` at the end of the heading, like in this section.

<!--chapter:end:01-intro.Rmd-->

# Cross-references {#cross}

Cross-references make it easier for your readers to find and link to elements in your book.

## Chapters and sub-chapters

There are two steps to cross-reference any heading:

1. Label the heading: `# Hello world {#nice-label}`. 
    - Leave the label off if you like the automated heading generated based on your heading title: for example, `# Hello world` = `# Hello world {#hello-world}`.
    - To label an un-numbered heading, use: `# Hello world {-#nice-label}` or `{# Hello world .unnumbered}`.

1. Next, reference the labeled heading anywhere in the text using `\@ref(nice-label)`; for example, please see Chapter \@ref(cross). 
    - If you prefer text as the link instead of a numbered reference use: [any text you want can go here](#cross).

## Captioned figures and tables

Figures and tables *with captions* can also be cross-referenced from elsewhere in your book using `\@ref(fig:chunk-label)` and `\@ref(tab:chunk-label)`, respectively.

See Figure \@ref(fig:nice-fig).


``` r
par(mar = c(4, 4, .1, .1))
plot(pressure, type = 'b', pch = 19)
```

<div class="figure" style="text-align: center">
<img src="main_files/figure-html/nice-fig-1.png" alt="Plot with connected points showing that vapor pressure of mercury increases exponentially as temperature increases." width="80%" />
<p class="caption">(\#fig:nice-fig)Here is a nice figure!</p>
</div>

Don't miss Table \@ref(tab:nice-tab).


``` r
knitr::kable(
  head(pressure, 10), caption = 'Here is a nice table!',
  booktabs = TRUE
)
```



Table: (\#tab:nice-tab)Here is a nice table!

| temperature| pressure|
|-----------:|--------:|
|           0|   0.0002|
|          20|   0.0012|
|          40|   0.0060|
|          60|   0.0300|
|          80|   0.0900|
|         100|   0.2700|
|         120|   0.7500|
|         140|   1.8500|
|         160|   4.2000|
|         180|   8.8000|

<!--chapter:end:02-cross-refs.Rmd-->

# Parts

You can add parts to organize one or more book chapters together. Parts can be inserted at the top of an .Rmd file, before the first-level chapter heading in that same file. 

Add a numbered part: `# (PART) Act one {-}` (followed by `# A chapter`)

Add an unnumbered part: `# (PART\*) Act one {-}` (followed by `# A chapter`)

Add an appendix as a special kind of un-numbered part: `# (APPENDIX) Other stuff {-}` (followed by `# A chapter`). Chapters in an appendix are prepended with letters instead of numbers.




<!--chapter:end:03-parts.Rmd-->

# Footnotes and citations 

## Footnotes

Footnotes are put inside the square brackets after a caret `^[]`. Like this one ^[This is a footnote.]. 

## Citations

Reference items in your bibliography file(s) using `@key`.

For example, we are using the **bookdown** package [@R-bookdown] (check out the last code chunk in index.Rmd to see how this citation key was added) in this sample book, which was built on top of R Markdown and **knitr** [@xie2015] (this citation was added manually in an external file book.bib). 
Note that the `.bib` files need to be listed in the index.Rmd with the YAML `bibliography` key.


The RStudio Visual Markdown Editor can also make it easier to insert citations: <https://rstudio.github.io/visual-markdown-editing/#/citations>

<!--chapter:end:04-citations.Rmd-->

# Blocks

## Equations

Here is an equation.

\begin{equation} 
  f\left(k\right) = \binom{n}{k} p^k\left(1-p\right)^{n-k}
  (\#eq:binom)
\end{equation} 

You may refer to using `\@ref(eq:binom)`, like see Equation \@ref(eq:binom).


## Theorems and proofs

Labeled theorems can be referenced in text using `\@ref(thm:tri)`, for example, check out this smart theorem \@ref(thm:tri).

::: {.theorem #tri}
For a right triangle, if $c$ denotes the *length* of the hypotenuse
and $a$ and $b$ denote the lengths of the **other** two sides, we have
$$a^2 + b^2 = c^2$$
:::

Read more here <https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html>.

## Callout blocks


The R Markdown Cookbook provides more help on how to use custom blocks to design your own callouts: https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html

<!--chapter:end:05-blocks.Rmd-->

# Sharing your book

## Publishing

HTML books can be published online, see: https://bookdown.org/yihui/bookdown/publishing.html

## 404 pages

By default, users will be directed to a 404 page if they try to access a webpage that cannot be found. If you'd like to customize your 404 page instead of using the default, you may add either a `_404.Rmd` or `_404.md` file to your project root and use code and/or Markdown syntax.

## Metadata for sharing

Bookdown HTML books will provide HTML metadata for social sharing on platforms like Twitter, Facebook, and LinkedIn, using information you provide in the `index.Rmd` YAML. To setup, set the `url` for your book and the path to your `cover-image` file. Your book's `title` and `description` are also used.



This `gitbook` uses the same social sharing data across all chapters in your book- all links shared will look the same.

Specify your book's source repository on GitHub using the `edit` key under the configuration options in the `_output.yml` file, which allows users to suggest an edit by linking to a chapter's source file. 

Read more about the features of this output format here:

https://pkgs.rstudio.com/bookdown/reference/gitbook.html

Or use:


``` r
?bookdown::gitbook
```



<!--chapter:end:06-share.Rmd-->


# References {-}


<!--chapter:end:07-references.Rmd-->


<!--chapter:end:main.Rmd-->


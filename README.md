  <!-- badges: start -->
  [![R-CMD-check](https://github.com/eriklarsen4/golf/actions/workflows/golf-r-package-checks.yml/badge.svg)](https://github.com/eriklarsen4/golf/actions/workflows/golf-r-package-checks.yml.yml)
  ![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
  ![PyPI](https://img.shields.io/badge/PyPI-not_yet_published-lightgrey?logo=pypi&logoColor=white)
  ![Python](https://img.shields.io/badge/Python-3.11-blue?logo=python&logoColor=white)
  <!-- badges: end -->


I developed this repository from real-world golf data-- data I curated from rounds I played. As a member of the **United States Golf Association** (**Arizona Golf Association Chapter**), these rounds were official scores used to compute my official **GHIN** (**Global Handicap Index Number**), meaning I played with players who verified I played out every single stroke (no gimmes), before submitting my scorecard to the USGA.

The data includes

Stroke-level stats/metrics:

  + stroke distances estimated by a Garmin GPS watch for all non-putts
  + lie type
  + miss direction
  + club selection
  
Hole-by-hole stats/metrics:

  + number of strokes
  + chips
  + putts
  + penalties
  + double bogey / bogey / par / birdie / eagle, etc.

Course-level data:

  + course
  + course rating
  + tee selection
  + slope
  + Handicap Index

The data is ideal for numerous types of modeling, including linear mixed models, time series, and decision trees
It will likely expand, depending on additional data

Please see the [articles](https://github.com/eriklarsen4/golf/tree/main/inst/articles) for details

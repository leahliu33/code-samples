
# Stata sample — Book digitization & library loans

`book_digitization_analysis.do`

In this do file, I replicated and extended a published study on whether
Google's mass digitization of library books affected physical borrowing,
using a balanced panel of \~88,000 books (792,054 book-year
observations).

**What it shows:** a full analysis pipeline in a single, clearly
sectioned do-file —

-   **Panel construction:** cleaning, merging, and building a balanced
    book-year panel with zero-imputation for unobserved years.
-   **Main analysis:** log-OLS and linear probability models via
    `reghdfe`, with formatted tables (`esttab`).
-   **Robustness:** the Callaway & Sant'Anna (2021) staggered
    difference-in-differences estimator (`csdid`), which corrects the
    negative-weighting bias in two-way fixed-effects models under
    staggered treatment timing.
-   **Heterogeneity:** category-level analysis (History, Fiction,
    Science, Education, Art) with a comparison bar chart.

The `filepath` global at the top is a placeholder — please set it to
your own directory to run. Source data is not included; the header
documents each input and output.

Requires: `estout`, `reghdfe`, `csdid`, `drdid`. Stata 18+.

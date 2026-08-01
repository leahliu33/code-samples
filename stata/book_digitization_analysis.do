/*******************************************************************************
  Title       : Code Sample — Effect of Book Digitization on Library Loans
  Author      : Ziqian Liu 
  Date        : April 2025
  Software    : Stata 18

  Overview
  --------
  This do file is a code task where I replicate and extend results from a published
  study on whether Google's mass digitization of library books affected physical 
  borrowing at Harvard libraries.

  The dataset covers roughly 88,000 unique books across nine years, yielding a
  balanced panel of 792,054 observations after construction.

  Structure of this file
  ----------------------
  PART 0  :  Global settings, file paths, and package checks
  PART 1  :  Data cleaning and balanced panel construction
               → output: master.dta
  PART 2  :  Main analysis — replication of Table 5 (log-OLS and LPM)
               → output: table_5.tex
  PART 3  :  Robustness check — Callaway & Sant'Anna (2021) DiD estimator
               → output: table_5_robust.tex
  PART 4  :  Heterogeneity analysis across book categories
               → output: category_bar_comparison.png, art_vs_edu_comparison.tex

  Summary of Outputs
  ------------------
  table_5.tex
      Replicates columns 1 and 2 of Table 5 from the main paper. 

  table_5_robust.tex
      Reports the Average Treatment Effect on the Treated (ATT) from the
      Callaway & Sant'Anna (2021) estimator, which corrects for the negative-
      weighting bias that can arise in two-way fixed effects models with
      staggered treatment timing.
	  
  category_bar_comparison.png
      Bar chart comparing average log loans before and after digitization across
      five book categories (History, Fiction, Science, Education, Art). 

  art_vs_edu_comparison.tex
      Separate log-OLS regressions for Art books and Education books.
      
  Required packages 
  ----------------------------------------
  estout   — for eststo / esttab output
  reghdfe  — high-dimensional fixed effects regression
  csdid    — Callaway & Sant'Anna DiD estimator
  drdid    — doubly robust DiD (dependency of csdid)

*******************************************************************************/


/*==============================================================================
  
  PART 0: GLOBAL SETTINGS
  
==============================================================================*/

	* Clear memory, suppress output paging, and close any open log
	clear all
	set more off
	capture log close

	* Increase memory limits for large datasets
	set maxvar 32000
	set matsize 10000

	* -------------------------------------------------------------------
	* Set root filepath — modify this line to match your system
	* -------------------------------------------------------------------
	global filepath "/path/to/project"

	* Derived path globals (no need to change these)
	global script   "$filepath/_script"
	global data     "$filepath/_data"
	global raw      "$data/_raw"
	global interm   "$data/_intermediate"
	global cleaned  "$data/_cleaned"
	global graphic  "$filepath/_graphic"
	global tabular  "$filepath/_tabular"
	global latex    "$filepath/_writeup_latex"

	* -------------------------------------------------------------------
	* Install required packages if not already present
	* -------------------------------------------------------------------
	foreach pkg in estout reghdfe csdid drdid {
		cap which `pkg'
		if _rc ssc install `pkg'
	}

	* Start log file (records all output for review)
	log using "$filepath/logfile", replace


/*==============================================================================

  PART 1: DATA CLEANING AND BALANCED PANEL CONSTRUCTION
  Input  : $raw/loans_merged.dta
  Output : $cleaned/master.dta
  
==============================================================================*/

	/*------------------------------------------------------------------------------
	  1a. Import and initial exploration
	------------------------------------------------------------------------------*/

	use "$raw/loans_merged.dta", clear

	browse
	summarize

	/*------------------------------------------------------------------------------
	  1b. Drop unnecessary variables
	------------------------------------------------------------------------------*/

	/* The study focuses on whether digital distribution affects physical demand.
	   I retain: year of loan, scanning-related variables, location, and book
	   identifiers. All other variables are dropped for clarity.               */

	drop date_loaned date_due date_returned loan_number month_loaned date_scanned author

	/*------------------------------------------------------------------------------
	  1c. Restrict sample to study window (2003–2011)
	------------------------------------------------------------------------------*/

	drop if year_loaned < 2003 | year_loaned > 2011

	/*------------------------------------------------------------------------------
	  1d. Data quality checks
	------------------------------------------------------------------------------*/

	* Confirm no missing values in key identifiers and treatment variable
	misstable summarize bib_doc_id year_loaned scanned    // expected: no missing

	/*------------------------------------------------------------------------------
	  1e. Variable labelling and reordering
	------------------------------------------------------------------------------*/

	order year_loaned, after(_oclc)

	label var scanned      "The book was scanned"
	label var year_loaned  "When the book was loaned"
	label var year_scanned "When the book was scanned"
	label var location     "Where the book is stored"

	* Save intermediate checkpoint
	save "$interm/interm_cleaned.dta", replace

	/*------------------------------------------------------------------------------
	  1f. Aggregate loans to book×year level
	------------------------------------------------------------------------------*/

	/* Each row in the raw data is a single loan transaction. I sum transactions
	   to obtain total loans per book per year, then keep one row per book-year. */

	gen loans = 1
	bysort bib_doc_id year_loaned: egen total_loans_year = sum(loans)
	drop loans

	bysort bib_doc_id year_loaned: keep if _n == 1

	save "$interm/interm_cleaned_each_year_one.dta", replace

	/*------------------------------------------------------------------------------
	  1g. Build balanced panel skeleton (every book × every year 2003–2011)
	------------------------------------------------------------------------------*/

	/* The raw data only records years in which a book was borrowed at least once.
	   To construct a balanced panel I need explicit zero-loan observations for
	   book-years with no activity. I do this with a cross-join of all book IDs
	   and all years, then merge back to the observed data.                      */

	* Extract unique book IDs
	preserve
		bysort bib_doc_id: keep if _n == 1
		keep bib_doc_id
		tempfile all_books
		save `all_books'
	restore

	* Create a dataset containing each year in the study window
	clear
	set obs 9
	gen year_loaned = 2002 + _n

	tempfile all_years
	save `all_years'

	* Full cross-join: every book paired with every year
	use `all_books', clear
	cross using `all_years'
	tempfile balanced_frame
	save `balanced_frame'

	/*------------------------------------------------------------------------------
	  1h. Merge balanced skeleton with observed loan data
	------------------------------------------------------------------------------*/

	/* After merging:
		 _merge == 3  →  book was observed in that year (keep original loan count)
		 _merge == 1  →  book existed but had zero loans that year (impute 0)    */

	merge 1:1 bib_doc_id year_loaned using "$interm/interm_cleaned_each_year_one.dta"

	replace total_loans_year = 0 if _merge == 1
	drop _merge

	/*------------------------------------------------------------------------------
	  1i. Forward-and-backward fill time-invariant book attributes
	------------------------------------------------------------------------------*/

	/* After the cross-join, book-level string variables (title, location, etc.)
	   are missing for imputed zero-loan rows. I propagate non-missing values
	   forward then backward within each book to fill those gaps.               */

	local varlist title_display _oclc location scanned year_scanned

	foreach var of local varlist {

		gen fill_`var' = `var'

		* Forward fill (ascending year order)
		bysort bib_doc_id (year_loaned): replace fill_`var' = fill_`var'[_n-1] ///
			if missing(fill_`var')

		* Backward fill (descending year order) to handle pre-scan missing values
		gen revorder = -year_loaned
		bysort bib_doc_id (revorder): replace fill_`var' = fill_`var'[_n-1] ///
			if missing(fill_`var')
		drop revorder

		replace `var' = fill_`var'
		drop fill_`var'
	}

	/*------------------------------------------------------------------------------
	  1j. Final sort, rename, and save master dataset
	------------------------------------------------------------------------------*/

	sort bib_doc_id year_loaned
	rename year_loaned year

	label var year             "Year"
	label var total_loans_year "Total number of loans in this year"

	* Verify panel balance
	count          // expected: 792,054 observations (88,006 books × 9 years)
	tab year       // each year should show 88,006 observations
	xtset bib_doc_id year   // confirms strongly balanced panel

	save "$cleaned/master.dta", replace


/*==============================================================================
  PART 2: MAIN ANALYSIS — REPLICATION OF TABLE 5
  Input  : $cleaned/master.dta
  Output : $tabular/table_5.tex, $cleaned/master_table_5.dta
==============================================================================*/

	/*------------------------------------------------------------------------------
	  Note:
	  
	  I replicate the two main specifications from Table 5 of Nagaraj & Reimers
	  (2021): a log-OLS model and a Linear Probability Model (LPM), both absorbing
	  book fixed effects and year×location fixed effects, with standard errors
	  clustered at the book level.

	  The independent variable is post_scanned: a dummy equal to 1 for all years
	  after a book was digitized by Google Books. Book FEs remove permanent
	  differences across books; year×location FEs control for demand shocks that
	  are specific to a library branch in a given year.
	------------------------------------------------------------------------------*/

	use "$cleaned/master.dta", clear

	/*------------------------------------------------------------------------------
	  2a. Construct treatment indicator: post-digitization dummy
	------------------------------------------------------------------------------*/

	/* post_scanned = 1 if the current year is AFTER the book's scan year;
	   books that were never scanned remain 0 throughout.                    */

	gen post_scanned = 0
	replace post_scanned = 1 if year > year_scanned & !missing(year_scanned)

	/*------------------------------------------------------------------------------
	  2b. Prepare outcome and control variables
	------------------------------------------------------------------------------*/

	* Log-transform loans (adding 1 handles zero-loan observations)
	gen log_loans = log(total_loans_year + 1)

	* Recode missing library locations as an explicit category so they are absorbed
	* by the year×location FE rather than dropped from the sample
	replace location = "missing_loc" if missing(location)
	encode location, generate(location_num)

	/*------------------------------------------------------------------------------
	  2c. Log-OLS regression
	  Outcome: log(loans + 1); identifies percentage-change effects
	------------------------------------------------------------------------------*/

	reghdfe log_loans post_scanned, ///
		absorb(bib_doc_id year#location_num) ///
		vce(cluster bib_doc_id)
	eststo log_ols

	/*------------------------------------------------------------------------------
	  2d. Linear Probability Model (LPM)
	  Outcome: indicator for any borrowing in a given year (extensive margin)
	------------------------------------------------------------------------------*/

	gen get_loaned = (total_loans_year > 0)

	reghdfe get_loaned post_scanned, ///
		absorb(bib_doc_id year#location_num) ///
		vce(cluster bib_doc_id)
	eststo lpm

	/*------------------------------------------------------------------------------
	  2e. Export results to LaTeX
	------------------------------------------------------------------------------*/

	esttab log_ols lpm using "$tabular/table_5.tex", replace ///
		b(4) se(5) star(* 0.10 ** 0.05 *** 0.01) ///
		keep(post_scanned) ///
		label ///
		booktabs ///
		title("Effects of Digitization on Harvard Library Loans") ///
		mlabels("log-OLS" "LPM") ///
		prefoot("\midrule Book FE & Yes & Yes \\ Year-Location FE & Yes & Yes \\")

	* Save enriched dataset (includes post_scanned, log_loans, get_loaned)
	drop _est_log_ols _est_lpm
	save "$cleaned/master_table_5.dta", replace


/*==============================================================================
  PART 3: ROBUSTNESS CHECK — CALLAWAY & SANT'ANNA (2021) ESTIMATOR
  Input  : $cleaned/master_table_5.dta
  Output : $tabular/table_5_robust.tex, $cleaned/master_robost.dta
==============================================================================*/

	/*------------------------------------------------------------------------------
	  Note:
	 
	  In settings with staggered treatment adoption (different books are digitized
	  in different years), the standard two-way fixed effects (TWFE) estimator can
	  be biased when treatment effects are heterogeneous across cohorts. In
	  particular, TWFE may assign negative implicit weights to some group-time ATTs,
	  potentially masking or exaggerating the true effect.

	  Callaway & Sant'Anna (2021) address this by separately estimating the ATT for
	  each group (defined by year of first digitization) and then aggregating them.
	  This approach uses only "clean" comparisons — never-treated and not-yet-treated
	  books as the control group — avoiding the problematic contaminated comparisons
	  in TWFE. I implement this via the csdid package.
	------------------------------------------------------------------------------*/

	use "$cleaned/master_table_5.dta", clear

	/*------------------------------------------------------------------------------
	  3a. Define treatment cohort variable
	  cohort = year a book was first scanned (0 for books never scanned)
	------------------------------------------------------------------------------*/

	gen cohort = 0
	replace cohort = year_scanned if !missing(year_scanned)

	/*------------------------------------------------------------------------------
	  3b. Callaway & Sant'Anna DiD — log-OLS outcome
	------------------------------------------------------------------------------*/

	quietly csdid log_loans, ///
		ivar(bib_doc_id) time(year) gvar(cohort) notyet method(reg)
		/* notyet: uses not-yet-treated observations as part of the control group */

	quietly estat simple   // aggregate all group-time ATTs into a single ATT

	* Capture ATT, standard error, and p-value from the returned matrix
	tempname temp1
	matrix `temp1' = r(table)
	local att_log = `temp1'[1,1]
	local se_log  = `temp1'[2,1]
	local p_log   = `temp1'[4,1]

	di "log_loans  — ATT: `att_log'  SE: `se_log'  p-value: `p_log'"

	/*------------------------------------------------------------------------------
	  3c. Callaway & Sant'Anna DiD — LPM outcome
	------------------------------------------------------------------------------*/

	quietly csdid get_loaned, ///
		ivar(bib_doc_id) time(year) gvar(cohort) notyet method(reg)

	quietly estat simple

	tempname temp2
	matrix `temp2' = r(table)
	local att_lpm = `temp2'[1,1]
	local se_lpm  = `temp2'[2,1]
	local p_lpm   = `temp2'[4,1]

	di "get_loaned — ATT: `att_lpm'  SE: `se_lpm'  p-value: `p_lpm'"

	/*------------------------------------------------------------------------------
	  3d. Compile results into a matrix and export to LaTeX
	------------------------------------------------------------------------------*/

	matrix A = J(2, 3, .)
	matrix A[1,1] = `att_log'
	matrix A[1,2] = `se_log'
	matrix A[1,3] = `p_log'
	matrix A[2,1] = `att_lpm'
	matrix A[2,2] = `se_lpm'
	matrix A[2,3] = `p_lpm'

	matrix colnames A = ATT SE P_value
	matrix rownames A = "Log-OLS (csdid)" "LPM (csdid)"

	matrix list A   // display for log verification

	esttab matrix(A, fmt(%9.3f)) using "$tabular/table_5_robust.tex", ///
		replace booktabs ///
		star(* 0.10 ** 0.05 *** 0.01) ///
		title("Robustness Check: Callaway \& Sant'Anna (2021) DiD Estimator") ///
		collabels("ATT" "Std. Error" "p-value") ///
		noobs nomtitles label

	save "$cleaned/master_robost.dta", replace


/*==============================================================================
  PART 4: HETEROGENEITY ANALYSIS — EFFECT ACROSS BOOK CATEGORIES
  Input  : $raw/original_data.csv, $cleaned/master.dta,
           $cleaned/master_table_5.dta
  Output : $graphic/category_bar_comparison.png
           $tabular/art_vs_edu_comparison.tex
==============================================================================*/

	/*------------------------------------------------------------------------------
	  Note:
	
	  The average treatment effect estimated above may mask heterogeneity across
	  content types. For example, the substitution of digital for physical access
	  could be stronger for reference-oriented books (e.g., Education, Science)
	  than for books where the reading experience itself matters (e.g., Fiction,
	  Art). To fix this, in this section I merges book-category information from the paper's
	  replication package and examines whether the digitization effect differs
	  across genres.
	------------------------------------------------------------------------------*/

	/*------------------------------------------------------------------------------
	  4a. Import and prepare the book-category dataset
	------------------------------------------------------------------------------*/

	/* original_data.csv contains book metadata scraped from Wikipedia and Google,
	   including a genre/category field (volumeinfocategories).                  */

	import delimited "$raw/original_data.csv", clear

	keep title volumeinfocategories
	rename title             title_display
	rename volumeinfocategories book_category

	label var title_display  "Book Title"
	label var book_category  "Book Genre/Category"

	* Remove books with no title and deduplicate
	drop if missing(title_display)
	duplicates drop title_display, force

	encode title_display, gen(title_display_encoded)

	save "$interm/book_category.dta", replace

	/*------------------------------------------------------------------------------
	  4b. Sample master dataset to stay within Stata's encode limit (60,000 labels)
	------------------------------------------------------------------------------*/

	/* Stata's encode command supports at most 60,000 value labels. Because the
	   master dataset has ~88,000 unique titles, I randomly downsample to 60,000
	   unique titles before encoding and merging.                                */

	use "$cleaned/master.dta", clear

	* Create a subset of unique book titles for the category merge
	preserve
		duplicates drop title_display, force
		save "$interm/master_unique_titles.dta", replace
	restore

	use "$interm/master_unique_titles.dta", clear
	count   // ~84,687 unique titles before sampling

	drop if missing(title_display)
	isid title_display

	* Reproducible random sample of 60,000 titles
	set seed 12345
	gen rand = runiform()
	sort rand
	keep in 1/60000
	drop rand

	encode title_display, gen(title_display_encoded)
	isid title_display_encoded

	/*------------------------------------------------------------------------------
	  4c. Merge category information onto the sampled title list
	------------------------------------------------------------------------------*/

	merge 1:1 title_display_encoded using "$interm/book_category.dta"

	* Keep only matched observations (titles with category information)
	keep if _merge == 3
	drop _merge

	save "$interm/merged_unique_titles.dta", replace

	/*------------------------------------------------------------------------------
	  4d. Merge category data back to the full panel using bib_doc_id
	------------------------------------------------------------------------------*/

	merge 1:m bib_doc_id using "$cleaned/master_table_5.dta"

	keep if _merge == 3    // 167,868 panel observations with category information
	drop _merge

	drop title_display_encoded _oclc

	/*------------------------------------------------------------------------------
	  4e. Clean the category string variable
	------------------------------------------------------------------------------*/

	replace book_category = regexs(1) if regexm(book_category, "'([^']*)'")

	save "$cleaned_master_category.dta", replace

	/*------------------------------------------------------------------------------
	  4f. Visualisation: average log loans by category, pre vs. post digitization
	------------------------------------------------------------------------------*/

	use "$cleaned_master_category.dta", clear

	drop if missing(book_category)

	* Identify and display the 10 most common categories (for reference)
	preserve
		contract book_category, freq(category_count)
		gsort -category_count
		list book_category category_count in 1/10
	restore

	* Assign each book to one of five focal categories, or "Other"
	gen category_group = "Other"
	local selected "History Fiction Science Education Art"
	foreach var of local selected {
		replace category_group = "`var'" if book_category == "`var'"
	}

	drop book_category
	encode category_group, gen(cat_factor)

	* Keep only digitized books and the five focal categories
	keep if !missing(year_scanned)
	keep if inlist(category_group, "History", "Fiction", "Science", "Education", "Art")

	* Collapse to category × treatment-status means for plotting
	preserve
		collapse (mean) log_loans (count) n = log_loans, ///
			by(category_group post_scanned)

		label define treat 0 "Pre-Digitization" 1 "Post-Digitization"
		label values post_scanned treat

		* Grouped bar chart: each category shows pre- and post-digitization bars
		graph bar log_loans, over(post_scanned) over(category_group) ///
			ytitle("Average Log Loans") ///
			asyvars ///
			bar(1, color(navy)) bar(2, color(cranberry)) ///
			legend(position(top) row(1))

		graph export "$graphic/category_bar_comparison.png", replace
	restore

	/*------------------------------------------------------------------------------
	  4g. Regression analysis: Art vs. Education
	------------------------------------------------------------------------------*/

	reghdfe log_loans post_scanned if category_group == "Art", ///
		absorb(bib_doc_id year#location_num) ///
		vce(cluster bib_doc_id)
	eststo Art_reg

	reghdfe log_loans post_scanned if category_group == "Education", ///
		absorb(bib_doc_id year#location_num) ///
		vce(cluster bib_doc_id)
	eststo Edu_reg

	esttab Art_reg Edu_reg using "$tabular/art_vs_edu_comparison.tex", replace ///
		b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
		keep(post_scanned) ///
		mtitles("Art" "Education") ///
		title("Differential Effects of Digitization: Art vs. Education Books") ///
		prefoot("\midrule Book FE & Yes & Yes \\ Year-Location FE & Yes & Yes \\") ///
		addnote("Book and year×location fixed effects included in all specifications.")


/*==============================================================================
  CLOSE LOG
==============================================================================*/

log close

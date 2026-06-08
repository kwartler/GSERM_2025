# =============================================================================
# run_scraper_US.R
# Collect reviews from the US Glassdoor site (glassdoor.com), in ENGLISH.
# Edit the section marked CONFIGURE and run. Do not modify glassdoor_scraper.R.
#
# >>> THIS SCRIPT NEEDS A US IP ADDRESS <<<
# Glassdoor sends you to a country-specific site based on your IP. From
# Switzerland you will be redirected to de.glassdoor.ch and the .com URLs will
# fail. To collect US/English reviews you must appear to be in the US:
#   - EASIEST: turn on a US VPN on your computer, then leave PROXY <- NULL.
#   - OR: set a US proxy in the PROXY setting below.
# If you want GERMAN/Swiss reviews instead, use run_scraper_DE.R (no VPN needed).
#
# IMPORTANT — PLEASE READ BEFORE PROCEEDING:
# -----------------------------------------------------------------------------
# Glassdoor's Terms of Service (Section 3) explicitly prohibit automated
# scraping of their website:
#   https://www.glassdoor.com/about/terms.htm
#
# This script is provided to support academic research. Using it involves a
# legal and ethical judgement call that is yours to make — not your supervisor's.
# Before proceeding you should:
#
#   (a) Read Glassdoor's ToS yourself.
#   (b) Check whether your institution's IRB or ethics board has guidance on
#       scraping data from platforms with ToS restrictions.
#   (c) Consider contacting Glassdoor directly for a data access agreement —
#       some institutions have obtained one for research purposes.
#   (d) If you proceed, document this clearly in your methods chapter, including
#       the ToS restriction, the small-scale/non-commercial nature of the use,
#       and how the data is stored and protected.
#
# Practically, Glassdoor has not historically pursued legal action against
# small-scale academic scrapers, but that is a risk assessment you must make
# for yourself. This note exists so you are making an informed decision.
# =============================================================================

source("glassdoor_scraper.R")

MARKET <- GD_MARKET_US   # US site, English reviews


# =============================================================================
# CONFIGURE — edit this section
# =============================================================================

# Company to scrape. Find these values from the Glassdoor URL when you visit
# the company's reviews page in a normal browser, e.g.:
#   https://www.glassdoor.com/Reviews/Apple-Reviews-E1138.htm
#                                     ^^^^^ name       ^^^^ id
EMPLOYER_NAME <- "Apple"   # URL slug (capitalisation matters)
EMPLOYER_ID   <- "1138"    # numeric ID from URL

# How many pages of reviews to collect. Each page has ~10 reviews.
# Start small (e.g. 2-3 pages) to verify it works before a large run.
MAX_PAGES <- 5

# Where to save the output files
OUTPUT_DIR <- "output"

# US IP address (see the note at the top of this file):
#   - leave NULL and use a US VPN on your computer, OR
#   - set a US proxy, e.g. "http://USER:PASS@us-host:PORT" (or "socks5://host:port").
PROXY <- NULL

# =============================================================================
# RUN — do not edit below this line
# =============================================================================

# Launch browser (always opens a visible window — required for manual login)
session <- gd_start_session(market = MARKET, proxy = PROXY)

# A browser window will open — log in manually (Google, email, whatever you use)
gd_wait_for_login(session, market = MARKET)

# Scrape reviews
message(sprintf("\nScraping %s reviews for: %s (ID: %s)",
                MARKET$key, EMPLOYER_NAME, EMPLOYER_ID))
reviews <- gd_scrape_company(
  session       = session,
  employer_name = EMPLOYER_NAME,
  employer_id   = EMPLOYER_ID,
  max_pages     = MAX_PAGES,
  market        = MARKET
)

# Save output
if (nrow(reviews) > 0) {
  gd_save(reviews, out_dir = OUTPUT_DIR,
          prefix = paste0(tolower(EMPLOYER_NAME), "_US"))
  message(sprintf("\nDone. %d reviews collected.", nrow(reviews)))
  print(head(reviews))
} else {
  message("No reviews were collected. Check the browser window for errors ",
          "(CAPTCHA, login failure, or wrong country — did the page redirect ",
          "away from glassdoor.com?).")
}

# Close browser window and clean up when finished
gd_close(session)

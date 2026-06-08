# =============================================================================
# debug_DE.R
# Diagnostic run for the DE site. Use this when run_scraper_DE.R collects 0
# reviews. It logs you in, opens the reviews page, and saves the page's HTML to
# output/debug_page.html plus prints a summary. Send that file (or the summary)
# back so the parser can be fixed to match the current page layout.
# =============================================================================

source("~/Downloads/glassdoorGET/glassdoor_scraper.R")
# EXAMPLE PATH: source("~/Downloads/glassdoorGET/glassdoor_scraper.R")

MARKET        <- GD_MARKET_DE
EMPLOYER_NAME <- "Apple"
EMPLOYER_ID   <- "1138"

session <- gd_start_session(market = MARKET)
gd_wait_for_login(session, market = MARKET)

url <- gd_reviews_url(EMPLOYER_NAME, EMPLOYER_ID, 1, market = MARKET)
message("Navigating to: ", url)
session$Page$navigate(url)
Sys.sleep(5)   # give the page time to render

gd_debug_page(session)

# Leave the browser open so you can look at the page yourself; close it with:
#   gd_close(session)

# =============================================================================
# run_scraper_DE.R
# Collect reviews from the Swiss/German Glassdoor site (de.glassdoor.ch),
# in GERMAN. Edit the section marked CONFIGURE and run.
# Do not modify glassdoor_scraper.R.
#
# >>> THIS SCRIPT WORKS DIRECTLY FROM A SWISS / GERMAN IP — NO VPN NEEDED <<<
# Because Glassdoor already routes a Swiss IP to de.glassdoor.ch, this is the
# natural site to use while you are in Switzerland. The reviews collected will
# be the German-language reviews shown on that site.
# If you want US/English reviews instead, use run_scraper_US.R (needs a US VPN).
#
# IMPORTANT — PLEASE READ BEFORE PROCEEDING:
# -----------------------------------------------------------------------------
# Glassdoor's Terms of Service explicitly prohibit automated scraping of their
# website (the same terms apply across all country sites):
#   https://www.glassdoor.com/about/terms.htm   (DE: https://de.glassdoor.ch/about/terms.htm)
#
# This script is provided to support academic research. Using it involves a
# legal and ethical judgement call that is yours to make — not your supervisor's.
# Before proceeding you should:
#
#   (a) Read Glassdoor's ToS yourself.
#   (b) Check whether your institution's IRB or ethics board has guidance on
#       scraping data from platforms with ToS restrictions. Note that German/
#       Swiss data-protection rules (GDPR / Swiss FADP) may also apply to review
#       text that contains personal information.
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
# EXAMPLE PATH source("~/Downloads/glassdoorGET/glassdoor_scraper.R")

MARKET <- GD_MARKET_DE   # Swiss/German site, German reviews


# =============================================================================
# CONFIGURE — edit this section
# =============================================================================

# Company to scrape. Find these values from the German Glassdoor URL when you
# visit the company's reviews page in a normal browser, e.g.:
#   https://de.glassdoor.ch/Bewertungen/Apple-Bewertungen-E1138.htm
#                                       ^^^^^ name           ^^^^ id
# The numeric ID (E1138) is the same company ID used on every Glassdoor site,
# so a company's US and DE pages share the same ID — only the name slug and the
# word "Bewertungen" differ from the US "Reviews".
EMPLOYER_NAME <- "Apple"   # URL slug (capitalisation matters)
EMPLOYER_ID   <- "1138"    # numeric ID from URL

# How many pages of reviews to collect. Each page has ~10 reviews.
# Start small (e.g. 2-3 pages) to verify it works before a large run.
MAX_PAGES <- 5

# Where to save the output files
OUTPUT_DIR <- "output"

# Proxy is normally NOT needed here — a Swiss/German IP already reaches this
# site. Leave NULL unless you specifically need to route through a proxy.
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
          prefix = paste0(tolower(EMPLOYER_NAME), "_DE"))
  message(sprintf("\nDone. %d reviews collected.", nrow(reviews)))
  print(head(reviews))
} else {
  message("No reviews were collected. Check the browser window for errors ",
          "(CAPTCHA, login failure, or wrong employer name/ID).")
}

# Close browser window and clean up when finished
gd_close(session)

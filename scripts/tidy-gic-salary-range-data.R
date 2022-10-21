library(tidyverse)

# Copied the data from:
# > https://www.canada.ca/en/privy-council/programs/appointments/governor-council-appointments/compensation-terms-conditions-employment/salary-ranges-performance-pay.html
#
# Older salary ranges available from previous PCO site.
#   2018 scrape, for:
#   - 2013: https://web.archive.org/web/20180201222842/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2013-eng.htm
#   - 2012: https://web.archive.org/web/20180201222844/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2012-eng.htm
#   - 2011: https://web.archive.org/web/20180201222845/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2011-eng.htm
#   2015 scrape, for:
#   - 2010: https://web.archive.org/web/20150613040456/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2010-eng.htm
#
# Into a format like so:
# -----------------------------------
# fiscal_year_start	group_level	salary_min	salary_max	max_performance_award
# 2021	EX 1	$121,550	$142,982	15.00%
# EX 2	$136,270	$160,299	15.00%
# EX 3	$152,506	$179,348	15.00%
# EX 4	$174,802	$205,650	26.00%
# EX 5	$195,908	$230,436	26.00%
# DM 1	$219,300	$258,000	26.00%
# DM 2	$252,000	$296,400	33.00%
# DM 3	$282,200	$332,000	33.00%
# DM 4	$315,900	$371,600	39.00%
# -----------------------------------
# But this is hardly tidy, so let’s clean it up! We need to:
# - fill down the fiscal years (currently just one per section)
# - break out group and level
# - convert salary_min and salary_max to integers
# - convert max_performance_award to a double (15.00% -> 0.15)

gic_salary_ranges_raw <- read_tsv("data/source/canada.ca/salary-ranges-gic-appointees-raw.tsv")

gic_salary_ranges <- gic_salary_ranges_raw %>%
  fill(fiscal_year_start, .direction = "down") %>%
  mutate(
    group_level = str_replace_all(group_level, coll("-"), " "),
    group_level = str_replace_all(group_level, coll("GX"), "GX 0") # fake number for GX level
  ) %>%
  separate(group_level, into = c("group", "level"), remove = FALSE, convert = TRUE) %>%
  mutate(across(
      contains("salary"),
      ~ as.integer(str_remove_all(.x, "[^[0-9]]"))
  )) %>%
  mutate(
    max_performance_award = as.double(str_remove(max_performance_award, "%$")) / 100
  ) %>%
  arrange(desc(fiscal_year_start), group, level)

gic_salary_ranges %>%
  write_csv("data/source/canada.ca/salary-ranges-gic-appointees.csv")

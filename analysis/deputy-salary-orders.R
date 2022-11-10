source("load.R")

source("lib/helpers.R")

library(tidytext)
library(lubridate)
library(fuzzyjoin)

# find deputy salary orders with English text and salary ranges
deputy_salary_order_attachments <- salary_order_attachments %>%
  filter(pc_number %in% (
    salary_orders %>%
      filter(str_detect(precis, "Deputy Min")) %>%
      pull(pc_number)
  )) %>%
  filter(
    str_detect(text, "Governor General in Council"),
    str_detect(text, "within the range")
  ) %>%
  select(id, pc_number, date, text)

gic_appointee_order_attachments <- salary_order_attachments %>%
  filter(pc_number %in% (
    salary_orders %>%
      filter(str_detect(precis, "(?:persons|individuals) appointed by the Governor")) %>%
      pull(pc_number)
  )) %>%
  filter(
    str_detect(text, "Governor General in Council"),
    str_detect(text, "within the range"),
    str_detect(text, "Deputy Min"),
    ! pc_number == "2007-2023" # remove an OIC that has mostly non-DM revisions (the one DM revision is manually re-added, below)
  ) %>%
  select(id, pc_number, date, text)


# for debugging / text reference
deputy_salary_order_attachments %>%
  select(id, pc_number, date, text) %>%
  write_csv("data/out/deputy-salary-order-attachments.csv")

gic_appointee_order_attachments %>%
  select(id, pc_number, date, text) %>%
  write_csv("data/out/gic-appointee-salary-order-attachments.csv")

salary_revisions_raw <- bind_rows(
  deputy_salary_order_attachments,
  gic_appointee_order_attachments
) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    text = str_remove_all(text, regex("^PC Number: [0-9]{4}-[0-9]{4}$", multiline = TRUE)), # opening PC number
    text = str_remove_all(text, regex("^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}$", multiline = TRUE)), # opening date
    text = str_remove_all(text, regex("^Write closure fall-back .*|^var defPreFooter .*", multiline = TRUE)), # accidental javascript]
    text = str_remove_all(text, regex("^…/[0-9]+$|^ ?- [0-9]+ -$", multiline = TRUE)), # page numbers
    text = str_replace_all(text, coll("; and"), ";"),
    text = str_replace_all(text, coll(";"), "."),
    text = str_replace_all(text, regex("([a-z,])(\n\n)"), "\\1 "), # fix for 2+ line breaks within an entry
    text = str_replace_all(text, coll("–"), "-"),
    text = str_replace_all(text, coll("witihin"), "within"),
    text = str_replace_all(text, coll("andwithin"), "and within"),
    text = str_replace_all(text, coll("within the ange"), "within the range"),
    text = str_replace_all(text, coll("‑"), "-"),
    text = str_replace_all(text, coll("Council,on"), "Council, on"),
    text = str_replace_all(text, coll("Mnister"), "Minister")
  ) %>%
  unnest_tokens(salary_revision, text, token = "paragraphs", to_lower = FALSE) %>%
  mutate(
    salary_revision = str_squish(salary_revision)
  ) %>%
  filter(
    ! salary_revision == "",
    ! str_detect(salary_revision, "échelle|compter du|à compter"), # French entries
    ! str_detect(salary_revision, "^Sur recommandation du premier ministre"), # French entries
    ! str_detect(salary_revision, "^…"), # errant page number
    ! str_detect(salary_revision, "in the amount and on the date indicated in the annexed schedule, of:$") # intro phrases for post-2015 revisions
  ) %>%
  bind_rows(# manually add in an errant DM revision found amidst a bunch of others
    tibble(
      id = 17846,
      pc_number = "2007-2023",
      date = as_date("2007-12-18"),
      salary_revision = "Her Excellency the Governor General in Council, on the recommendation of the Prime Minister, hereby fixes the salary of Ian E. Bennett, former Deputy Minister, Department of Finance, as set out in the schedule hereto, which salary is within the range ($245,100 - $288,400), for the period commencing April 1, 2006 and terminating June 11, 2006."
    )
  )

# for debugging / text reference
salary_revisions_raw %>%
  write_csv("data/out/deputy-salary-revisions-raw.csv")



# The form differs for salary revisions pre/post 2015. (There are none in 2015, handily!)
# We can use a different handling function for each.
# 
# Before 2015, they begin with a preamble:
# > "His Excellency the Governor General in Council, on the recommendation of the Prime Minister, hereby fixes the salary of "
# 
# After 2015, there’s no preamble.
# 
# We can use the presence of the preamble (or just the year!) to sort out which handling function to use.
# 
# For both, though, we'll need to capture:
# 
# - name
# - position
# - (possible multiple per entry)
#   - salary min
#   - salary max
#   - effective date (occasionally with specific "from" and "to")

salary_revisions_pre_2015 <- salary_revisions_raw %>%
  filter(year(date) < 2015) %>%
  filter(
    ! str_detect(salary_revision, "within the range GCQ? [0-9]"),
    ! str_detect(salary_revision, regex("within the Crown Corporation Group [0-9]", ignore_case = TRUE)),
    ! str_detect(salary_revision, regex("which per diem rate", ignore_case = TRUE)),
    ! str_detect(salary_revision, "of Members \\(|of Lay Members")
  ) %>%
  mutate(
    salary_revision = str_remove(salary_revision, "^His Excellency the Governor General in Council, on the recommendation of the Prime Minister, hereby fixes the salary of "),
    salary_revision = str_remove(salary_revision, "^Her Excellency the Governor General in Council, on the recommendation of the Prime Minister, hereby fixes the salary (and employment conditions|and other employment conditions|and conditions of employment)? ?of "),
    salary_revision = str_remove(salary_revision, "^Her Excellency the Governor General in Council, on the recommendation of the Prime Minister, hereby fixes the salary of "),
    salary_revision = str_replace_all(salary_revision, coll(", s set out in the schedule"), ", as set out in the schedule"),
    salary_revision = str_replace_all(salary_revision, coll(", set out in the schedule"), ", as set out in the schedule")
  ) %>%
  separate(salary_revision, sep = ", ", into = c("name_full", "salary_revision"), extra = "merge") %>%
  separate(salary_revision, sep = ", as set out in the schedule hereto, which salary is within the range \\(", into = c("position", "salary_revision"), extra = "merge") %>%
  separate(salary_revision, sep = " – | - |\\), ", into = c("salary_min", "salary_max", "salary_revision"), extra = "merge") %>%
  mutate(
    salary_revision = str_replace_all(salary_revision, coll("starting"), "commencing"),
    salary_revision = str_remove(salary_revision, ".$")
  ) %>%
  mutate(
    salary_revision = str_replace(salary_revision, coll("30 September"), "September 30") # one date out of format, in all the years!
  ) %>%
  extract(
    salary_revision,
    into = c("start", "end"),
    "(?:commencing|effective) ([A-Za-z]* ?[0-9]+ ?, ?[0-9]{4})(?: and terminating)?(?: on)? ?([A-Za-z]* ?[0-9]+ ?, ?[0-9]{4})?"
  ) %>%
  filter(
    ! is.na(end), # catch a few oddballs (none of them are in-scope)
    ! name_full == "Her Excellency the Governor General in Council", # some more oddballs
    ! name_full == fixed("7(1) of the National Capital Act")
  )

salary_revisions_pre_2015 %>% write_csv("data/out/deputy-salary-levels-revisions-pre-2015.csv")


salary_revisions_post_2015 <- salary_revisions_raw %>%
  filter(year(date) > 2015) %>%
  mutate(
    # Handle some name edge cases:
    # - "Natynczyk, Walter J., General (Retired), ..."
    # - "Sabia, Michael Jonathan, O.C., ..."
    # - "Shugart, P.C., Senator the Honourable Ian"
    salary_revision = str_remove_all(
      salary_revision,
      paste("General \\(Retired\\), ", "O\\.C\\., ", "P\\.C\\., Senator the Honourable", sep = "|")
    ),
    salary_revision = str_replace_all(salary_revision, "Natynczyk, Walter John,|Natynczyk, Walter,|Natynczyk, Walter J.,", "Natynczyk, Walter J.,"),
    salary_revision = str_replace_all(
      salary_revision,
      "within the range of",
      "within the range"
    ),
    salary_revision = str_replace_all(salary_revision, "Agency within", "Agency, within"),
    salary_revision = str_replace_all(salary_revision, "Cabinet within", "Cabinet, within")
  ) %>%
  separate(salary_revision, sep = ", ", into = c("name_last", "name_first", "salary_revision"), extra = "merge") %>%
  mutate(name_full = str_squish(paste(name_first, name_last))) %>%
  select(id:name_first, name_full, everything()) %>%
  separate(salary_revision, sep = ", within the range \\(", into = c("position", "salary_revision"), extra = "merge") %>%
  mutate(
    salary_revision = paste0(", within the range (", salary_revision),
    salary_revision = str_replace_all(salary_revision, " and within the range", ", within the range"),
    salary_revision = str_replace_all(salary_revision, ",within the range", ", within the range"),
    salary_revision = str_remove(salary_revision, ".$")
  ) %>%
  mutate(
    salary_revision = str_split(salary_revision, ", within the range \\(")
  ) %>%
  unnest_longer(salary_revision) %>%
  filter(! salary_revision == "") %>%
  mutate(
    salary_revision = str_replace_all(salary_revision, coll(") effective"), "), effective"),
    salary_revision = str_replace_all(salary_revision, coll(") and effective"), "), effective")
  ) %>%
  separate(salary_revision, sep = " – | - |\\), ", into = c("salary_min", "salary_max", "salary_revision"), extra = "merge") %>%
  extract(
    salary_revision,
    into = c("start", "end"),
    "(?:commencing|effective)(?: on)? ([A-Za-z]* ?[0-9]+ ?, ?[0-9]{4})(?: and ending)?(?: on)? ?([A-Za-z]* ?[0-9]+ ?, ?[0-9]{4})?"
  )

salary_revisions_post_2015 %>% write_csv("data/out/deputy-salary-levels-revisions-post-2015.csv")

salary_revisions <- bind_rows(
  salary_revisions_pre_2015,
  salary_revisions_post_2015
) %>%
  mutate(
    end = if_else(end == "", NA_character_, end),
    start = mdy(start),
    end = mdy(end),
    across(contains("salary"), ~ as.integer(str_remove_all(.x, "[^0-9]")))
  ) %>%
  mutate(
    salary_range = salary_max - salary_min
  ) %>%
  standardize_positions() %>%
  standardize_names() %>%
  select(-name_last, -name_first) %>%
  select(
    id:name_full,
    name_standardized,
    position,
    position_standardized,
    position_portfolio_department,
    salary_min:salary_max,
    salary_range,
    start:end,
    everything()
  )

# salary_revisions %>% count(position_portfolio_department, sort = TRUE) %>% mutate(prop = round(n / sum(n), 2))
# salary_revisions %>% count(position_standardized, position_portfolio_department) %>% View()
# salary_revisions %>% count(position_standardized, position_portfolio_department) %>% filter(is.na(position_portfolio_department)) %>% arrange(-n) %>% View()

# TODO: filter out...
# - ambassador
# - consul (as whole word)
# filter in...
# - "deputy"
# - "secretary"
# - "minister"
# - "advisor"
# - "lead"
# - "coordinator"
# - "comptroller"
# - "officer"
# - "commissioner"
# - "clerk"
# - "president"
# - "chief"
# - "attorney"

salary_revisions %>% count(position, sort = TRUE)
salary_revisions %>% count(position_standardized, sort = TRUE)
salary_revisions %>% count(position_standardized, sort = TRUE) %>% View

# TODO: for position_standardized, look at: ','


# Find the _last_ revision for a given combo of [person, position, date range]
#   salary_revisions_post_2015 %>% arrange(name_full, position, start, end)
# Or maybe group_by, then arrange, with `date` as final arbiter?
# Also deal with `end` somehow? May need to fill down, or such
#
# Multi-step: find the fiscal year for a given revision (based on `start`—check if `end` ever exceeds a FY?), then fuzzyjoin
# Confirm that the timespan for entries never exceeds a year:
# salary_revisions_post_2015 %>%
#   filter(! is.na(end)) %>%
#   mutate(revision_years = time_length(start %--% end, "years")) %>%
#   filter(revision_years >= 1)
salary_revisions_classified <- salary_revisions %>%
  mutate(
    start = case_when(
      pc_number == "2019-1324" & name_full == "Andrea Lyon" & position == "Senior Advisor to the Privy Council Office" & start == "2018-04-01" ~ ymd("2019-01-07"), # ref: https://pm.gc.ca/en/news/news-releases/2018/12/07/prime-minister-announces-changes-senior-ranks-public-service
      pc_number == "2020-0974" & name_full == "Ava Yaskiel" & position == "Associate Deputy Minister of Finance with G7 and G20 responsibilities" & start == "2020-04-01" ~ ymd("2020-07-24"), # ref: https://orders-in-council.canada.ca/attachment.php?attach=39511&lang=en
      pc_number == "2016-0803" & name_full == "Liseanne Forand" & position == "Senior Advisor to the Privy Council Office" & start == "2015-04-01" ~ ymd("2015-07-06"), # ref: https://orders-in-council.canada.ca/attachment.php?attach=31359&lang=en
      pc_number == "2019-1324" & name_full == "Marie Lemay" & position == "Senior Advisor to the Privy Council Office" & start == "2018-04-01" ~ ymd("2019-01-28"), # ref: https://orders-in-council.canada.ca/attachment.php?attach=37324&lang=en
      pc_number == "2019-1324" & name_full == "Ronald Parker" & position == "Senior Advisor to the Privy Council Office" & start == "2018-04-01" ~ ymd("2018-12-17"), # ref: https://orders-in-council.canada.ca/attachment.php?attach=37216&lang=en
      TRUE ~ start
    ),
    end = case_when(
      pc_number == "2022-1092" & name_full == "Ava Yaskiel" & position == "Associate Deputy Minister of Finance with G7 and G20 responsibilities" & start == "2021-04-01" ~ ymd("2021-08-31"), # ref: https://orders-in-council.canada.ca/attachment.php?attach=39511&lang=en (no subsequent appointment OICs)
      pc_number == "2011-1158" & name_full == "Karen Jackson" & position == "Associate Deputy Minister of Human Resources and Skills Development" & start == "2011-04-01" ~ ymd("2011-08-03"), # ref: https://orders-in-council.canada.ca/attachment.php?attach=26486&lang=en (typo in date, 2001 instead of 2011)
      TRUE ~ end
    )
  ) %>%
  mutate(fiscal_year_start = map_dbl(start, get_fiscal_year_start_for_date)) %>%
  group_by(name_standardized, position_standardized, start) %>%
  fill(end, .direction = "updown") %>%
  group_by(name_standardized, position_standardized, start, end) %>%
  slice_tail(n = 1) %>% # to check for possible errors (i.e., multiple revisions for same position in same fiscal year), run this after slice_tail: %>% group_by(name_full, position, fiscal_year_start) %>% count(fiscal_year_start) %>% filter(n > 1) %>% write_csv("data/out/multiple-revisions-in-fiscal.csv")
  classify_group_levels_for_salary() %>%
  estimate_end_dates() # TODO: finish end dates work, in comment below ("setting end dates...")

zz <- salary_revisions_classified %>%
  estimate_end_dates()

zz %>%
  select(
    pc_number,
    name_standardized,
    position_standardized,
    start,
    end,
    end_is_estimated,
    time_until_next
  )

# setting end dates...
# - group by name
#   - need to standardize names first...
# - for each row, if.na(end), set end to lead(start) - 1
#   - but not if the time_length from end to start would then be more than 1 yr? 2 yrs? (there's a gap in 2012 for most we may want to cover)
#   - assumes there's no duplicate starts?
# once set, gut checks...
# - some negative values for `time_until_next_revision`, to investigate
# - no `end_is_estimated == TRUE` with a `time_until_next_revision > 0.002732240` (~one day)
# missing end dates...
# - basic set: `filter(is.na(time_until_next_revision), is.na(end))`
# - from that set, can filter out anything with `start >= 2021-04-01`, since we'll assume any of those are current
# - once we have more salary ranges, we could then filter out any that are unclassified (one reason there are a bunch without ends is we included a few salary orders that were for other groups _but included a few DMs_, so that'd muck things up)

# find unclassified entries
salary_revisions_classified %>%
  filter(is.na(matched_group_level))

salary_revisions_classified %>%
  filter(month(start) == 4, day(start) == 1) %>%
  count(fiscal_year_start, matched_group_level) %>%
  ggplot(aes(x = fiscal_year_start, y = n, colour = matched_group_level, fill = matched_group_level)) +
  geom_col(position = "dodge")

library(plotly)

positions_with_more_than_one_level <- salary_revisions_classified %>%
  group_by(position_standardized) %>%
  distinct(matched_level) %>%
  ungroup() %>%
  count(position_standardized) %>%
  filter(n > 1) %>%
  pull(position_standardized)

ggplotly(salary_revisions_classified %>%
  filter(
    position_standardized %in% (
      salary_revisions_classified %>%
        filter(position_standardized %in% positions_with_more_than_one_level) %>%
        count(position_standardized, sort = TRUE) %>%
        slice_head(n = 15) %>%
        pull(position_standardized)
    )
  ) %>%
  ggplot(aes(x = start, y = matched_level, colour = position_standardized)) +
  geom_point() +
  geom_line()
)


summarize_matched_levels_by_group <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarize(
      count = n(),
      min = min(matched_level, na.rm = TRUE),
      max = max(matched_level, na.rm = TRUE),
      avg = round(mean(matched_level, na.rm = TRUE), 1),
      median = median(matched_level, na.rm = TRUE),
      first = first(matched_level, order_by = start),
      last = last(matched_level, order_by = start)
    ) %>%
    mutate(
      diff_min_max = max - min,
      diff_max_last = max - last
    )
}

salary_revisions_classified %>%
  summarize_matched_levels_by_group(name_full) %>%
  View("summary levels by person")

salary_revisions_classified %>%
  summarize_matched_levels_by_group(position_standardized) %>%
  View("summary levels by position")

salary_revisions_classified %>%
  summarize_matched_levels_by_group(position_portfolio_department) %>%
  View("summary levels by portfolio")

salary_revisions_classified %>%
  summarize_matched_levels_by_group(position_seniority, position_portfolio_department) %>%
  arrange(position_portfolio_department) %>%
  View("summary levels by seniority, portfolio")

salary_revisions_classified %>%
  filter(
    str_detect(position_standardized, "indian|aboriginal|indigenous"),
    ! str_detect(position_standardized, "associate")
  ) %>%
  summarize_matched_levels_by_group()




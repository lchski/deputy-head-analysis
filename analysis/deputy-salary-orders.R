source("load.R")

library(tidytext)
library(lubridate)

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

# for debugging / text reference
deputy_salary_order_attachments %>%
  select(id, pc_number, date, text) %>%
  write_csv("data/out/deputy-salary-order-attachments.csv")

salary_revisions_raw <- deputy_salary_order_attachments %>%
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
    text = str_replace_all(text, coll("within the ange"), "within the range")
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
    salary_revision = str_replace_all(
      salary_revision,
      "within the range of",
      "within the range"
    )
  ) %>%
  separate(salary_revision, sep = ", ", into = c("name_last", "name_first", "salary_revision"), extra = "merge") %>%
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
  ) %>%
  mutate(
    position = str_remove_all(position, "^former "),
    end = if_else(end == "", NA_character_, end),
    start = mdy(start),
    end = mdy(end),
    across(contains("salary"), ~ as.integer(str_remove_all(.x, "[^0-9]")))
  )

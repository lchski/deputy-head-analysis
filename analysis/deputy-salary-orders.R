source("load.R")

library(tidytext)

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

salary_revisions <- deputy_salary_order_attachments %>%
  mutate(
    text = str_remove_all(text, regex("^PC Number: [0-9]{4}-[0-9]{4}$", multiline = TRUE)), # opening PC number
    text = str_remove_all(text, regex("^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}$", multiline = TRUE)), # opening date
    text = str_remove_all(text, regex("^Write closure fall-back .*|^var defPreFooter .*", multiline = TRUE)), # accidental javascript]
    text = str_remove_all(text, regex("^…/[0-9]+$|^ ?- [0-9]+ -$", multiline = TRUE)), # page numbers
    text = str_replace_all(text, coll("; and"), ";"),
    text = str_replace_all(text, coll(";"), "."),
    text = str_replace_all(text, regex("([a-z,])(\n\n)"), "$1")
  ) %>%
  unnest_tokens(salary_revision, text, token = "paragraphs", to_lower = FALSE) %>%
  mutate(
    salary_revision = str_squish(salary_revision)
  ) %>%
  filter(
    ! salary_revision == "",
    ! str_detect(salary_revision, "échelle|compter du"), # French entries
    ! str_detect(salary_revision, "^Sur recommandation du premier ministre"), # French entries
    ! str_detect(salary_revision, "^…"), # errant page number
    ! str_detect(salary_revision, "in the amount and on the date indicated in the annexed schedule, of:$") # intro phrases for post-2015 revisions
  )

# for debugging / text reference
salary_revisions %>%
  write_csv("data/out/deputy-salary-revisions.csv")



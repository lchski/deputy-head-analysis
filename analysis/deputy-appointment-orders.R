source("load.R")

library(tidytext)
library(lubridate)
library(fuzzyjoin)

deputy_appointment_orders

deputy_appointment_order_attachments

appointments_raw <- deputy_appointment_order_attachments %>%
  mutate(
    text = str_remove_all(text, regex("^PC Number: [0-9]{4}-[0-9]{4}$", multiline = TRUE)), # opening PC number
    text = str_remove_all(text, regex("^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}$", multiline = TRUE)), # opening date
    text = str_remove_all(text, regex("^Write closure fall-back .*|^var defPreFooter .*", multiline = TRUE)), # accidental javascript]
    text = str_remove_all(text, regex("^…/[0-9]+$|^ ?- [0-9]+ -$", multiline = TRUE)), # page numbers
    text = str_replace_all(text, regex("([a-z,])(\n\n)"), "\\1 ") # fix for 2+ line breaks within an entry
  ) %>%
  select(id, pc_number, date, text) %>%
  unnest_tokens(appointment, text, token = "paragraphs", to_lower = FALSE) %>%
  mutate(
    appointment = str_squish(appointment)
  ) %>%
  filter(
    ! appointment == "",
    ! str_detect(appointment, "échelle|compter du|à compter|en vertu|amovible"), # French entries
    ! str_detect(appointment, "^Sur recommandation du premier ministre"), # French entries
    ! str_detect(appointment, "^\\.\\.\\.")
  )

multiline_appointment_ids <- appointments_raw %>%
  count(id, sort = TRUE) %>%
  filter(n > 1) %>%
  pull(id)

# TODO: deal with these
multiline_appointments <- appointments_raw %>%
  filter(id %in% more_than_1_ids)

appointments <- appointments_raw %>%
  filter(
    ! id %in% more_than_1_ids,
    str_detect(appointment, "on the recommendation of the Prime Minister"), # a few odd ones, not DMs, recommended by not-PM
    str_detect(appointment, "appoints"), # TODO: deal with 14121, where an AssocDM is designated (not appointed) DM Int'l Trade, at what seems a higher level
    str_detect(appointment, "within the range"), # only ones with salary
    ! str_detect(appointment, "within the range GCQ? [0-9]")
  ) %>%
  mutate(
    appointment = str_replace_all(appointment, c(
      "reappoints" = "appoints",
      "Ontario to be" = "Ontario, to be",
      "Alberta to be" = "Alberta, to be"
    )),
    appointment = str_replace_all(appointment, "‑|–", "-"),
  ) %>%
  separate(appointment, into = c("authority", "appointment"), sep = ",(?: hereby)? appoints ") %>%
  separate(appointment, into = c("name_full", "appointment"), sep = " of ", extra = "merge") %>% # issue with 40284
  separate(appointment, into = c("city", "province", "appointment"), sep = ", ", extra = "merge") %>%
  filter(! city %in% c("Indian Affairs and Northern Development", "the Canadian Northern Economic Development Agency")) %>%
  separate(appointment, into = c("position", "appointment"), sep = ", to hold office during pleasure") %>%
  mutate(appointment = str_replace(appointment, "^ and fixes", ", and fixes")) %>%
  separate(appointment, into = c("term", "appointment"), sep = ", and fixes ") %>%
  separate(appointment, into = c("pronoun", "appointment"), sep = " ", extra = "merge") %>%
  mutate(appointment = str_remove(appointment, "^.* which (?:salary|remuneration) is within the range \\(")) %>%
  separate(appointment, sep = " - |\\), |\\)\\.|\\) , ", into = c("salary_min", "salary_max", "start"), extra = "merge") %>%
  mutate(
    start = str_remove(start, "^effective "),
    start = case_when(
      str_detect(start, "upon approval of the Order in Council") ~ date,
      TRUE ~ mdy(start)
    ),
    across(contains("salary"), ~ as.integer(str_remove_all(.x, "[^0-9]")))
  )
# TODO: use `term` to get start dates (where there isn't one already), and end dates (where provided directly; ignore "X years"?)

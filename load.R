library(tidyverse)

gic_salary_ranges <- read_csv("data/source/canada.ca/salary-ranges-gic-appointees.csv") %>%
  bind_rows( # 2022 ranges aren't out yet (won't be for... some time, probably!), so we carry the 2021 ranges forward
    read_csv("data/source/canada.ca/salary-ranges-gic-appointees.csv") %>%
      filter(fiscal_year_start == 2021) %>%
      mutate(fiscal_year_start = 2022)
  ) %>%
  arrange(group, fiscal_year_start, level)

deputy_appointment_orders <- read_csv("data/source/github.com/lchski/oic-data/deputy-appointment-orders.csv")
deputy_appointment_order_attachments <- read_csv("data/source/github.com/lchski/oic-data/deputy-appointment-order-attachments.csv") %>%
  left_join(
    deputy_appointment_orders %>%
      select(attachments, pc_number, date),
    by = c("id" = "attachments"))

# TODO: convert `attachments` to list[integer]
salary_orders <- read_csv("data/source/github.com/lchski/oic-data/salary-orders.csv") %>%
  mutate(attachments = str_split(attachments, ";"))
salary_order_attachments <- read_csv("data/source/github.com/lchski/oic-data/salary-order-attachments.csv") %>%
  left_join(
    salary_orders %>%
      unnest(attachments) %>%
      select(attachments, pc_number, date) %>%
      mutate(attachments = as.integer(attachments)),
    by = c("id" = "attachments"))

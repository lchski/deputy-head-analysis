library(tidyverse)

gic_salary_ranges <- read_csv("data/source/canada.ca/salary-ranges-gic-appointees.csv")

deputy_appointment_orders <- read_csv("data/source/github.com/lchski/oic-data/deputy-appointment-orders.csv")
deputy_appointment_order_attachments <- read_csv("data/source/github.com/lchski/oic-data/deputy-appointment-order-attachments.csv") %>%
  left_join(
    deputy_appointment_orders %>%
      select(attachments, pc_number, date),
    by = c("id" = "attachments")) %>%
  mutate(
    text = str_replace_all(text, "\n(?!\n\n)", " "),
    text = str_squish(text)
  ) %>%
  filter(str_detect(text, "which (salary|remuneration) is within the range")) # only attachments with salary ranges

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

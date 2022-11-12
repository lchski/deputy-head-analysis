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
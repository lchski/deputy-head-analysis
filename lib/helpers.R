get_fiscal_year_start_for_date <- function(dtc) {
  if (month(dtc) <= 3) {
    return(year(dtc) - 1)
  }
  
  return(year(dtc))
}

# assumes a df with column `position`
standardize_positions <- function(df) {
  df %>%
    mutate(
      position = str_remove_all(position, "^former "),
      position = str_replace_all(position, c(
        "Miniser" = "Minister",
        "Treasury of Canada Secretariat" = "Treasury Board",
        "Treasury Board of Canada Secretariat" = "Treasury Board"
      )),
      position = case_when(
        str_detect(position, "Deputy Secretary to the Cabinet \\(Plans and Consultations\\)$|Deputy Secretary to the Cabinet \\(Results and Delivery\\)$|Deputy Secretary to the Cabinet \\(Senior Personnel and Public Service Renewal\\)$") ~ paste0(position, ", Privy Council Office"),
        position == "Chief Public Health Officer of Canada" ~ "Chief Public Health Officer",
        position == "General, Chief of the Defence Staff" ~ "Chief of the Defence Staff",
        position == "Chief Information Officer for the Government of Canada" ~ "Chief Information Officer of Canada",
        TRUE ~ position
      )
    ) %>%
    mutate(
      position_standardized = str_to_lower(position),
      position_standardized = str_replace_all(position_standardized, c(
        "minister," = "minister",
        "director," = "director",
        "coordinator," = "coordinator",
        "secretary," = "secretary"
      )),
      position_standardized = str_remove_all(position_standardized, "to |the |of |for |deputy minister |government canada|department "),
      position_standardized = str_squish(position_standardized)
    ) %>%
    mutate(position_portfolio_department = case_when(
      str_detect(position_standardized, "national defence") ~ "DND",
      str_detect(position_standardized, "finance") ~ "FIN",
      str_detect(position_standardized, "privy council|cabinet|intergovernmental affairs") ~ "PCO",
      str_detect(position_standardized, "advisor prime minister") ~ "PCO",
      str_detect(position_standardized, "treasury board|comptroller general|chief information|chief human resources|public service human resources|public service accessibility") ~ "TBS",
      str_detect(position_standardized, "immigration") ~ "IRCC",
      str_detect(position_standardized, "social development|labour(?! relations)|employment and social|human resources and|transformation officer, service canada") ~ "ESDC",
      str_detect(position_standardized, "agriculture") ~ "AAFC",
      str_detect(position_standardized, "justice") ~ "JUS",
      str_detect(position_standardized, "public health") ~ "PHAC",
      str_detect(position_standardized, "health(?! and| research)") ~ "HC",
      str_detect(position_standardized, "indigenous|aboriginal|indian") ~ "INAC", # NB: definitely a simplifying decision here, given machinery history
      str_detect(position_standardized, "foreign affairs|international") ~ "GAC",
      str_detect(position_standardized, "veterans") ~ "VAC",
      str_detect(position_standardized, "heritage") ~ "PCH",
      str_detect(position_standardized, "women") ~ "WAGE",
      str_detect(position_standardized, "transport(?!ation)") ~ "TC",
      str_detect(position_standardized, "infrastructure") ~ "INFC",
      str_detect(position_standardized, "innovation|industry|economic development|economic diversification|atlantic canada opportunities") ~ "ISED", # TBC: want to split economic development agencies into constituent parts?
      str_detect(position_standardized, "natural resources") ~ "NRCan",
      str_detect(position_standardized, "environment") ~ "ECCC",
      str_detect(position_standardized, "fisheries|coast guard") ~ "DFO",
      str_detect(position_standardized, "public safety") ~ "PS",
      str_detect(position_standardized, "public services|public works") ~ "PSPC",
      str_detect(position_standardized, "shared services") ~ "SSC",
      str_detect(position_standardized, "security intelligence") ~ "CSIS",
      str_detect(position_standardized, "communications security") ~ "CSE",
      str_detect(position_standardized, "revenue") ~ "CRA",
      str_detect(position_standardized, "border") ~ "CBSA",
      str_detect(position_standardized, "food inspection") ~ "CFIA",
      str_detect(position_standardized, "school") ~ "CSPS",
      str_detect(position_standardized, "statistician") ~ "StatCan",
      str_detect(position_standardized, "correction") ~ "CSC",
      TRUE ~ NA_character_
    )) %>%
    mutate(
      position_seniority = case_when(
        str_detect(position_standardized, "associate|assistant|vice-|deputy(?! attorney)") ~ "0 - junior",
        TRUE ~ "1 - normal / other"
      )
    )
}

# assumes a df with column `name_full`
standardize_names <- function(df) {
  df %>%
    mutate(
      name_full = case_when(
        name_full == "Yaprak Baltacioglu" ~ "Yaprak Baltacioğlu",
        name_full == "David Butler-Jones" ~ "David Jones",
        name_full == "Ward P. D. Elcock" ~ "Ward P.D. Elcock",
        name_full == "William Pentney" ~ "William F. Pentney",
        name_full == "Yazmine Cecilia Laroche" ~ "Yazmine Laroche",
        name_full == "Yasmine Laroche" ~ "Yazmine Laroche",
        name_full == "Robert Fadden" ~ "Richard Fadden", # 2013-1353 has... the wrong name (for one of his roles!)
        TRUE ~ name_full
      )
    ) %>%
    mutate(
      name_standardized = str_to_lower(name_full),
      name_standardized = str_replace_all(name_standardized, c(
        "b.a. (bruce) archibald" = "bruce archibald",
        "chistine hogan" = "christine hogan",
        "daniel watson" = "daniel quan-watson",
        "daphne l\\. meredith" = "daphne meredith",
        "donna jane miller" = "donna miller",
        "j. michael horgan" = "michael horgan",
        "janet e\\. king" = "janet king",
        "jonathan t\\. fried" = "jonathan fried",
        "michelle d’auray" = "michelle d'auray",
        "morris a\\. rosenberg" = "morris rosenberg",
        "paul j\\. leblanc" = "paul leblanc",
        "paul michael boothe" = "paul boothe",
        "philippe s. rabot" = "philippe rabot",
        "r\\. tiff macklem" = "tiff macklem",
        "richard b\\. fadden" = "richard fadden",
        "rob stewart" = "robert stewart",
        "wayne g\\. wouters" = "wayne wouters",
        "yaprak baltacio_lu" = "yaprak baltacioğlu"
      ))
    )
}

# assumes a df with various vars (lol)
classify_group_levels_for_salary <- function(df) {
  df %>%
    mutate(fiscal_year_start = fiscal_year_start * 100000) %>% # inflate well beyond the tolerance, so we match years "exactly"
    difference_left_join(
      gic_salary_ranges %>% filter(group == "DM") %>% mutate(fiscal_year_start = fiscal_year_start * 100000),
      max_dist = 3500 # tolerance of +/- $3500 for salary
    ) %>%
    rename(
      fiscal_year_start = fiscal_year_start.x,
      salary_min = salary_min.x,
      salary_max = salary_max.x,
      matched_group_level = group_level,
      matched_group = group,
      matched_level = level,
      matched_salary_min = salary_min.y,
      matched_salary_max = salary_max.y,
      matched_max_performance_award = max_performance_award
    ) %>%
    select(-fiscal_year_start.y) %>%
    mutate(fiscal_year_start = fiscal_year_start / 100000) %>%
    ungroup() %>%
    arrange(name_standardized, start)
}

estimate_end_dates <- function(df) {
  df %>%
    group_by(name_standardized) %>%
    mutate(
      end_is_estimated = is.na(end),
      end = if_else(is.na(end), lead(start) - 1, end)
    ) %>%
    mutate(
      time_in_position = time_length(start %--% end, "years"),
      time_until_next = time_length(end %--% lead(start), "years")
    ) %>%
    ungroup()
}

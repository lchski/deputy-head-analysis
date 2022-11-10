# assumes a df with column `position`
standardize_positions <- function(df) {
  df %>%
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

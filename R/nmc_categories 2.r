## ── NMC Category Lookup ──────────────────────────────────────────────────────
## Canonical mapping of notifiable conditions → NMC regulation categories 1–4.
## Source: SA National Notifiable Medical Conditions regulations.
## Used by province and condition dashboards to group conditions.
## ─────────────────────────────────────────────────────────────────────────────

nmc_category_lookup <- function() {
  category_1 <- c(
    "Acute flaccid paralysis",
    "Acute rheumatic fever",
    "Anthrax",
    "Botulism",
    "Cholera",
    "Covid-19",
    "Congenital rubella syndrome",
    "Crimean-congo viral haemorrhagic fever (human)",
    "Diphtheria",
    "Ebola virus (vhf)",
    "Enteric fever (typhoid or paratyphoid fever)",
    "Food borne illness outbreak",
    "Haemolytic uraemic syndrome (HUS)",
    "Listeriosis",
    "Malaria",
    "Marburg virus (vhf)",
    "Measles",
    "Meningococcal disease",
    "Mpox",
    "Pertussis",
    "Plague",
    "Poliomyelitis",
    "Rabies",
    "Respiratory disease caused by a novel respiratory pathogen",
    "Rift valley fever (human)",
    "Rubella",
    "Smallpox",
    "Waterborne illness outbreak - undefined",
    "Yellow fever"
  )

  category_2 <- c(
    "Agricultural or stock remedy poisoning",
    "Bilharzia (schistosomiasis)",
    "Brucellosis",
    "Congenital syphilis",
    "Haemophilus influenzae type B",
    "Hepatitis A",
    "Hepatitis B",
    "Hepatitis C",
    "Hepatitis E",
    "Lead poisoning",
    "Legionellosis",
    "Leprosy",
    "Maternal death (pregnancy, childbirth and puerperium)",
    "Mercury poisoning",
    "Soil transmitted helminths",
    "Tetanus",
    "Tuberculosis: extensively drug -resistant (XDR -TB)",
    "Tuberculosis: multidrug- resistant (MDR -TB)",
    "Tuberculosis:extra-pulmonary",
    "Tuberculosis:pulmonary"
  )

  category_3 <- c(
    "Endemic arboviral diseases chikungunya virus",
    "Endemic arboviral diseases sindbis virus",
    "Endemic arboviral diseases west nile virus",
    "Non-endemic arboviral diseases: zika virus",
    "Non-endemic arboviral diseases : dengue fever virus",
    "Non-typhoidal salmonellosis",
    "Shiga toxin-producing escherichia coli",
    "Shigellosis",
    "Gonorrhoea ceftriaxone-resistant neisseria gonorrhoea"
  )

  category_4 <- c(
    "Carbapenemase-producing enterobacteriaceae",
    "Vancomycin-resistant enterococci",
    "Staphylococcus aureus: hgisa and gisa",
    "Colistin-resistant pseudomonas aeruginosa",
    "Colistin-resistant acinetobacter baumanii",
    "Clostridium difficile"
  )

  data.frame(
    condition_lookup = c(category_1, category_2, category_3, category_4),
    nmc_category     = c(
      rep(1L, length(category_1)),
      rep(2L, length(category_2)),
      rep(3L, length(category_3)),
      rep(4L, length(category_4))
    ),
    stringsAsFactors = FALSE
  )
}

## Fuzzy-join helper: match data conditions to the lookup using case-insensitive
## prefix matching (to handle slight spelling variations in the raw data).
assign_nmc_category <- function(dt) {
  lookup <- nmc_category_lookup()
  lookup$condition_lower <- tolower(lookup$condition_lookup)
  dt[, condition_lower := tolower(condition)]
  # Exact match first
  dt <- merge(dt, lookup[, c("condition_lower", "nmc_category")],
              by = "condition_lower", all.x = TRUE)
  # For unmatched: try prefix matching (first 15 chars)
  unmatched <- dt[is.na(nmc_category)]
  if (nrow(unmatched) > 0) {
    unmatched[, prefix15 := substr(condition_lower, 1, 15)]
    lookup$prefix15 <- substr(lookup$condition_lower, 1, 15)
    prefix_match <- merge(
      unmatched[, .(condition_lower, prefix15)],
      lookup[, .(prefix15, nmc_category_prefix = nmc_category)],
      by = "prefix15", all.x = TRUE
    )
    prefix_match <- unique(prefix_match, by = "condition_lower")
    dt <- merge(dt, prefix_match[, .(condition_lower, nmc_category_prefix)],
                by = "condition_lower", all.x = TRUE)
    dt[is.na(nmc_category) & !is.na(nmc_category_prefix),
       nmc_category := nmc_category_prefix]
    dt[, nmc_category_prefix := NULL]
    dt[, prefix15 := NULL]
  }
  # Fallback: assign unmatched to category 0 (unknown)
  dt[is.na(nmc_category), nmc_category := 0L]
  dt[, condition_lower := NULL]
  dt[]
}

## Category label helper
nmc_category_label <- function(cat_num) {
  labels <- c(
    "0" = "Uncategorised",
    "1" = "Category 1 — Immediate Notification",
    "2" = "Category 2 — Routine Notification",
    "3" = "Category 3 — Sentinel Surveillance",
    "4" = "Category 4 — AMR Organisms"
  )
  labels[as.character(cat_num)]
}

nmc_category_short <- function(cat_num) {
  labels <- c(
    "0" = "Uncategorised",
    "1" = "Cat 1 — Immediate",
    "2" = "Cat 2 — Routine",
    "3" = "Cat 3 — Sentinel",
    "4" = "Cat 4 — AMR"
  )
  labels[as.character(cat_num)]
}

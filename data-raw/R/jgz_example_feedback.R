
# ==============================================================================
# Maak jgz_example_feedback uit jgz_oefendata
# ==============================================================================

library(dplyr)
library(tidyr)
# ------------------------------------------------------------------------------
# 1. Oefendata inladen
# ------------------------------------------------------------------------------

# Pas dit eventueel aan als jgz_oefendata op een andere manier is opgeslagen.
data("jgz_background", package = "dscoreddi")
data("jgz_vwo", package = "dscoreddi")

# ------------------------------------------------------------------------------
# 2. Achtergronddata en Van Wiechen-data koppelen
# ------------------------------------------------------------------------------

jgz_vwo <- jgz_vwo |>
  mutate(
    jaar = as.numeric(format(Datum, "%Y"))
  )

vwd_raw <- jgz_vwo |>
  left_join(
    jgz_background,
    by = c("ID", "jaar")
  ) |>
  mutate(
   leeftijd = as.numeric(Datum - Geboortedatumkind),
    age = leeftijd / 365.25,
    agemos = age * 12
  ) |> drop_na(age)



# ------------------------------------------------------------------------------
# 3. Van Wiechen-responses hercoderen
# ------------------------------------------------------------------------------

vwd <- vwd_raw |>
  mutate(
    across(
      starts_with("vW."),
      ~ case_when(
        .x == "-" ~ 0,
        .x == "+" ~ 1,
        .x == "M" ~ 1,
        TRUE ~ NA_real_
      )
    )
  )


# ------------------------------------------------------------------------------
# 4. Links- en rechtsitems combineren
# ------------------------------------------------------------------------------

vwd <- vwd |>
  mutate(
    vW.2 = ifelse(vW.2L == 1 & vW.2R == 1, 1, 0),
    vW.3 = ifelse(vW.3L == 1 & vW.3R == 1, 1, 0),
    vW.6 = ifelse(vW.6L == 1 & vW.6R == 1, 1, 0),
    vW.9 = ifelse(vW.9L == 1 & vW.9R == 1, 1, 0),
    vW.10 = ifelse(vW.10L == 1 & vW.10R == 1, 1, 0),
    vW.11 = ifelse(vW.11L == 1 & vW.11R == 1, 1, 0),
    vW.13 = ifelse(vW.13L == 1 & vW.13R == 1, 1, 0),
    vW.15 = ifelse(vW.15L == 1 & vW.15R == 1, 1, 0),
    vW.52 = ifelse(vW.52L == 1 & vW.52R == 1, 1, 0),
    vW.53 = ifelse(vW.53L == 1 & vW.53R == 1, 1, 0),
    vW.59 = ifelse(vW.59L == 1 & vW.59R == 1, 1, 0),
    vW.71 = ifelse(vW.71L == 1 & vW.71R == 1, 1, 0),
    vW.75 = ifelse(vW.75L == 1 & vW.75R == 1, 1, 0)
  )


# ------------------------------------------------------------------------------
# 5. Samengevoegde BDS-items verdelen op basis van leeftijd
# ------------------------------------------------------------------------------

vwd <- vwd |>
  mutate(
    # vW.55 is de a-variant
    vW.55b = vW.55,
    vW.55c = vW.55,
    vW.55d = vW.55,

    vW.68a = vW.68,
    vW.68b = vW.68,
    vW.68c = vW.68
  ) |>
  mutate(
    vW.55b = ifelse(
      age < 0.5 / 365.25 | age > 42.5 / 365.25,
      NA,
      vW.55b
    ),
    vW.55c = ifelse(
      age < 42.5 / 365.25 | age > 102.5 / 365.25,
      NA,
      vW.55c
    ),
    vW.55d = ifelse(
      age < 102.5 / 365.25 | age > 146.5 / 365.25,
      NA,
      vW.55d
    ),
    vW.55 = ifelse(
      age < 146.5 / 365.25 | age > 1,
      NA,
      vW.55
    ),
    vW.68a = ifelse(
      age < 0.75 | age >= 1.75,
      NA,
      vW.68a
    ),
    vW.68b = ifelse(
      age < 1.75 | age >= 2.50,
      NA,
      vW.68b
    ),
    vW.68c = ifelse(
      age < 2.50 | age >= 3.50,
      NA,
      vW.68c
    )
  ) |>
  select(-vW.68)


# ------------------------------------------------------------------------------
# 6. Itemnamen omzetten naar ddi-format
# ------------------------------------------------------------------------------

vwo_columns <- names(vwd)[
  grepl("^vW\\.[1-9]", names(vwd))
]

vwo_names <- sub(
  pattern = "^vW\\.",
  replacement = "v",
  x = vwo_columns
)

ddi_names <- dscoreddi::rename_vwo_gsed(vwo_names)

names(vwd)[
  match(vwo_columns, names(vwd))
] <- ddi_names


# ------------------------------------------------------------------------------
# 7. Data voor Shiny selecteren
# ------------------------------------------------------------------------------
# DDI-itemkolommen bepalen
ddi_items <- names(vwd)[grepl("^ddi", names(vwd))]

# Contactmomenten met minimaal één bekende ddi-score
valid_contacts <- vwd |>
  mutate(
    n_items_available = rowSums(
      !is.na(across(all_of(ddi_items)))
    )
  ) |>
  filter(n_items_available > 0)

# Kinderen met minimaal 2 bruikbare contactmomenten
eligible_ids <- valid_contacts |>
  distinct(ID, ContactmomentID) |>
  count(ID, name = "n_contactmomenten") |>
  filter(n_contactmomenten >= 2) |>
  pull(ID)

# Voorbeelddata voor feedback-app
jgz_example_feedback <- valid_contacts |>
  filter(ID %in% eligible_ids) |>
  select(
    ID,
    ContactmomentID,
    age,
    agemos,
    all_of(ddi_items)
  ) |>
  arrange(ID, age)

# Bepaal per kind het aantal unieke contactmomenten.
eligible_ids <- vwd |>
  distinct(ID, ContactmomentID) |>
  count(ID, name = "n_contactmomenten") |>
  filter(n_contactmomenten >= 2) |>
  pull(ID)

jgz_example_feedback <- vwd |>
  filter(ID %in% eligible_ids) |>
  select(
    ID,
    ContactmomentID,
    age,
    agemos,
    starts_with("ddi")
  ) |>
  arrange(ID, age)


# ------------------------------------------------------------------------------
# 8. Controle
# ------------------------------------------------------------------------------

# Aantal kinderen
dplyr::n_distinct(jgz_example_feedback$ID)

# Aantal contactmomenten per kind
jgz_example_feedback |>
  distinct(ID, ContactmomentID) |>
  count(ID, name = "n_contactmomenten") |>
  arrange(desc(n_contactmomenten))

# Controleer de ddi-itemnamen
names(jgz_example_feedback)[
  startsWith(names(jgz_example_feedback), "ddi")
]


# ------------------------------------------------------------------------------
# 9. Opslaan als packagedataset
# ------------------------------------------------------------------------------

usethis::use_data(
  jgz_example_feedback,
  overwrite = TRUE,
  compress = "xz"
)




#' GetOriginGroupingPreset
#'
#' Get mapping from RegionOfOrigin to GroupOfOrigin
#'
#' @param preset Grouping preset type. Default = 'REPCOUNTRY + UNK + OTHER'
#' @param distr Distribution of RegionOfOrigin. Default = NULL
#'
#' @return NULL
#'
#' @examples
#' distr <- data.table::data.table(
#'   FullRegionOfOrigin = c('REPCOUNTRY', 'SUBAFR'),
#'   Count = c(1536, 2237)
#' )
#' GetOriginGroupingPreset('REPCOUNTRY + UNK + 3 most prevalent regions + OTHER', distr)
#'
#' @export
GetOriginGroupingPreset <- function(
  preset = 'REPCOUNTRY + UNK + OTHER',
  distr = NULL
) {
  generalPresets <- c(
    'REPCOUNTRY + UNK + OTHER',
    'REPCOUNTRY + UNK + SUB-SAHARAN AFRICA + OTHER',
    'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER',
    'Custom'
  )

  migrantPresets <- c(
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + OTHER',
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + OTHER',
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER',
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER',
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER', # nolint
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER', # nolint
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER', # nolint
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' # nolint
  )

  stopifnot(preset %in% union(generalPresets, migrantPresets))

  # Initialize mapping
  map <- c(
    'REPCOUNTRY', 'UNK', 'EASTEUR', 'CENTEUR', 'WESTEUR', 'NORTHAM', 'EUROPE', 'SUBAFR',
    'NORTHAFRMIDEAST', 'SOUTHASIA', 'EASTASIAPAC', 'CAR', 'LATAM', 'AUSTNZ', 'ABROAD'
  )
  names(map) <- map

  # Adjust according to preset
  switch(preset,
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + OTHER' = ,
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + OTHER' = ,
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = ,
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = ,
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = , # nolint
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = , # nolint
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = , # nolint
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = { # nolint
      map[map %chin% c('EASTEUR')] <- 'EASTERN EUROPE'
      map[map %chin% c('CENTEUR', 'WESTEUR', 'EUROPE', 'NORTHAM')] <- 'EUROPE-OTHER-NORTH AMERICA'
      map[map %chin% c('SUBAFR')] <- 'SUB-SAHARAN AFRICA'
      map[map %chin% c('NORTHAFRMIDEAST')] <- 'AFRICA-OTHER'
      map[map %chin% c('CAR', 'LATAM')] <- 'CARIBBEAN-LATIN AMERICA'
      map[map %chin% c('SOUTHASIA', 'EASTASIAPAC')] <- 'ASIA'
      map[
        !(map %chin% c(
          'EASTERN EUROPE', 'EUROPE-OTHER-NORTH AMERICA', 'SUB-SAHARAN AFRICA', 'AFRICA-OTHER',
          'ASIA', 'CARIBBEAN-LATIN AMERICA', 'UNK', 'REPCOUNTRY'
        ))
      ] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + OTHER' = ,
    'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER' = {
      map[
        map %chin% c(
          'ABROAD', 'SUBAFR', 'WESTEUR', 'CENTEUR', 'EASTEUR', 'EASTASIAPAC', 'EUROPE', 'AUSTNZ',
          'SOUTHASIA', 'NORTHAFRMIDEAST', 'NORTHAM', 'CAR', 'LATAM'
        )
      ] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + SUB-SAHARAN AFRICA + OTHER' = {
      map[map %chin% c('SUBAFR')] <- 'SUB-SAHARAN AFRICA'
      map[
        map %chin% c(
          'ABROAD', 'WESTEUR', 'CENTEUR', 'EASTEUR', 'EASTASIAPAC', 'EUROPE', 'AUSTNZ', 'SOUTHASIA',
          'NORTHAFRMIDEAST', 'NORTHAM', 'CAR', 'LATAM'
        )
      ] <- 'OTHER'
    }
  )

  # Second pass for migrant-compatible presets
  switch(preset,
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + OTHER' = {
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER-NORTH AMERICA')] <- 'EUROPE-NORTH AMERICA'
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + OTHER' = {
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = {
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER-NORTH AMERICA')] <- 'EUROPE-NORTH AMERICA'
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = {
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER-NORTH AMERICA')] <- 'EUROPE-NORTH AMERICA'
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
    },
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = { # nolint
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER-NORTH AMERICA + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = { # nolint
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
    },
    'REPCOUNTRY + UNK + EUROPE-NORTH AMERICA + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = { # nolint
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER-NORTH AMERICA')] <- 'EUROPE-NORTH AMERICA'
    }
  )

  map <- as.data.table(map, keep.rownames = TRUE)
  setnames(map, new = c('FullRegionOfOrigin', 'GroupedRegionOfOrigin'))

  if (preset == 'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER' && !is.null(distr)) {
    sepRegions <- head(
      distr[!FullRegionOfOrigin %chin% c('REPCOUNTRY', 'UNK'), FullRegionOfOrigin],
      3
    )
    map[FullRegionOfOrigin %chin% sepRegions, GroupedRegionOfOrigin := FullRegionOfOrigin]
  }

  # Add migrant mapping
  if (preset %in% migrantPresets) {
    map[, MigrantRegionOfOrigin := 'UNK']
    map[GroupedRegionOfOrigin %chin% c('EASTERN EUROPE', 'EUROPE-OTHER-NORTH AMERICA', 'EUROPE-NORTH AMERICA'), MigrantRegionOfOrigin := 'EUROPE-NORTH AMERICA'] # nolint
    map[GroupedRegionOfOrigin %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER', 'AFRICA'), MigrantRegionOfOrigin := 'AFRICA'] # nolint
    map[GroupedRegionOfOrigin %chin% c('ASIA'), MigrantRegionOfOrigin := 'ASIA']
    map[GroupedRegionOfOrigin %chin% c('CARIBBEAN-LATIN AMERICA'), MigrantRegionOfOrigin := 'CARIBBEAN-LATIN AMERICA'] # nolint
    map[GroupedRegionOfOrigin %chin% c('OTHER'), MigrantRegionOfOrigin := 'OTHER']
    map[GroupedRegionOfOrigin %chin% c('REPCOUNTRY'), MigrantRegionOfOrigin := 'REPCOUNTRY']
  } else {
    map[, MigrantRegionOfOrigin := NA_character_]
  }

  mapList <- ConvertOriginGroupingDtToList(map)

  return(mapList)
}

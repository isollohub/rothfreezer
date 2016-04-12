#' @title Parse filemaker strains DB
#'
#' @description Parse the filemaker strains DB from an exported CSV.
#'
#' @param file Path to filemaker CSV.
#' @param write Should the resulting tables be written. Defaults to \code{FALSE}.
#'
#'
#' @importFrom readr read_csv write_csv cols col_character
#' @export

parse_filemaker_export <- function(file , write = TRUE, blame = TRUE) {

  strains <-
    read_csv(
      file,
      col_names = c('set', 'number', 'genotype', 'notes', 'parent', 'made_by', 
                    'dna_made', 'shelf', 'rack', 'pullout', 'insert', 'row', 'column',
                    'date', 'origin', 'original_number', 'plasmid', 'extension',
                    'location', 'strain_name', 'dash', 'reference', 'strain_id',
                    'parent1', 'parent2', 'parents'),
      col_types = cols(.default = col_character()),
      na = c('', 'NA', ' ', '"', '""', '"""', '""""')
    ) %>%
    mutate(entry = 1:n())
  
  # Enforce Eric's Dogma
  cleanish <-
    strains %>%
    mutate(
      row     = suppressWarnings(ifelse(is.na(as.integer(row)),    match(toupper(row), LETTERS), row)),
      column  = suppressWarnings(ifelse(is.na(as.integer(column)), match(toupper(column),LETTERS), column)),
      row     = suppressWarnings(as.integer(row)),
      column  = suppressWarnings(as.integer(column)),
      shelf   = suppressWarnings(as.integer(shelf)),
      rack    = suppressWarnings(as.integer(rack)),
      pullout = suppressWarnings(as.integer(pullout)),
      insert  = suppressWarnings(as.integer(insert))
    ) %>%
    filter(
      !is.na(number),
      !is.na(shelf),
      !is.na(rack),
      !is.na(pullout),
      !is.na(insert),
      !is.na(row),
      !is.na(column),
      !grepl('\\s', set),
      !grepl('\\s', extension),
      !grepl('\\s', number)
    ) %>%
    mutate(
      extension = ifelse(is.na(extension), '', extension),
      set = toupper(set),
      set = gsub('^P', 'p', set),
      strain_id = paste0(set, number, '-', extension),
      strain_id = gsub('-{1,}$', '', strain_id),
      location = paste0(shelf, rack, pullout, insert, row, column)
    ) %>%
    mutate_each(funs(gsub('\t|\v', ' ', .)), genotype, notes) %>%         # replace tabs with space
    mutate_each(funs(gsub('^\\s{1,}|\\s{1,}$', '', .)), genotype, notes) %>%  # Remove \v (?!) and leading/trailing spaces
    filter(!(set %in% c('', '?', NA)))

  # Construct sub-tables
  notsoclean <- 
    strains %>%
    filter(!(entry %in% cleanish$entry)) %>%
    select(-entry)
  
  duplicated <-
    cleanish %>%
    filter(duplicated(location) | duplicated(location, fromLast = T))

  clean <-
    cleanish %>%
    filter(!(entry %in% duplicated$entry)) %>%
    select(-entry)
  
  duplicated <- select(duplicated , -entry)

  addresses <- select(clean, strain_id, shelf, rack, pullout, insert, row, column)
  
  clean <- clean %>%
    select(-(shelf:column)) %>%
    select(strain_id, everything())
  
  # Write to file
  if (write) {
    dir  <- dirname(file)
    base <- gsub('\\.csv$|\\.CSV$', basename(file))
    
    write_csv(addresses,  paste0(dir, "/" , "addresses.csv"))
    write_csv(notsoclean, paste0(dir, "/" , base, "-dirty.csv"))
    write_csv(clean,      paste0(dir, "/" , base, "-cleaned.csv"))
    write_csv(duplicated, paste0(dir, "/" , base, "-duplicates.csv"))
  
    message("Results written to ", dir)
  }

  stock <- list(clean = clean, dirty = notsoclean, duplicated = duplicated)

  return(stock)

}

#' Update gene annotation table
#' 
#' Update \emph{S. cerevisiae} gene annotations from 
#' \href{http://www.yeastgenome.org}{SGD}.
#' 
#' @param to Where to write the CSV
#' 
#' @importFrom readr read_tsv write_csv
#' @export

update_genes <- function(to) {
  'http://downloads.yeastgenome.org/curation/chromosomal_feature/SGD_features.tab' %>%
    read_tsv(col_names = FALSE) %>%
    col_names(
      'gene_id', 'type', 'qualifier', 'orf_id', 'gene_name', 'gene_alias',
      'parent_feature_name', 'secondary_sgd_id', 'chromosome', 'start', 'stop',
      'strand', 'genetic_position', 'coordinate_version', 'sequence_version', 
      'gene_description'
    ) %>%
    mutate_(gene_name = ~ifelse(gene_name == '', orf_id, gene_name)) %>%
    select_(~gene_id, ~orf_id, ~gene_name, ~gene_alias, ~gene_description) %>%
    write_csv(to)
}

col_names <- function(df, ...) { names(df) <- c(...); return(df) }

#' Update genetic distances
#' 
#' Update \emph{S.cerevisiae} genetic distances from 
#' \href{http://www.yeastgenome.org}{SGD}
#' 
#' @param to Where to write the CSV
#' 
#' @importFrom readr read_tsv write_csv
#' @export

update_genetic_distances <- function() {
  
  # Get location information for all ORFs on SGD
  sgd <- 
    'http://downloads.yeastgenome.org/curation/chromosomal_feature/SGD_features.tab' %>%
    read_tsv(
      col_types = 'cc_cc___ciic____',
      col_names = c(
        'gene_id', 'type', 'orf_id', 'gene_name', 'chromosome', 'start', 'stop',
        'strand')
    ) %>%
    filter_(~type %in% c('ORF', 'centromere')) %>%
    mutate_(
      gene_name = ~ifelse(is.na(gene_name), orf_id, gene_name),
      max = ~pmax(start, stop),
      min = ~pmin(start, stop)
    )
  
  # Assign each ORF to a chromosome arm
  with_chromosome_arm <-
    left_join(
      sgd %>% filter_(~type != 'centromere'),
      sgd %>% filter_(~type == 'centromere') %>% select_(~chromosome, cen_min = ~min, cen_max = ~max)
    ) %>%
    mutate_(arm = ~ifelse(max < cen_min, 'left', 'right'))
  
  # Pair each ORF to all ORFs on the same chromosome arm and compute the distance
  gene_pairs <-
    full_join(
      select_(with_chromosome_arm, ~chromosome, ~arm, gene_id_a = ~gene_id, orf_id_a = ~orf_id, gene_name_a = ~gene_name, a_max = ~max, a_min = ~min),
      select_(with_chromosome_arm, ~chromosome, ~arm, gene_id_b = ~gene_id, orf_id_b = ~orf_id, gene_name_b = ~gene_name, b_max = ~max, b_min = ~min)
    ) %>%
    mutate_(
      end_to_start = ~a_max - b_min,
      start_to_end = ~a_min - b_max,
      distance = ~pmin(abs(end_to_start), abs(start_to_end)),
      distance = ~ifelse(sign(end_to_start) != sign(start_to_end), 0, distance)
    ) %>%
    select_(~chromosome, ~arm, ~gene_id_a, ~gene_id_b, ~orf_id_a, ~orf_id_b, ~gene_name_a, ~gene_name_b, ~distance)
  
  # Write to file
  by_chromosome <- split(gene_pairs, paste(gene_pairs$chromosome, gene_pairs$arm))
  lapply(by_chromosome, function(x) { 
    write_csv(x, paste0('genetic-distances/', x$chromosome[1], '-', x$arm[1], '.csv'))
  })
}

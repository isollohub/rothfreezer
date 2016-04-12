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

#' Update gene annotation table
#' 
#' Update \emph{S. cerevisiae} gene annotations from 
#' \href{http://www.yeastgenome.org}{SGD}.
#' 
#' @param to Where to write the CSV
#' 
#' @importFrom data.table fread
#' @export

update_genes <- function(to) {
  'http://downloads.yeastgenome.org/curation/chromosomal_feature/SGD_features.tab' %>%
    fread(data.table = FALSE) %>%
    select_(
      gene_id = ~V1,
      orf_id  = ~V4,
      gene_name  = ~V5,
      gene_alias = ~V6,
      gene_description = ~V16
    ) %>%
    write.csv(to, row.names = FALSE)
}

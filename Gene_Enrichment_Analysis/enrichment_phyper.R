enrichment_phyper <- function(NDD, List, background){
  
  q <- length(intersect(List$ensembl, NDD$gene)) -1
  m <- nrow(NDD)
  n <- length(setdiff(background[,1], NDD$gene))
  k <- length(List$ensembl)
  pvalue <- phyper(q,m,n,k, lower.tail = FALSE)
  return(as.vector(c(pvalue, q+1, m)))
}
  
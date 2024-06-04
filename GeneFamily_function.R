# GeneFamily function -----------------------------------------------------

fam_enrich_out <- read.delim("fam_enrich_out.txt", header = TRUE, sep = "\t", dec = ".")


GeneFamily_analysis <- 
  function(GF=NULL){
    l <- length(GF)
    allGF <- c()
    genes <- c()
    for (i in 1:l) {
      allGF <- c(allGF, fam_enrich_out[grep(GF[i], fam_enrich_out$Gene.Family), "Short.Name"])
      allGF <- c(allGF, fam_enrich_out[grep(GF[i], fam_enrich_out$Short.Name), "Short.Name"])
      genes <- c(genes, fam_enrich_out[grep(GF[i], fam_enrich_out$Gene.Family), "Gene.IDs"])
      genes <- c(genes, fam_enrich_out[grep(GF[i], fam_enrich_out$Short.Name), "Gene.IDs"])
    }
    allGF <- unique(allGF)
    genes <- unique(genes)
    if (length(allGF)>0) {
      genes=substr(genes,start=2,stop=(as.integer(nchar(genes))))
      genes=strsplit(genes, ' ',)
      genes=unlist(genes)
      for (i in 1:length(genes)) {
        genes[i] <- substr(genes[i],start=2,stop=(as.integer(nchar(genes[i])-2)))
      }
      length(genes)
      for (i in 1:length(genes)) {
        l <- c(l, grep(genes[i], DEG_data$ID))
      }
      l <- unique(l)
      GeneFamily <- DEG_data[l, ]
      l <- nrow(GeneFamily)
      go_number <- c()
      for (i in 1:l) {
        go_number <- c(go_number, grep(GeneFamily$ID[i],BiNGO_output$Genes))
      }
      go_number <- unique(go_number)
      go_names <- BiNGO_output$GO_Description[go_number]
      if (length(go_number)!=0) {
        GeneFamily_2 <- data.frame()
        GeneFamily_2 <- data.frame(matrix(0,ncol=(length(go_number)), nrow=nrow(GeneFamily)))
        colnames(GeneFamily_2) <- go_names
        for (i in 1:length(go_number)) {
          GeneFamily_2[BiNGO_output$GO_Description[go_number[i]]] <- NA
          for (k in 1:nrow(GeneFamily)) {
            if (length(grep(GeneFamily$ID[k],BiNGO_output$Genes[go_number[i]]) * 1) == 1) {
              GeneFamily_2[k,BiNGO_output$GO_Description[go_number[i]]] <- "OK"
            } else {
              GeneFamily_2[k,BiNGO_output$GO_Description[go_number[i]]] <- ""
            }
          }
        }
      }
      GeneFamily <- cbind(GeneFamily, GeneFamily_2)
      View(GeneFamily)
    } else {
      print("Gene Family not found")
    } 
  }

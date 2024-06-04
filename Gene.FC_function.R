#  Gene.FC function -------------------------------------------------------

Gene_analysis <- 
  function(Gene_ID="ARR") {
    l <- c()
    for (i in 1:length(Gene_ID)) {
      l <- c(l, grep(Gene_ID[i], DEG_data$ID))
      l <- c(l, grep(Gene_ID[i], DEG_data$external_gene_name))
      l <- c(l, grep(Gene_ID[i], DEG_data$tair_symbol))
    }
    l <- unique(l)
    l
    if (length(l)==0) {
      print("Gene(s) not found")
    } else {
      Gene <- data.frame()
      Gene <- DEG_data[l, ]
      if (nrow(Gene)>1) {
        l <- nrow(Gene)
        go_number <- c()
        for (i in 1:l) {
          go_number <- c(go_number, grep(Gene$ID[i],BiNGO_output$Genes))
        }
        go_number <- unique(go_number)
      } else {
        go_number <- grep(Gene$ID,BiNGO_output$Genes)
      }
      if (length(go_number)!=0) {
        for (i in 1:length(go_number)) {
          Gene[BiNGO_output$GO_Description[go_number[i]]] <- NA
          for (k in 1:nrow(Gene)) {
            if (length(grep(Gene$ID[k],BiNGO_output$Genes[go_number[i]]) * 1) == 1) {
              Gene[k,BiNGO_output$GO_Description[go_number[i]]] <- "OK"
            } else {
              Gene[k,BiNGO_output$GO_Description[go_number[i]]] <- ""
            }
          }
        }
      }
      View(Gene)
    }
  }

# GO.FC function ----------------------------------------------------------

BiNGO_output <- read.csv("RNAseq_data.csv", 
                         +     sheet = "Hoja1")
colnames(BiNGO_output) <- c("GO_ID","GO_Description","p-val","Corrected_p-val","Cluster_frequency","Total_frequency","Genes")

BiNGO_output <- as.data.frame(BiNGO_output)

GO_analysis <- 
  function(BP=""){
    if (length(BP)>1) {
      l <- length(BP)
      allbp <- c()
      genes <- c()
      for (i in 1:l) {
        allbp <- c(allbp, BiNGO_output[grep(BP[i], BiNGO_output$GO_Description), "GO_Description"])
        genes <- c(genes, BiNGO_output[grep(BP[i], BiNGO_output$GO_Description), "Genes"])
      }
      allbp <- unique(allbp)
      genes <- unique(genes)
    } else {
      allbp <- BiNGO_output[grep(BP, BiNGO_output$GO_Description), "GO_Description"]
      genes <- BiNGO_output[grep(BP, BiNGO_output$GO_Description), "Genes"]
    }
    if (length(allbp)>0) {
      genes <- unique(unlist(strsplit(genes, " ")))
      l <- c()
      for (i in 1:length(genes)) {
        l <- c(l, grep(genes[i], RNAseq_data$ID))
      }
      l <- unique(l)
      l
      GO <- RNAseq_data[l, ]
      GO_2 <- data.frame(matrix(0,ncol=(length(unlist(allbp))), nrow=length(l)))
      colnames(GO_2) <- unlist(allbp)
      GO <- cbind(GO, GO_2)
      for (k in 1:length(allbp)) {
        a <- BiNGO_output[grep(allbp[k], BiNGO_output$GO_Description), c("GO_Description","Genes")]
        for (i in 1:length(unique(unlist(strsplit(genes, " "))))) {
          b <- (grepl(GO[i,"ID"], a$Genes)   == 1)*1
          if (
            b == 1
          )
          {
            GO[i, k+6] <- "OK"
          } else {
            GO[i, k+6] <- " " 
          }
        }
      }
      View(GO)
    } else {
      print("Bioprocess(es) not found")
    } 
  }


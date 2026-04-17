library(clusterProfiler)
library(DOSE)
library(DO.db)
library(EnsDb.Hsapiens.v86)

get_DO_data <- function(ont="DO") {
  ont="DO"
  if (ont == "DO"){
    tryCatch(utils::data(list="DO2ALLEG", package="DOSE"))
    assign("DO2ALLEG", DO2ALLEG )
    DO2ALLEG <- get("DO2ALLEG")
    
    tryCatch(utils::data(list="EG2ALLDO", package="DOSE"))
    assign("EG2ALLDO", EG2ALLDO )
    EG2ALLDO <- get("EG2ALLDO")
    
    PATHID2EXTID <- get("DO2ALLEG" )
    EXTID2PATHID <- get("EG2ALLDO" )
    
    PATH2NAME.df <- toTable(DOTERM)
    PATH2NAME.df <- PATH2NAME.df[, c("do_id", "Term")]
    PATH2NAME.df <- unique(PATH2NAME.df)
    PATH2NAME <- PATH2NAME.df[,2]
    names(PATH2NAME) <- PATH2NAME.df[,1]
    
  }else if (ont == "DOLite"){
    
    tryCatch(utils::data(list="DOLite2EG", package="DOSE"))
    assign("DOLite2EG", DOLite2EG ) 
    DOLite2EG <- get("DOLite2EG")
    
    tryCatch(utils::data(list="EG2DOLite", package="DOSE"))
    assign("EG2DOLite", EG2DOLite )
    EG2DOLite <- get("EG2DOLite")
    
    tryCatch(utils::data(list="DOLiteTerm", package="DOSE"))
    assign("DOLiteTerm", DOLiteTerm )
    DOLiteTerm <- get("DOLiteTerm")
    
    PATHID2EXTID <- get("DOLite2EG")
    EXTID2PATHID <- get("EG2DOLite")
    PATH2NAME <- get("DOLiteTerm")
  }
  
  return( list(PATHID2EXTID = PATHID2EXTID , 
               PATH2NAME  = PATH2NAME 
  ) )
}

get_DO_pathway <- function(ont = "DO", patterns = c("diabetes", "cardiovascular", "heart", "cardiac", "hypertension")) {
  
  database <- get_DO_data(ont = ont)
  DOpathway <- database$PATHID2EXTID
  pathway_name <- database$PATH2NAME
  
  selected_pathway <- pathway_name[grep(paste0(patterns, collapse = "|"), pathway_name)]
  selected_pathway <- selected_pathway[!selected_pathway %in% c("carcinoma", "tumor", "cancer", "defect")]
  
  DOpathway <- DOpathway[names(DOpathway) %in% names(selected_pathway)]
  selected_pathway <- selected_pathway[match(names(DOpathway), names(selected_pathway))]
  names(DOpathway) <- unname(selected_pathway)
  
  DOpathway <- DOpathway[sapply(DOpathway, length) >= 10]
  
  for (i in seq_along(DOpathway)) {
    this <- DOpathway[i]
    getgene <- ensembldb::select(EnsDb.Hsapiens.v86,
                                 keys = unname(this)[[1]],
                                 keytype = "ENTREZID",
                                 columns = c("SYMBOL", "ENTREZID"))
    DOpathway[[i]] <- unique(getgene$SYMBOL)
  }
  
  return(DOpathway)
}

testCache <- tempdir()
trioA <- Trio$new("figshare:26142922/47361079", cachePath = testCache)
data <- trioA |> purrr::pluck("data")
data_log_normalized <- data %>%
  mutate(across(where(is.numeric), ~ log(. + 1)))
data <- data_log_normalized

calculate_scores <- function(to_eval, gs) {
  colnames_data <- colnames(to_eval)
  scores <- sapply(names(gs), function(pathway) {
    genes <- intersect(gs[[pathway]], colnames_data)
    gene_means <- sapply(genes, function(gene) mean(to_eval[[gene]], na.rm = TRUE))
    sum(gene_means, na.rm = TRUE)
  })
  return(scores)
}

trioA$addMetric("disease score", calculate_scores)
trioA$addAuxData("get_DO_pathway", get_DO_pathway, c("disease score"))
trioA$data <- data
result <- calculate_scores(data, DOpathway)

evaluation <- trioA$evaluate(list(get_DO_pathway = data))
disease_scores <- evaluation$get_DO_pathway$`disease score`


df <- data.frame(
  Disease = names(result),
  Normalized_Score = result
)

top15_df <- df %>%
  arrange(desc(Normalized_Score)) %>%
  slice_head(n = 15)

p1 <- ggplot(top15_df, aes(x = Normalized_Score, y = reorder(Disease, Normalized_Score))) +
  geom_bar(stat = "identity", fill = "#E64525") +
  coord_flip() +
  labs(x = "disease", y = "score") +
  th 

ggsave("genescore.pdf", plot = p1, width = 4, height = 6, dpi = 300)


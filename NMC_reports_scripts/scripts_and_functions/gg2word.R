# display a ggplot in a word document quickly 

gg2word <- function( plot, directory_name, dims = NULL) {
  
  if(is.null(dims)){
  dims<- c(11,6.5)} else{ dims = dims}
  
  dims[1]
  
  plot_name <- deparse(substitute(plot))
  
  ggsave(
    paste0(as.character(directory_name),"/",as.character(plot_name),".png"),
    suppressWarnings(plot + theme(plot.background = element_rect(fill = "white", color = "grey90", linewidth = 0.25))),
    width = dims[1],
    height = dims[2],
    dpi = 200
  )
  
  #Include the plot in the Quarto document
  knitr::include_graphics(paste0(as.character(directory_name),"/",as.character(plot_name),".png"))
  
}

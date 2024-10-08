#' get colours for the OME
#'
#' @param n the number of colours
#' @param type either "complementary" or "constrast"
#'
#' @return a vector of colours
#' @export
#'
get_OME_colours <- function(n, type = 'complementary'){
  OME_colours = c("#10263A","#009BC2", "#37B4B0")
  OME_contrast = c("#009BC2", "#10263A", '#d7386b')
  if(n==1){
    return(OME_colours[2])
  }
  if(n==2){
    if(type == 'contrast'){return(OME_contrast[c(1,3)])}
    return(OME_colours[1:2])
  }
  if(n == 3){
    if(type == 'contrast'){return(OME_contrast)}
    return(OME_colours)
  }

  if(n > 3){
    if(type == 'contrast'){colours = OME_contrast}else{colours = OME_colours}
    rbPal = grDevices::colorRampPalette(colours)
    colours= rbPal(n)
    return(colours)
  }
}

#' get colours for the OME
#'
#' @param n the number of colours.
#' @param type either "complementary",  "contrast" or "distinct".
#'
#' @return a vector of colours.
#' @export
#'
#' @examplesIf FALSE
#' get_OME_colours(3) |> scales::show_col()
#' get_OME_colours(3, type = 'contrast') |> scales::show_col()
#' get_OME_colours(10, type = 'contrast') |> scales::show_col()
#'
#' @details
#' OME complementary colours = c("#10263A","#009BC2", "#37B4B0").
#' OME contrast colours = c("#009BC2", "#10263A", '#d7386b').
#'
#' If more than 3 colours are specified we interpolate the colours using `grDevices::colorRampPalette()`.
#'
get_OME_colours <- function(n, type = 'complementary'){
  # UoN_colours = c('#FFFFFF', #White
  #                 '#FAF6EF', #Portland stone
  #                 '#93D500', #Bramley apple
  #                 '#37B4B0', #Trent turquoise
  #                 '#009BC1', #Malaysia Sky Blue
  #                 '#005F36', #Forest Green
  #                 '#792D85', #Civic Purple
  #                 '#D7336C', #Pioneering Pink
  #                 '#B91C2E', # Jubilee Red
  #                 '#F98109', #Mandarin Orange
  #                 '#DEB406', #Rebel's Gold
  #                 '#10263B' #Nottingham Blue
  # )
  #
  # IntroReport_colour = c('#4eb6af', '#4dbbdc', '#3d9bc2', '#205d81', '#12253c')


  OME_colours = c("#10263B","#009BC1", "#37B4B0")
  OME_contrast = c("#009BC1", "#10263B", '#D7336C')
  OME_distinct = c('#10263B', '#B91C2E', '#009BC1', '#DEB406', '#FAF6EF' )

if(type %in% c('contrast', 'complementary')){
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

  if(type == 'distinct'){
    if(n==1)return(OME_distinct[3])
    if(n==2)return(OME_distinct[c(1:2)])
    if(n==3)return(OME_distinct[c(1:3)])
    if(n==4)return(OME_distinct[c(1:4)])
    if(n==5)return(OME_distinct)
    if(n >5 & n < 10) return(c(OME_distinct, OME_distinct[1:(n-5)] |> colorspace::adjust_transparency(.7)))
    if(n>9) stop('Type = distinct only allows up to 9 colours.' )

  }

}

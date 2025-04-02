#' get colours for the OME
#'
#' @param n the number of colours.
#' @param type either "complementary",  "contrast" or "distinct" or "distinct".
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
#' Base colours for palettes:
#'
#' - OME complementary colours = c("#10263A","#009BC2", "#37B4B0").
#' - OME contrast colours = c("#009BC2", "#10263A", '#d7386b').
#' - OME distinct colours = c("#009BC1",'#DEB406', '#D7336C', '#10263B')
#'
#' If more than the designated colours are specified we interpolate the colours using `grDevices::colorRampPalette()`.
#'
#' For "distinct2", when 4< n <=10 we force the distinct colours to appear in the provided colours. This is done by going to a palette with those colours (n=7 or 10) and working back by removing interpolated colours.
get_OME_colours <- function(n, type = 'contrast'){
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
  OME_distinct = c("#009BC1",'#DEB406', '#D7336C',  '#10263B')

if(type %in% c('contrast2', 'complementary')){
  if(n==1){
    return(OME_colours[2])
  }
  if(n==2){
    if(type == 'contrast2'){return(OME_contrast[c(1,3)])}
    return(OME_colours[1:2])
  }
  if(n == 3){
    if(type == 'contrast2'){return(OME_contrast)}
    return(OME_colours)
  }

  if(n > 3){
    if(type == 'contrast2'){colours = OME_contrast}else{colours = OME_colours}
    rbPal = grDevices::colorRampPalette(colours)
    colours= rbPal(n)
    return(colours)
  }
}

  if(type == 'contrast'){
    if(n==1)return("#009BC1")
    if(n==2)return(c("#009BC1", "#10263B"))
    if(n==3)return(c("#009BC1", "#D7336C", "#10263B"))
    if(n==4)return(c("#009BC1", "#08607E", "#10263B", "#D7336C"))
    if(n==5)return(c("#009BC1", "#08607E", "#10263B", "#732C53", "#D7336C"))
    if(n==6)return(c("#009BC1", "#057494", "#0A4D67", "#10263B", "#522A4B", "#D7336C"))
    if(n==7)return(c("#009BC1", "#057494", "#0A4D67", "#10263B", "#522A4B", "#942E5B", "#D7336C"))
    if(n==8)return(c("#009BC1", "#047D9F", "#08607E", "#0C435C", "#10263B", "#412947", "#732C53", "#D7336C"))
    if(n==9)return(c("#009BC1", "#047D9F", "#08607E", "#0C435C", "#10263B", "#412947", "#732C53", "#A52F5F", "#D7336C"))
    if(n > 9){
      rbPal = grDevices::colorRampPalette(contrast)
      colours= rbPal(n)
      return(colours)
    }
  }
  if(type == 'distinct2'){
    if(n==1)return(OME_distinct[1])
    if(n==2)return(OME_distinct[c(1,4)])
    if(n==3)return(OME_distinct[c(1,3,4)])
    if(n==4)return(OME_distinct[c(1:4)])
    if(n>4){
      rbPal = grDevices::colorRampPalette(OME_distinct)
      colours= rbPal(n)
      return(colours)
    }
  }
  if(type == 'distinct'){
    if(n==1)return("#009BC1")
    if(n==2)return(c("#009BC1", "#10263B"))
    if(n==3)return(c("#009BC1", "#D7336C", "#10263B"))
    if(n==4)return(c("#009BC1", "#DEB406", "#D7336C", "#10263B"))
    if(n==5)return(c("#009BC1", "#A6AD34", "#DEB406", "#D7336C", "#10263B"))
    if(n==6)return(c("#009BC1", "#85AA50", "#DEB406", "#D7336C", "#872D58", "#10263B"))
    if(n==7)return(c("#009BC1", "#6FA763", "#DEB406", "#DA7339", "#D7336C", "#732C53", "#10263B"))
    if(n==8)return(c("#009BC1", "#93AB44", "#DEB406", "#DB8928", "#D95D4A", "#D7336C", "#942E5B", "#10263B"))
    if(n==9)return(c("#009BC1", "#49A382", "#93AB44", "#DEB406", "#DB8928", "#D95D4A", "#D7336C", "#942E5B", "#10263B"))
    if(n==10)return(c("#009BC1", "#49A382", "#93AB44", "#DEB406", "#DB8928", "#D95D4A", "#D7336C", "#942E5B", "#522A4B", "#10263B"))
    if(n > 10){
      rbPal = grDevices::colorRampPalette(OME_distinct)
      colours= rbPal(n)
      return(colours)
    }
  }
}

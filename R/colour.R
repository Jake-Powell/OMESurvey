#' get colours for the OME
#'
#' @param n the number of colours.
#' @param type either "contrast/divergent", "complementary/sequential" or "distinct/qualitative".
#'
#' @return a vector of "n" colours.
#' @export
#'
#' @examplesIf FALSE
#' get_OME_colours(3) |> scales::show_col()
#' get_OME_colours(3, type = 'complementary') |> scales::show_col()
#' get_OME_colours(9, type = 'distinct') |> scales::show_col()
#'
#' @details
#' Base colours for palettes:
#'
#' - OME complementary/sequential colours = "#10263B" (Dark Navy/Nottingham Blue), "#009BC1" (Blue/Malaysia Sky Blue) and "#37B4B0"(Turquoise/Trent Turquoise).
#' - OME contrast/divergent colours = "#009BC1" (Blue/Malaysia Sky Blue), "#10263B" (Dark Navy/Nottingham Blue) and '#D7336C' (Pink/Pioneering Pink).
#' - OME distinct/qualitative colours = "#009BC1"(Blue/Malaysia Sky Blue),'#DEB406' (Gold/Rebel's Gold), '#D7336C' (Pink/Pioneering Pink) and '#10263B' (Dark Navy/Nottingham Blue).
#'
#' For 'contrast' and 'distinct' specific colour orderings are chosen for n <= 9, whereas for 'complementary' specific colours selected for n <= 3. If n is larger than we interpolate the palette using grDevices::colorRampPalette().
get_OME_colours <- function(n, type = 'contrast'){

  OME_colours = c("#10263B","#009BC1", "#37B4B0")
  OME_contrast = c("#009BC1", "#10263B", '#D7336C')
  OME_distinct = c("#009BC1",'#DEB406', '#D7336C',  '#10263B')

  if(type %in% c('complementary','sequential')){
    if(n==1) return(OME_colours[2])
    if(n==2) return(OME_colours[1:2])
    if(n == 3) return(OME_colours)
    if(n > 3){
      rbPal = grDevices::colorRampPalette(OME_colours)
      colours= rbPal(n)
      return(colours)
    }
  }

  if(type %in% c('contrast','divergent')){
    if(n==1)return("#047D9F")
    if(n==2)return(c("#047D9F", "#A52F5F"))
    if(n==3)return(c("#047D9F", "#10263B", "#A52F5F"))
    if(n==4)return(c("#047D9F", "#0C435C", "#412947", "#A52F5F"))
    if(n==5)return(c("#009BC1", "#08607E", "#10263B", "#732C53", "#D7336C"))
    if(n==6)return(c("#009BC1", "#08607E", "#0C435C", "#412947", "#732C53", "#D7336C"))
    if(n==7)return(c("#009BC1", "#08607E", "#0C435C", "#10263B", "#412947", "#732C53", "#D7336C"))
    if(n==8)return(c("#009BC1", "#047D9F", "#08607E", "#0C435C", "#412947", "#732C53", "#A52F5F", "#D7336C"))
    if(n==9)return(c("#009BC1", "#047D9F", "#08607E", "#0C435C", "#10263B", "#412947", "#732C53", "#A52F5F", "#D7336C"))
    if(n > 9){
      rbPal = grDevices::colorRampPalette(OME_contrast)
      colours= rbPal(n)
      return(colours)
    }
  }


  if(type %in% c('distinct','qualitative')){
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

  ##### BELOW IS THE COLOUR GRAVEYARD TO PALETTES THAT FAILED TO CAPTURE THE HEART OF THE TEAM
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
  # if(type %in% c('contrast2', 'complementary')){
  #   if(n==1){
  #     return(OME_colours[2])
  #   }
  #   if(n==2){
  #     if(type == 'contrast2'){return(OME_contrast[c(1,3)])}
  #     return(OME_colours[1:2])
  #   }
  #   if(n == 3){
  #     if(type == 'contrast2'){return(OME_contrast)}
  #     return(OME_colours)
  #   }
  #
  #   if(n > 3){
  #     if(type == 'contrast2'){colours = OME_contrast}else{colours = OME_colours}
  #     rbPal = grDevices::colorRampPalette(colours)
  #     colours= rbPal(n)
  #     return(colours)
  #   }
  # }
  # if(type == 'distinct2'){
  #   if(n==1)return(OME_distinct[1])
  #   if(n==2)return(OME_distinct[c(1,4)])
  #   if(n==3)return(OME_distinct[c(1,3,4)])
  #   if(n==4)return(OME_distinct[c(1:4)])
  #   if(n>4){
  #     rbPal = grDevices::colorRampPalette(OME_distinct)
  #     colours= rbPal(n)
  #     return(colours)
  #   }
  # }

  # if(type == 'contrast'){
  #   if(n==1)return("#009BC1")
  #   if(n==2)return(c("#009BC1", "#10263B"))
  #   if(n==3)return(c("#009BC1", "#10263B", "#D7336C"))
  #   if(n==4)return(c("#009BC1", "#08607E", "#10263B", "#D7336C"))
  #   if(n==5)return(c("#009BC1", "#08607E", "#10263B", "#732C53", "#D7336C"))
  #   if(n==6)return(c("#009BC1", "#057494", "#0A4D67", "#10263B", "#522A4B", "#D7336C"))
  #   if(n==7)return(c("#009BC1", "#057494", "#0A4D67", "#10263B", "#522A4B", "#942E5B", "#D7336C"))
  #   if(n==8)return(c("#009BC1", "#047D9F", "#08607E", "#0C435C", "#10263B", "#412947", "#732C53", "#D7336C"))
  #   if(n==9)return(c("#009BC1", "#047D9F", "#08607E", "#0C435C", "#10263B", "#412947", "#732C53", "#A52F5F", "#D7336C"))
  #   if(n > 9){
  #     rbPal = grDevices::colorRampPalette(contrast)
  #     colours= rbPal(n)
  #     return(colours)
  #   }
  # }
}

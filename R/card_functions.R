#' Read yaml data and use to create card
#' 
#' This is in early dev
#' @param file path to an 'article' qmd file
#' @importFrom yaml read_yaml
#' @export
#' 
create_card_from_qmd <- function(file){
  x <- read_yaml(file) # makes list from yaml for a given article
  create_card(
    color1 = x$color_1,
    headline = x$title,
    img_src = x$image_url,
    alt = x$alt_text
  )
}


#' Create html card using R
#' 
#' This is the underlying function, written by Dax
#' @importFrom htmltools tags
#' @export
create_card <- function(person_page, img_src, headline, alt, name, color1) {
  tags$div(
    class = "card-column",
    tags$button(
      class = "paper-card", 
      href = person_page, 
      style = paste0("background-color:", color1),
      p(class = "headline", headline),
      tags$img(class ="card-image", src = img_src, alt = name),
      p(class = "headline-2", headline)))
}

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
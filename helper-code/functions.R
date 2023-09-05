library(htmltools)
library(glue)


smart_blocks<-function(
    block_texts = c("Block Text 1", "Block Text 2", "Block Text 3"),
    block_icons = rep("fas fa-table",3),
    bg_color = "purple",
    border_color = "white",
    block_height = 60,
    border_size = "0",
    font_color = "white",
    icon_color = "white"){
  
  block_count <- length(block_texts)
  
  blocks<-lapply(1:block_count, function(i) {
    htmltools::tags$div(class = "block",
             style = glue("border-radius:10px;background-color:{bg_color};display:flex;color:{font_color};
                        padding:2%;flex: 1 1 calc(50% - 20px);margin: 1%;border:{border_size}px {border_color} solid;height:{block_height}px;"),
             htmltools::tags$i(class=block_icons[i], style=glue("margin-right:20px;margin-left:10px;color:{icon_color}")),
             htmltools::tags$span(block_texts[i])
    )})
  
  htmltools::tagList(
    htmltools::tags$div(class = "blocks",
             style = "display:flex;flex-wrap:wrap;justify-content:center;",
             blocks)
  )
}

item_titles <- c("Item 1", "Item 2", "Item 3")
item_icons = rep("fas fa-table",3)
item_bullets = list(c("Bullet 1","Bullet 2","Bullet 3"),
                 c("Bullet 4","Bullet 5","Bullet 6"),
                 c("Bullet 7","Bullet 8","Bullet 9"))


smart_columns<-function(
    item_titles,
    item_icons,
    item_bullets,
    icon_color = "black",
    icon_bg_color = "red",
    icon_size = "12",
    border_color = "black",
    title_color = "black",
    title_size = "12",
    border_size = "1",
    border_radius = "1px",
    bullet_left_margin = "10px"){
  
  items<- length(item_titles)
  item_width <- round((100/items)-(1.1*items),2)
  
tags$div(class="smart-cols", style="display:flex;",
  lapply(1:3, function(i){tags$div(class= "smart-col",
           style=glue('text-align: center;
      border: {border_size}px {border_color} solid;
      width:{item_width}%;
      border-radius:10px;
      margin-right:1%;
      margin-left:1%'),
      tags$div(class="icon-container",
               style = glue('background-color: {icon_bg_color};
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border-radius: 50%;
        padding: 5%;
        margin-top:5%;
        margin-bottom:5%;'),
               tags$i(class=item_icons[i],
                      style = glue('color:{icon_color};font-size:{icon_size}px'))
               ),
      tags$div(class="bullet-title",
               style = glue('font-weight:bold;color:{title_color}'),
               item_titles[i]),
      tags$div(class = "bullet-container",
               style=glue('text-align:left;margin-left:{bullet_left_margin};'),
               tags$ul(
                 lapply(1:length(item_bullets[i][[1]]), function(b){
                   tags$li(item_bullets[i][[1]][b])
                   })
               )
      )
  )})
)
}

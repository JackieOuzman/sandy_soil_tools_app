

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(htmlwidgets)
library(slickR)

######################################################################################################
#Simple carousel
#Let's start easy: show the team logos one at a time

slickR(obj = nba_player_logo$uri,height = 100, width = "95%")

#setting functions
slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  settings(dots = TRUE)
######################################################################################################
#There are many more settings you can define, such as autoplay. For all the settings go to slick.js homepage

slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  settings(#dots = TRUE,
           autoplay = TRUE)

######################################################################################################

opts <- settings(
  #dots = TRUE,
  initialSlide = 0,
  slidesToShow = 5, 
  slidesToScroll = 5, 
  focusOnSelect = TRUE)

slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  opts

######################################################################################################

slick_link <- slickR(obj = nba_player_logo$uri,
                     objLinks = nba_player_logo$player_home, #not sure how to do this beyond the example
                     height = 100, width = "95%") 

slick_link + opts

######################################################################################################

cP1 <- htmlwidgets::JS("function(slick,index) {
                            return '<a>'+(index+1)+'</a>';
                       }")

opts_dot_number <- settings(
  initialSlide = 0,
  slidesToShow = 5,
  slidesToScroll = 5,
  focusOnSelect = TRUE,
  dots = TRUE,
  customPaging = cP1 #this line replaces the dots with number as defined as html code in cP1
)

slick_dots <- slickR(
  obj = nba_player_logo$uri,
  height = 100,
  width = "95%"
)

slick_dots + opts_dot_number

######################################################################################################



cP2 <- JS("function(slick,index) {
          return '<a><img src= ' + dotObj[index] + '  width=100% height=100%></a>';
          }")

opts_dot_logo <- 
  settings(
    initialSlide = 0,
    slidesToShow = 5,
    slidesToScroll = 5,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP2
  )

# Putting it all together in one slickR call
s2 <- htmltools::tags$script(
  sprintf("var dotObj = %s", jsonlite::toJSON(nba_team_logo$uri))
)

slick_dots_logo <- slickR(
  obj = nba_player_logo$uri,
  height = 100,
  width = "95%"
) + opts_dot_logo

htmltools::browsable(htmltools::tagList(s2, slick_dots_logo))


######################################################################################################

# Stacking Carousels
# You can stack carousels one on top of the other with the %stack% operator

slick_up_stack <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_down_stack <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_up_stack %stack% slick_down_stack


######################################################################################################

# Synching Carousels
# There are instances when you have many outputs at once and do not want to go through all, so you can stack and synch two carousels one for viewing and one for searching with the %synch% operator.

slick_up_synch <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_down_synch <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_up_synch %synch% slick_down_synch

######################################################################################################



#Adding a Caption to Image
#You can add a caption to an image by synching two carousels, where the upper is the content (e.g. image) and the bottom is the caption (p)

slickR(obj = nba_player_logo$uri[1:2], height = 100, width = "95%") %synch%
  ( slickR(nba_player_logo$name[1:2], slideType = 'p') + settings(arrows = FALSE) )


######################################################################################################

#this is what I want but with my images and captions 9cant get this working yet - and in a shiny app

slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  settings(#dots = TRUE,
    autoplay = TRUE)



imgs <- c('https://static.pexels.com/photos/45201/kitty-cat-kitten-pet-45201.jpeg',
          'http://i0.kym-cdn.com/photos/images/original/000/068/576/kittycat.jpg')
print(imgs)


slickR(obj =  imgs,slideId = 'ex1',
       padding='20%', height=400, width=600)+
  settings(slidesToShow=1,
          centerMode = TRUE,
          dots = TRUE)



slickR::slickR(
  obj =  imgs,
  slideId = 'ex1',
  padding='1%',
  slickOpts = list(
    slidesToShow=1,
    centerMode = TRUE,
    dots = TRUE
  ), height=400, width=600
)


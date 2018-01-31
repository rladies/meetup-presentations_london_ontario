# A fun mini-tutorial based very closely on Maëlle Salmon's recent blogpost, "Galentine's Day"
# http://www.masalmon.eu/2018/01/26/galentinesday/

# Some changes to Maëlle's original code:
#    - stock message text
#    - fonts, background colours, positions changed
#    - r-ladies "stamp" added
#    - removed set.seed to get randomness every time!

library(tidyverse)
library(magick) # image processing
library(charlatan) # make fake data
library(praise) # generate random positive adjectives


# Not feeling like a compliment? Check out these other fun packages instead:
#devtools::install_github("jhollist/dadjoke")
#install.packages("fortunes")
#devtools::install_github("dill/emoGG")

# Get logo from meetup to add to card
rladies_logo <- magick::image_read("https://secure.meetupstatic.com/photos/event/5/8/4/8/highres_458602600.jpeg")
# And in case the wifi doesn't work.... good to have a backup plan!
#rladies_logo <- magick::image_read("Logo_R-ladies.png")

# Who to display as an example?
# We will use this down below to show in our Viewer
example_name <- "Jovana"

# Add more names to the list!
namesList <- c("Jovana", "Demetri", "Vicki", "Sally")
nNames = length(namesList)

# set.seed(some number) sets a random seed to start sampling from a distribution. 
#IF you set the seed to be the same number every time you run your code, the sample will be the same each time (good for reproducibility/simulations!).
# Remove if you want the colors/compliments to be random every time
#set.seed(1111)

# Generate compliment function syntax using the praise package
compliment <- function(name, animal){
     praise::praise(paste0(name, ", you ${adjective}, ${adjective}, ${adjective} ", animal))
}

# Generage compliments table
compliments <- tibble::tibble(name = namesList,
                              animal = sample(rcorpora::corpora(which = "animals/common")$animals, size = nNames),
                              compliment = purrr::map2_chr(name, animal, compliment))


compliments <- dplyr::mutate(compliments, 
                             background = charlatan::ch_hex_color(n = nNames), 
                             text_colour = charlatan::ch_hex_color(n = nNames))


create_card <- function(who, compliments){
     compliments <- dplyr::filter(compliments, name == who)
     magick::image_blank(600, 400,
                         color = compliments$background) %>%
          magick::image_annotate(text = compliments$name,
                                 #font = "Braggadocio",
                                 font = "Phosphate",
                                 color = compliments$text_colour,
                                 location = "+50+25",
                                 size = 72) %>%
          magick::image_annotate(text = compliments$compliment,
                                 font = "Courier",
                                 #color = compliments$text_colour,
                                 color = "white",
                                 location = "+50+150",
                                 #boxcolor = "white",
                                 size = 15) %>%
          magick::image_annotate(text = " THANK YOU FOR COMING!!!! ",
                                 font = "Courier",
                                 boxcolor = compliments$text_colour,
                                 location = "+50+225",
                                 color = "white",
                                 size = 24) %>%
          magick::image_annotate(text = "Love, R-Ladies #LdnOnt ",
                                 font = "Courier",
                                 color = compliments$text_colour,
                                 location = "+125+325",
                                 #boxcolor = "white",
                                 size = 30)%>%
          
          #magick::image_mosaic(rladies_logo)%>%
          magick::image_composite(image_resize(rladies_logo,
                                               #geometry=c(44,44)), 
                                               #offset = "+525+315") %>%
                                               geometry=c(100,100)), 
                                  offset = "+475+25") %>%
          # magick::image_append(text=emo::ji("flower"),
          #      location="+50+50") %>%
          magick::image_write(paste0("Thankyou_cards", who, ".png"))
}

# Execute the function create_card for all names and compliments
purrr::walk(compliments$name, create_card, compliments)

# Read in one of the cards, using the example name defined above.
# This will print it to the viewer so we don't have to open it separately
image_read(paste0("Thankyou_cards",example_name,".png"))



library(gganimate)
library(gifski) #--needed for images
library(png) #--ditto

# NOTE: THESE THINGS TAKE LIKE 1 MINUTE TO RENDER. 

# We'll start with a static plot
myp <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

plot(myp)

# Makes 'species' our frame state
# Only show points from a single state (species) at a time
# Move between the species groups at a rate of 2
# Stay on the static frame for 1
# Don't wrap the transition

myanim <- myp + 
  transition_states(states = Species,
                    transition_length = 2,
                    state_length = 1, 
                    wrap = F) + 
  # ease_aes defines how you move between groups.
  # The default is linear
  ease_aes('cubic-in-out') + # Slow start and end for a smoother look
  # add a title
  ggtitle('Now showing {closest_state}',
        subtitle = 'Frame {frame} of {nframes}')

myanim

# Ok but this is stupid. 
# How to incorporate color?
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(colour = Species), size = 2) + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1) + 
  # Fun other things
  enter_fade() + 
  exit_shrink()

# How do I save it?

# Nifty way to set working directory
library(here)
setwd(here())
anim_save("myanim.gif")

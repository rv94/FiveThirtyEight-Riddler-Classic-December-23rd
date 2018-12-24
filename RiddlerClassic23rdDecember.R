##Load required libraries and clear workspace
rm(list=ls())
library(tidyverse)
library(ggplot2)
##Generate Probability, keep looping for i until probability that that a song won't be replayed exceeds 0.5
for (i in 100:1000000)
{ if  (prod((c(i:(i-100+1)))/(rep(i, 100))) >= 0.5)
  {break
  }
}
print(i)
##7175  
##Initialise empty data frame with two columns, song count and desired probability
df <- data.frame(99, 0)
names(df) <- c("SongCount","NoSnowballProb")
###Generate probabilities for songcount until 47015 (precalculated as the song count for a probability of no snowballs being 0.9)
i<- 100  
for (i in 100:47015){   
      df[i-98,1] <- i
      df[i-98,2] <- (prod((c(i:(i-100+1)))/(rep(i, 100))))
  }
##Plot desired plot 
df %>% ggplot(aes(x = SongCount, y = NoSnowballProb)) +
        geom_line() +
        geom_hline(yintercept = 0.5, color = 'grey', alpha = 0.7) +
        geom_vline(xintercept = 7175, color = 'grey', alpha = 0.7) +
        scale_x_continuous(breaks=seq(0,50000,5000)) +
        scale_y_continuous(breaks=seq(0,1,0.1)) +
        ggtitle("Song Count vs. Probability of Cranky Not Throwing A Snowball") +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.minor.y = element_blank()) +
        xlab("Song Count") +
        ylab("P(Cranky NOT throwing a snowball)")
library(htmltools)
library(ggplot2)
library(compositions)
library(cowplot)
library(animation)

#create simple compositional data set to illustrate dependency
df <- data.frame(mRNA = rep(c("mRNA_1" , "mRNA_2" , "mRNA_3", "mRNA_4"), 2),
                 Expression = c(10, 20, 15, 5, clo(c(10, 20, 15, 5))), 
                 type = rep(c("Absolute Abundance", "Relative Abundance"), each = 4))

draw.bars <- function(change, i){
  #apply change to compositional and non-compositional data
  ndf <- df
  ndf[which(ndf$type == "Absolute Abundance"), ]$Expression <- ndf[which(ndf$type == "Absolute Abundance"), ]$Expression + change
  ndf[which(ndf$type == "Relative Abundance"), ]$Expression <- clo(ndf[which(ndf$type == "Absolute Abundance"), ]$Expression + change)
  #shell data to set the y-axis limits
  df2 <- data.frame(mRNA = rep(c("mRNA_1" , "mRNA_2" , "mRNA_3", "mRNA_4"), 2),
                    limit = c(rep(30,4), rep(1,4)), 
                    type = rep(c("Absolute Abundance", "Relative Abundance"), each = 4))
  #plot the expression
  pl <- ggplot(ndf, aes(x = mRNA, y = Expression)) + 
    geom_bar(stat = "identity") + 
    geom_point(data = df2, aes(x = mRNA, y = limit), color = "white") +
    xlab("") + 
    facet_wrap(~type, scales = "free_y") 
  png(paste0("./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency", i, ".png"), width = 480, height = 480)
  # ggplot2::ggsave(paste0("./ASA_Poster_Content/images/Demonstrate_CoDA_Diff", i, ".png"),plot = pl, width = 480, height = 480)
  print(pl)
  dev.off()  
}

changes <- list(c(0,0,0,0),
                c(0,2,0,0),
                c(0,4,0,0),
                c(0,6,0,0),
                c(0,8,0,0),
                c(0,10,0,0))

draw.bars.alr <- function(change, i){
  # browser()
  #apply change to compositional and non-compositional data
  ndf <- df[-8, ]
  ndf[which(ndf$type == "Absolute Abundance"), ]$Expression <- ndf[which(ndf$type == "Absolute Abundance"), ]$Expression + change
  ndf[which(ndf$type == "Relative Abundance"), ]$Expression <- alr(ndf[which(ndf$type == "Absolute Abundance"), ]$Expression + change)
  ndf$type <- ifelse(ndf$type == "Absolute Abundance", "Absolute Abundance", "ALR Transformed")
  #shell data to set the y-axis limits
  df2 <- data.frame(mRNA = rep(c("mRNA_1" , "mRNA_2" , "mRNA_3", "mRNA_4"), 2),
                    limit = c(rep(30, 4), rep(2.2, 4)), 
                    type = rep(c("Absolute Abundance", "ALR Transformed"), each = 4))
  #plot the expression
  pl <- ggplot(ndf, aes(x = mRNA, y = Expression)) + 
    geom_bar(stat = "identity") + 
    geom_point(data = df2, aes(x = mRNA, y = limit), color = "white") +
    xlab("") + 
    facet_wrap(~type, scales = "free_y") 
  png(paste0("./ASA_Poster_Content/images/Demonstrate_ALR", i, ".png"), width = 480, height = 480)
  print(pl)
  dev.off()  
}

for(i in 1:length(changes)){
  draw.bars(changes[[i]], i)
}

for(i in 1:length(changes)){
  draw.bars.alr(changes[[i]], i)
}

files <- list.files("./ASA_Poster_Content/images", full.names = TRUE, pattern = "Dependency")
paste(files, collapse = " ")
shell('convert.exe -loop 0 -delay 100 ./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency1.png ./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency2.png ./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency3.png ./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency4.png ./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency5.png ./ASA_Poster_Content/images/Demonstrate_CoDA_Dependency6.png  "C:/Classes/ASA_Poster/ASA_Poster_Content/images/BarAnimation.gif"')

files2 <- list.files("./ASA_Poster_Content/images", full.names = TRUE, pattern = "ALR")
paste(files2, collapse = " ")
shell('convert.exe -loop 0 -delay 100 ./ASA_Poster_Content/images/Demonstrate_ALR1.png ./ASA_Poster_Content/images/Demonstrate_ALR2.png ./ASA_Poster_Content/images/Demonstrate_ALR3.png ./ASA_Poster_Content/images/Demonstrate_ALR4.png ./ASA_Poster_Content/images/Demonstrate_ALR5.png ./ASA_Poster_Content/images/Demonstrate_ALR6.png  "C:/Classes/ASA_Poster/ASA_Poster_Content/images/CLRAnimation.gif"')


plot.diff <- function(change, i){
  ndf <- df
  ndf$diff <- 0
  ndf[which(ndf$type == "Absolute Abundance"), ]$diff <- (ndf[which(ndf$type == "Absolute Abundance"), ]$Expression + change) -
    ndf[which(ndf$type == "Absolute Abundance"), ]$Expression
  ndf[which(ndf$type == "Relative Abundance"), ]$diff <- clo((ndf[which(ndf$type == "Relative Abundance"), ]$Expression + change)) -
    ndf[which(ndf$type == "Relative Abundance"), ]$Expression
  ndf$type <- ifelse(ndf$type == "Absolute Abundance", "Actual Difference", "Perceived Difference")
  df2 <- data.frame(mRNA = rep(c("mRNA_1" , "mRNA_2" , "mRNA_3", "mRNA_4"), 2),
                    limit = c(rep(6,2), rep(-6, 2), rep(0.6, 2), rep(-0.6, 2)), 
                    type = rep(c("Actual Difference", "Perceived Difference"), each = 4))
  pl <- ggplot(ndf, aes(x = mRNA, y = diff)) +
    geom_bar(stat = "identity") +
    geom_point(data = df2, aes(x = mRNA, y = limit), color = "white") +
    xlab("") + 
    facet_wrap(~type, scales = "free_y") 
  png(paste0("./ASA_Poster_Content/images/Demonstrate_CoDA_Diff", i, ".png"), width = 480, height = 480)
  # ggplot2::ggsave(paste0("./ASA_Poster_Content/images/Demonstrate_CoDA_Diff", i, ".png"),plot = pl, width = 480, height = 480)
  print(pl)
  dev.off()
  
}

for(i in 1:length(changes)){
  plot.diff(changes[[i]], i)
}



# shell('convert.exe -loop 0 -delay 100 C:\Classes\Dissertation\ASA_Poster_Content\images\Demostrate_CoDA_Diff1.png C:\Classes\Dissertation\ASA_Poster_Content\images\Demostrate_CoDA_Diff2.png C:\Classes\Dissertation\ASA_Poster_Content\images\Demostrate_CoDA_Diff3.png C:\Classes\Dissertation\ASA_Poster_Content\images\Demostrate_CoDA_Diff4.png C:\Classes\Dissertation\ASA_Poster_Content\images\Demostrate_CoDA_Diff5.png C:\Classes\Dissertation\ASA_Poster_Content\images\Demostrate_CoDA_Diff6.png "C:\Classes\Dissertation\ASA_Poster_Content\DiffAnimation.gif"')
# 
# files <- list.files("C:\\Classes\\Dissertation\\ASA_Poster_Content\\images", pattern = "Diff")
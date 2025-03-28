library(RFishBC)
library(dplyr)
library(FSA)
library(readxl)
library(ggplot2)
library(readr)

# cambiare anno e cartella a mano
dirname.base <- "C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/SOLEA_SS3/dati/VBGP/Paper_bifasic/BC"
dirname  <- paste0(dirname.base,'/', "Fuori/2014" ) # cambiare per ogni anno e ogni dentro fuori
setwd(dirname)
list2014_F <- listFiles("rds") # cambiare per ogni anno e ogni dentro fuori tipo list2014_D per "Dentro" etc.
list2014_F <- combineData(list2014_F) # cambiare per ogni anno e ogni dentro fuori
list2014_F$id <- as.character(parse_number(list2014_F$id))  # cambiare per ogni anno e ogni dentro fuori
list2014_F$area <- "Fuori"
list2014_F$year <- 2014 # cambiare per ogni anno
##



listfin <- rbind(list2014F,list2014D,list2015F,list2015D,list2016F,list2016D,list2017F,list2017D,list2018F,list2018D)


write.csv(listfin, "listfin2014_2018.csv") # db 2014-2018
##

###############################
setwd("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/SOLEA_SS3/dati/VBGP/Paper_bifasic/BC")
# Data for Back-Calculation
# merge con db con le lunghezze alla cattura
info_oto  <- read_excel("C:/Users/f.masnadi/Desktop/Stock Assessment/SS3/SOLEA_SS3/dati/VBGP/datiSolea_14_18.xlsx")
  info_oto$TL <-  info_oto$TL *10
info_oto$id <- as.character(parse_number(info_oto$Code)) 
info_oto  <- info_oto %>% select(-Specie,-anno,-`TL(cm)`,-Code)
fishdat <- info_oto %>%
  inner_join(listfin,by="id")

BClistfin <- backCalc(fishdat,TL,BCM = 1,inFormat="long",outFormat="long",digits=1);BClistfin # using  the Dahl-Lea model
write.csv(BClistfin,file="BClistfin.csv",quote=FALSE,row.names=FALSE)
#length(unique(fishdat$id)); length(unique(BClistfin$id))







#########################################################
# grafici finaliper paper
###########################################################
BC2007_2020 <- read_excel("BC2007_2020.xlsx",   sheet = "BCpaper_subset")
BC2007_2020$id <- as.character(BC2007_2020$id )
BC2007_2020  <- BC2007_2020 %>% filter(Age >0)

#plot by year
jpeg("plot by year 2007-2020 and sex.jpeg",width = 350, height = 350, units = "mm", res = 300)
ggplot(data=BC2007_2020, aes(x=Age,y=TL, colour=id)) + geom_point(  size=2) + xlab("time (Age)") + ylab("BC len (mm)")+geom_line( ) +  facet_grid(   year ~Sex ) # +facet_wrap(~  year,Sex)
dev.off()
jpeg("plot all 2007-2020.jpeg",width = 350, height = 250, units = "mm", res = 300)
ggplot(data=BC2007_2020, aes(x=Age,y=TL, colour=id)) + geom_point(  size=2) + xlab("time (Age)") + ylab("BC len (mm)")+geom_line( ) 
dev.off()
jpeg("plot by sex 2007-2020.jpeg",width = 350, height = 250, units = "mm", res = 300)
ggplot(data=BC2007_2020, aes(x=Age,y=TL, colour=id)) + geom_point(  size=2) + xlab("time (Age)") + ylab("BC len (mm)")+geom_line( )  +facet_wrap(~  Sex)
dev.off()
#ggplot(BC2007_2020, aes(Age, TL, colour=id)) + geom_line() + geom_point() +facet_wrap(~  year) + theme(legend.position='none')



# calculate mean back-calculated length-at-age 
tmp <- BC2007_2020 %>%
  group_by(Age) %>%  # add year for second plot
  summarize(n=validn(TL),
            mn=round(mean(TL),0),
            sd=round(sd(TL),1)) %>%
  as.data.frame()
tmp$up <- tmp$mn + tmp$sd
tmp$dw <- tmp$mn - tmp$sd
# plot mean back-calculated TL by age and SD
jpeg("plot mean BC by age.jpeg",width = 350, height = 250, units = "mm", res = 300)
ggplot(tmp, aes(Age, mn)) + geom_line() + geom_line(aes(Age, up),linetype = 2) + geom_line(aes(Age, dw), linetype = 2) + geom_point()+ xlab("time (Age)") + ylab("BC len (mm)")
dev.off()
jpeg("plot mean BC by age by year.jpeg",width = 350, height = 250, units = "mm", res = 300)
ggplot(tmp, aes(Age, mn)) + geom_line() + geom_line(aes(Age, up),linetype = 2) + geom_line(aes(Age, dw), linetype = 2) + geom_point()+ xlab("time (Age)") + ylab("BC len (mm)")+facet_wrap(~  year)
dev.off()





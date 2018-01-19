cruisePoly <- ggplot(aes(cruise, Sv_mean, group = cruise)) + geom_boxplot(data= acousticdata, ,fill = "lightblue") + geom_point(data=acousticmean, color = "orange", size = 3)
cruisePoly = cruisePoly +scale_x_discrete(labels = cruises3)+theme(axis.title.x =element_text(vjust=-3), axis.title.y =element_text(hjust = 0.5), axis.text.x=element_text(hjust=0.5)) + theme(text = element_text(size = 20))
cruisePoly = cruisePoly + labs(x="Cruise (Season.Year, W = winter and S = summer)", y = "Mean Volume Scattering (dB re 1 m^2 m^-3 )")
cruisePoly = cruisePoly + theme(plot.margin = unit(c(1, 8, 1, 8), "cm"))
cruisePoly

acousticdata$cruise = as.factor(acousticdata$cruise)



acousticmean = aggregate(Sv_mean~cruise, acousticdata, FUN = mean)
I = which(acousticdata$cruise == "1")
acousticdata=acousticdata[-I,]
I = which(acousticdata$region == "Northern")
acousticdata=acousticdata[-I,]
I = which(acousticdata$Lat_M < 26)
acousticdata=acousticdata[-I,]

acousticmean = aggregate(Sv_mean~cruise, acousticdata, FUN = mean)

cruisePolyGB <- ggplot(acousticdata,aes(cruise, Sv_mean, group = cruise)) + geom_boxplot(fill = "lightblue") + geom_point(data=acousticmean, color = "orange", size = 3)
cruisePolyGB = cruisePolyGB +scale_x_discrete(labels = cruises3)+theme(axis.title.x =element_text(vjust=-3), axis.title.y =element_text(hjust = 0.5), axis.text.x=element_text(hjust=0.5)) + theme(text = element_text(size = 20))
cruisePolyGB = cruisePolyGB + labs(x="Cruise (Season.Year, W = winter and S = summer)", y = "Mean Volume Scattering (dB re 1 m^2 m^-3 )")
cruisePolyGB = cruisePolyGB + theme(plot.margin = unit(c(1, 8, 1, 8), "cm"))
cruisePolyGB

aov.acoustics.sv <- aov(Sv_mean ~ cruise, data = acousticdata)
summary(aov.acoustics.sv)

aov.acoustics.sv2 <- aov(Sv_mean ~ cruise*region, data = acousticdata)
summary(aov.acoustics.sv2)

### POST-HOC tests for ANOVA....are these appropriate for these data - 11/28/2017 ###
library(multcomp)
library(foreign)
# aov.cruise <- aov(sA ~ CPoly, data = fishing)
test = TukeyHSD(aov.acoustics.sv2)
test 

cruises3 = c("W.07", "S.07", "W.10","W.11", "S.12","S.13", "W.14","S.14","S.16","S.17")





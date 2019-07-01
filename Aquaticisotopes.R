library(readxl)
library(ggplot2)
library(cluster)
library(factoextra)

aquatic <- read_excel("2019_EA_Houston_class project.xlsx")

ggplot(aquatic, aes(x=`corrected d13C (VPDB)`, y=`corrected d15N (Air)`, color=Trophic)) + geom_point() +
  geom_text(aes(label=Trophic), check_overlap = FALSE, hjust = 0, nudge_x = 0.05) +
  ylim(c(6,12.5)) + scale_color_viridis_d(end=0.9) + theme_bw() + coord_equal()
  
ggsave(file="quickisoplot.png", dpi=300, scale=1.4)

summary(aquatic)

aq.scaled <- scale(aquatic[,c(5:6)])
rownames(aq.scaled) <- aquatic$Trophic
head(aq.scaled)

distance <- get_dist(aq.scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(aq.scaled, centers = 6, nstart = 25)
fviz_cluster(k2, data = aq.scaled)

simmr.iso <- simmr_l
###########33
aq.scaled2 <- scale(aquatic[,c(7:8)])
rownames(aq.scaled2) <- aquatic$Trophic


distance2 <- get_dist(aq.scaled2)
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
ggsave(file="sampledistanceplot.png", dpi=400)

c2 <- kmeans(aq.scaled2, centers = 2, nstart = 25)
fviz_cluster(c2, data = aq.scaled2)
ggsave(file="sampleclusterplot.png", dpi=400, scale=1.5)

#load model object
load(file="outputs/Full Models.RData")

library(emmeans)
library(ggplot2)

summary(mod2.2.1.int)
#mod2.2.1.int
#code from: https://www.researchgate.net/post/How-to-use-lsmeans-to-make-interaction-plots-in-R
refR <- lsmeans(mod2.2.1.int, specs = c("OAlab","year"))
ref_dfR <- as.data.frame(summary(refR))
#pd <- position_dodge(0.1)
g4R <- ggplot(ref_dfR, aes(x=OAlab, y=lsmean,group=OAlab, colour=year))+
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=0.1) +
  geom_point()+theme_classic()+ggtitle("")+xlab("")+ylab("")+
  theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
print(g4R)










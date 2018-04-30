library(tidyverse)
library(directlabels)

## TTR plot
ttrs <- read_csv("meanttrs.csv")

pdf("ttrs.pdf", width = 6, height = 4)
ggplot(ttrs, aes(x = tokens, y = types, color = source, label = source)) + 
  #geom_smooth() +
  geom_point(size = .2) + 
  theme_classic(base_size = 18) + 
  geom_dl(method = list(dl.trans(x=x +.2), "last.qp", cex=1)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 36000))
dev.off()
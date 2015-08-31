---
layout: default
title:  "Overview"
date:   2015-10-08 15:46:20
categories: pocketbook
---




```r
knitr::read_chunk(paste0(root.dir,"input/code/code_part5.R"))
```



```r
source(paste0(root.dir,'./input/code/plot/plot_color.R'))

syb_part <- 5

## Part 5
colPart5 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart5[["Main"]][1]
## color for the grid
col.main2 <- colPart5[["Main"]][2]

source(paste0(root.dir,"./input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'./input/code/plot/map_categories.R'))
 # --------------------------------------------------------------- #


#    ___                                  _                   
#   / _ \  __   __   ___   _ __  __   __ (_)   ___  __      __
#  | | | | \ \ / /  / _ \ | '__| \ \ / / | |  / _ \ \ \ /\ / /
#  | |_| |  \ V /  |  __/ | |     \ V /  | | |  __/  \ V  V / 
#   \___/    \_/    \___| |_|      \_/   |_|  \___|   \_/\_/  
#                                                            
```

<!--
#    ___                                  _                   
#   / _ \  __   __   ___   _ __  __   __ (_)   ___  __      __
#  | | | | \ \ / /  / _ \ | '__| \ \ / / | |  / _ \ \ \ /\ / /
#  | |_| |  \ V /  |  __/ | |     \ V /  | | |  __/  \ V  V / 
#   \___/    \_/    \___| |_|      \_/   |_|  \___|   \_/\_/  
#                                                            
-->



```r
spread_title <- "Overview"
short_text <- "A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to population growth in the world. While growth rates have been slowing since the late 1960s, the world’s population has nevertheless doubled since then, to over 7 billion people. Population growth is generally highest where income levels are low. This is especially true in cities. Since 2008, there have been more people living in cities than in rural areas."
 # --------------------------------------------------------------- #
```

<h1> Overview </h1> 
<p>A combination of declining mortality rates, prolonged life expectancy and younger populations in regions with high fertility contributes to population growth in the world. While growth rates have been slowing since the late 1960s, the world’s population has nevertheless doubled since then, to over 7 billion people. Population growth is generally highest where income levels are low. This is especially true in cities. Since 2008, there have been more people living in cities than in rural areas.</p> 



```r
# Retrieve data
library(FAOSTAT)
dat <- getFAOtoSYB(domainCode = "OA",
                   elementCode = 551,
                   itemCode = 3010)
dat1 <- dat$aggregates
dat <- getFAOtoSYB(domainCode = "OA", 
                   elementCode = 561,
                   itemCode = 3010)
dat2 <- dat$aggregates
dat <- left_join(dat1,dat2)
df <- gather(dat, variable, value, 3:4)
 # --------------------------------------------------------------- #
```



```r
dat <- df %>% select(FAOST_CODE,Year,variable,value)
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat$variable <- as.character(dat$variable)
dat$variable[dat$variable == "OA_3010_551"] <- "Urban population"
dat$variable[dat$variable == "OA_3010_561"] <- "Rural population"

dat <- dat %>% group_by(Year,variable) %>%  summarise(value = sum(value, na.rm=TRUE)/1000000)

# print data for technical report
#datatable(dat)

# Draw the plot
p <- ggplot(dat, aes(x = Year, y = value))
p <- p + geom_area(aes(fill=variable), stat = "identity",position = "stack")
p <- p + scale_fill_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + theme(axis.text.x = element_text(angle = 45))
p <- p + labs(x="",y="billion people")
p
```

![plot of chunk P5overTOPRIGHT](figure/P5overTOPRIGHT-1.png) 

```r
# Caption

caption_text <- "rural and urban population"
```

</br> <p class='caption'>rural and urban population</p>



```r
# data
dat <- filter(syb.df, Year %in%
                   c(2004:2014)) %>% 
                  group_by(FAOST_CODE,SHORT_NAME) %>% 
                  dplyr::summarise(OA.TPBS.POP.PPL.GR10 = mean(OA.TPBS.POP.PPL.GR10, na.rm=TRUE))
dat <- ungroup(dat)
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.GR10),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -OA.TPBS.POP.PPL.GR10)
top10 <- dat %>% slice(1:10) %>% mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, OA.TPBS.POP.PPL.GR10),y=OA.TPBS.POP.PPL.GR10))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P5overLEFT](figure/P5overLEFT-1.png) 

```r
# Caption
caption_text <- "Population, average annual growth (2004-2014)"
 # --------------------------------------------------------------- #
```

</br> <p class='caption'>Population, average annual growth (2004-2014)</p>


```r
# data
dat <- syb.df %>% filter(Year %in% 2013) %>% select(FAOST_CODE,SP.DYN.LE00.IN)
dat <- dat[!is.na(dat$SP.DYN.LE00.IN),]

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -SP.DYN.LE00.IN)
top10 <- dat %>% slice(1:10) %>% mutate(color = "Countries with highest values")
bot10 <- dat %>% slice( (nrow(dat)-9):nrow(dat)) %>% mutate(color = "Countries with lowest values")
dat_plot <- rbind(top10,bot10)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, SP.DYN.LE00.IN),y=SP.DYN.LE00.IN))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="percent")
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P5overRIGHT](figure/P5overRIGHT-1.png) 

```r
# Caption
caption_text <- "Life expectancy at birth, countries with the highest and lowest values (2013)"
 # --------------------------------------------------------------- #
```

</br> <p class='caption'>Life expectancy at birth, countries with the highest and lowest values (2013)</p>



```r
# data
dat <- syb.df %>% filter(Year %in% c(2000:2014)) %>% 
  select(FAOST_CODE,Year,SHORT_NAME,OA.TEAPT.POP.PPL.NO)
dat <- dat[!is.na(dat$OA.TEAPT.POP.PPL.NO),]
dat <- dat[!is.na(dat$SHORT_NAME),]


# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat %>% group_by(subgroup,Year) %>% 
  summarise(OA.TEAPT.POP.PPL.NO = sum(OA.TEAPT.POP.PPL.NO, na.rm=TRUE)) %>% 
  mutate(OA.TEAPT.POP.PPL.NO = OA.TEAPT.POP.PPL.NO / 1000000)

p <- ggplot(dat_plot, aes(x=Year,y=OA.TEAPT.POP.PPL.NO,color=subgroup))
p <- p + geom_point() + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, length(unique(dat_plot$subgroup)))[["Sub"]])
p <- p + labs(x="",y="million people")
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P5overBOTTOM](figure/P5overBOTTOM-1.png) 

```r
# Caption
caption_text <- "Total economically active population (2000 to 2014)"
```

</br> <p class='caption'>Total economically active population (2000 to 2014)</p>



```r
dat <- syb.df %>% filter(Year %in% 2014, FAOST_CODE < 5000) %>% select(FAOST_CODE,SHORT_NAME,OA.TPR.POP.PPL.SHP)

map.plot <- left_join(dat,map.df)

# Add region key and subset
#map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","OA.TPR.POP.PPL.SHP")]
cat_data$value_cat <- categories(x=cat_data$OA.TPR.POP.PPL.SHP, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "Percent"

# graticule
grat_robin <- spTransform(graticule, CRS("+proj=robin"))  # reproject graticule
gr_rob <- fortify(grat_robin)
# crop the grid
if (!(region_to_report %in% c("GLO","COF"))) {
  gr_rob <- gr_rob[gr_rob$lat >= min(map.plot$lat) & gr_rob$lat <= max(map.plot$lat),]
  gr_rob <- gr_rob[gr_rob$long >= min(map.plot$long) & gr_rob$long <= max(map.plot$long),]
} else gr_rob <- gr_rob

create_map_here()
```

![plot of chunk P5overMAP](figure/P5overMAP-1.png) 

```r
# Caption
caption_text <- "Rural population, share of total population (2014)"
```

</br> <p class='caption'>Rural population, share of total population (2014)</p>



[jekyll-gh]: https://github.com/jekyll/jekyll
[jekyll]:    http://jekyllrb.com

---
layout: default
title:  "Coffee trade"
date:   2015-10-08 15:46:20
categories: pocketbook
---


```r
knitr::read_chunk(paste0(root.dir,"input/code/code_part6.R"))
```



```r
source(paste0(root.dir,'./input/code/plot/plot_color.R'))

syb_part <- 6

## Part 4
colPart6 <- plot_colors(part = syb_part, 12)
col.main1 <- colPart6[["Main"]][1]
## color for the grid
col.main2 <- colPart6[["Main"]][2]

source(paste0(root.dir,"./input/code/plot/theme.R"))

# map functions
source(paste0(root.dir,'./input/code/plot/map_categories.R'))


# 
#    ____            __    __                                              _                  _     _                 
#   / ___|   ___    / _|  / _|   ___    ___     _ __    _ __    ___     __| |  _   _    ___  | |_  (_)   ___    _ __  
#  | |      / _ \  | |_  | |_   / _ \  / _ \   | '_ \  | '__|  / _ \   / _` | | | | |  / __| | __| | |  / _ \  | '_ \ 
#  | |___  | (_) | |  _| |  _| |  __/ |  __/   | |_) | | |    | (_) | | (_| | | |_| | | (__  | |_  | | | (_) | | | | |
#   \____|  \___/  |_|   |_|    \___|  \___|   | .__/  |_|     \___/   \__,_|  \__,_|  \___|  \__| |_|  \___/  |_| |_|
#                                              |_|                                                                    
# 
```



```r
spread_title <- "Coffee trade"
short_text <- " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus egestas risus at lobortis lacinia. Mauris a nunc eleifend, sodales magna ut, congue arcu. Fusce in odio nunc. Mauris vehicula faucibus eros a blandit. Aenean ut tempus ipsum, eu faucibus lorem. Maecenas pretium nibh sit amet nulla accumsan, eu auctor massa facilisis. In malesuada nisl quis sem dapibus iaculis. Ut fermentum leo turpis, convallis luctus elit auctor sed. Quisque nec vestibulum augue. Praesent suscipit finibus tellus, ut semper quam fermentum luctus. "
```

<h1> Coffee trade </h1> 
<p> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus egestas risus at lobortis lacinia. Mauris a nunc eleifend, sodales magna ut, congue arcu. Fusce in odio nunc. Mauris vehicula faucibus eros a blandit. Aenean ut tempus ipsum, eu faucibus lorem. Maecenas pretium nibh sit amet nulla accumsan, eu auctor massa facilisis. In malesuada nisl quis sem dapibus iaculis. Ut fermentum leo turpis, convallis luctus elit auctor sed. Quisque nec vestibulum augue. Praesent suscipit finibus tellus, ut semper quam fermentum luctus. </p> 



```r
# Exports, coffee:
## Green
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 656)
TP_656_5922 <- dat$aggregates

## Roasted
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 657)
TP_657_5922 <- dat$aggregates

## Extracts
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5922,
                   itemCode = 659)
TP_659_5922 <- dat$aggregates

# Imports, coffee:
## Green
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 656)
TP_656_5622 <- dat$aggregates


## Roasted
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 657)
TP_657_5622 <- dat$aggregates

## Extracts
dat <- getFAOtoSYB(domainCode = "TP",
                   elementCode = 5622,
                   itemCode = 659)
TP_659_5622 <- dat$aggregates
```




```r
dat <- left_join(TP_656_5622,TP_657_5622)
dat <- left_join(dat,TP_659_5622)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "TP_656_5622"] <- "Coffee, green"
dat$fill[dat$variable == "TP_657_5622"] <- "Coffee, roasted"
dat$fill[dat$variable == "TP_659_5622"] <- "Coffee, extracts"

dat <- dat[!is.na(dat$value),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(Year,fill) %>% 
          dplyr::summarise(value  = sum(value, na.rm=TRUE)) %>%  ungroup()

p <- ggplot(dat_plot, aes(x=Year, y=value, color=fill))
p <- p + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="1 000 US$")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P6coffeetradeTOPRIGHT](figure/P6coffeetradeTOPRIGHT-1.png) 

```r
# Caption
caption_text <- "Imports of coffee"
```

</br> <p class='caption'>Imports of coffee</p>



```r
dat <- TP_656_5922[!is.na(TP_656_5922$TP_656_5922),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -TP_656_5922)
top15 <- dat %>% slice(1:20) %>% mutate(color = "2012")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% mutate(color = "2000")
dat_plot <- rbind(top15,top91)
# 
p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, TP_656_5922),y=TP_656_5922))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="mln tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P6coffeetradeLEFT](figure/P6coffeetradeLEFT-1.png) 

```r
# Caption
caption_text <- "Top 20 coffee exporters in 2012"
```

</br> <p class='caption'>Top 20 coffee exporters in 2012</p>


```r
dat <- TP_656_5622[!is.na(TP_656_5622$TP_656_5622),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -TP_656_5622)
top15 <- dat %>% slice(1:20) %>% mutate(color = "2012")
top91 <- dat %>% filter(FAOST_CODE %in% top15$FAOST_CODE, Year == 2000) %>% mutate(color = "2000")
dat_plot <- rbind(top15,top91)
# 
p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, TP_656_5622),y=TP_656_5622))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="mln tonnes")
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P6coffeetradeRIGHT](figure/P6coffeetradeRIGHT-1.png) 

```r
# Caption
caption_text <- "Top 20 coffee importers in 2012"
```

</br> <p class='caption'>Top 20 coffee importers in 2012</p>



```r
dat <- left_join(TP_656_5922,TP_657_5922)
dat <- left_join(dat,TP_659_5922)

# Add region key and subset
dat <- left_join(dat,region_key)
dat <- dat[which(dat[[region_to_report]]),]

dat <- gather(dat, variable, value, 3:5)
dat$fill[dat$variable == "TP_656_5922"] <- "Coffee, green"
dat$fill[dat$variable == "TP_657_5922"] <- "Coffee, roasted"
dat$fill[dat$variable == "TP_659_5922"] <- "Coffee, extracts"

dat <- dat[!is.na(dat$value),]

# DEFAULT GROUPING
df <- subgrouping(region_to_report = region_to_report)

# merge data with the region info
dat_plot <- merge(dat,df[c("FAOST_CODE","subgroup")],by="FAOST_CODE")

# AGREGATE
dat_plot <- dat_plot %>% group_by(Year,fill) %>% 
          dplyr::summarise(value  = sum(value, na.rm=TRUE)) %>%  ungroup()

p <- ggplot(dat_plot, aes(x=Year, y=value, color=fill))
p <- p + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 3)[["Sub"]])
p <- p + labs(x="",y="1 000 US$")
p <- p + theme(axis.text.x = element_text(angle=45))
p <- p + guides(color = guide_legend(nrow = 2))
p
```

![plot of chunk P6coffeetradeBOTTOM](figure/P6coffeetradeBOTTOM-1.png) 

```r
# Caption
caption_text <- "Exports of coffee"
```

</br> <p class='caption'>Exports of coffee</p>



```r
dat <- TP_656_5622 %>% filter(Year %in% 2012, FAOST_CODE < 5000)
pop <- syb.df %>% filter(Year %in% 2012, FAOST_CODE < 5000) %>% select(FAOST_CODE,OA.TPBS.POP.PPL.NO)

dat <- left_join(dat,pop)

dat <- dat %>% mutate(import_per_capita = (TP_656_5622 * 1000) / OA.TPBS.POP.PPL.NO)

map.plot <- full_join(dat,map.df)

# Add region key and subset

# map.plot <- map.plot[which(map.plot[[region_to_report]]),]

cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","import_per_capita")]
cat_data$value_cat <- categories(x=cat_data$import_per_capita, n=5, method="jenks")

map.plot <- left_join(map.plot,cat_data[c("FAOST_CODE","value_cat")])

# define map unit
map_unit <- "US$ per capita"

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

![plot of chunk P6coffeetradeMAP](figure/P6coffeetradeMAP-1.png) 

```r
# Caption
caption_text <- "Coffee imports in 2012"







#    ____         __   __                            _                 
#   / ___| ___   / _| / _|  ___   ___   _ __   _ __ (_)  ___  ___  ___ 
#  | |    / _ \ | |_ | |_  / _ \ / _ \ | '_ \ | '__|| | / __|/ _ \/ __|
#  | |___| (_) ||  _||  _||  __/|  __/ | |_) || |   | || (__|  __/\__ \
#   \____|\___/ |_|  |_|   \___| \___| | .__/ |_|   |_| \___|\___||___/
#                                      |_|                             
```

</br> <p class='caption'>Coffee imports in 2012</p>

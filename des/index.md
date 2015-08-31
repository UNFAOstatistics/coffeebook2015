---
layout: default
title:  "Dietary energy supply"
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





```r
spread_title <- "Dietary energy supply"
short_text <- "The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products."
 # --------------------------------------------------------------- #
```

<h1> Dietary energy supply </h1> 
<p>The dietary energy supply (DES) is the food available for human consumption, expressed in kilocalories per person per day. At the country level, it is calculated as a measure of food available for human use after taking out all non-food utilization, including exports, industrial use, animal feed, seed, wastage and changes in stocks. In 1961 the average global calorie availability was as low as 2 196 kcal/cap/day; by 2011, it had reached 2 870 kcal/cap/day, and was centered more around a narrow base of staple grains as well as meat and dairy products.</p> 


```r
# Retrieve data
dat <- read.csv(paste0(data.dir,"/Data/Raw/FSI2015_DisseminationDataset.csv"), stringsAsFactors=FALSE)
metdat <- read.csv(paste0(data.dir,"Data/Raw/FSI2015_DisseminationMetadata.csv"), stringsAsFactors=FALSE)
dat$FAOST_CODE <- as.factor(dat$FAOST_CODE)
dat$FAOST_CODE <- as.numeric(levels(dat$FAOST_CODE))[dat$FAOST_CODE]
# SOFI to M49 conversions
# Asia
dat$FAOST_CODE[dat$FAOST_CODE == 5853] <- 5300
dat$FAOST_CODE[dat$FAOST_CODE == 5001] <- 5000

# Add Area var from sybdata.df
tmp <- syb.df[!duplicated(syb.df[c("FAOST_CODE","Area")]),]
dat <- merge(dat,tmp[c("FAOST_CODE","Area")],by="FAOST_CODE")
dat <- merge(dat,FAOcountryProfile[c("FAOST_CODE","SHORT_NAME")],by="FAOST_CODE", all.x=TRUE)
# M49LatinAmericaAndCaribbean
dat$Area[dat$FAOST_CODE == 5205] <- "M49macroReg"
# dat$FS.OA.NOU.P3D1[dat$FS.OA.NOU.P3D1 == "<0.1"] <- 0.01
# dat$FS.OA.NOU.P3D1[dat$FS.OA.NOU.P3D1 == "ns"] <- 0
dat$FS.OA.NOU.P3D1 <- as.factor(dat$FS.OA.NOU.P3D1)
dat$FS.OA.NOU.P3D1 <- as.numeric(levels(dat$FS.OA.NOU.P3D1))[dat$FS.OA.NOU.P3D1]
dat$FS.OA.POU.PCT3D1[dat$FS.OA.POU.PCT3D1 == "<5.0"] <- 0.1
dat$FS.OA.POU.PCT3D1 <- as.factor(dat$FS.OA.POU.PCT3D1)
dat$FS.OA.POU.PCT3D1 <- as.numeric(levels(dat$FS.OA.POU.PCT3D1))[dat$FS.OA.POU.PCT3D1]

df <- dat[!duplicated(dat[c("FAOST_CODE","Year")]),]

# For despie graphs icn2.df
load(paste0(root.dir,"../ICN2PB14/Data/Processed/icn2.RData"))

 # --------------------------------------------------------------- #
```



```r
## Plot
despie <- icn2.df[icn2.df$Year %in% c(2009:2011), c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.SDES.CRLS.PCT3D","FBS.SDES.SR.PCT3D","FBS.SDES.SS.PCT3D","FBS.SDES.MO.PCT3D","FBS.SDES.VOAF.PCT3D","FBS.SDES.MEB.PCT3D")]
#despie <- despie[despie$FAOST_CODE %in% "5000",]

dw <- gather(despie,
             "var",
             "value",
             4:9)
d <- dw %>% group_by(FAOST_CODE,var) %>% dplyr::summarise(mean = mean(value))

d$var <- as.character(d$var)
d$var[d$var == "FBS.SDES.CRLS.PCT3D"] <- "Cereals\n(excl. beer)"
d$var[d$var == "FBS.SDES.SR.PCT3D"] <- "Starchy roots"
d$var[d$var == "FBS.SDES.SS.PCT3D"] <- "Sugar and\nsweeteners"
d$var[d$var == "FBS.SDES.MO.PCT3D"] <- "Meat and offals"
d$var[d$var == "FBS.SDES.VOAF.PCT3D"] <- "Milk\n(excl. butter)"
d$var[d$var == "FBS.SDES.MEB.PCT3D"] <- "Veg. oils and\nanimal fats"

d$FAOST_CODE <- factor(d$FAOST_CODE)
d$FAOST_CODE <- as.numeric(levels(d$FAOST_CODE))[d$FAOST_CODE]

dat <- left_join(d,region_key)

## option 1
pop <- syb.df %>% select(FAOST_CODE,Year,OA.TPBS.POP.PPL.NO) %>%  group_by(FAOST_CODE) %>% filter(Year == 2014)

dat <- dat[which(dat[[region_to_report]]),]

dat <- left_join(dat[c("FAOST_CODE","var","mean")],pop)
dat <- dat[!is.na(dat$mean),]
dat <- dat[!is.na(dat$OA.TPBS.POP.PPL.NO),]

dat_s <- dat %>% group_by(var) %>%  dplyr::summarise(wmean = weighted.mean(mean, OA.TPBS.POP.PPL.NO, na.rm=FALSE)) %>% 
             mutate(mean = wmean/sum(wmean)*100)

## option old

# Subset data
# if (region_to_report == "RAF") dat <- dat[dat$FAOST_CODE %in% 5100,]
# if (region_to_report == "RAP") dat <- dat[dat$FAOST_CODE %in% 5500,]
# if (region_to_report == "REU") dat <- dat[dat$FAOST_CODE %in% 5400,]
# if (region_to_report == "RNE") dat <- dat[dat$FAOST_CODE %in% 5300,]
# if (region_to_report == "GLO") dat <- dat[dat$FAOST_CODE %in% 5000,]
# if (region_to_report == "COF") dat <- dat[dat$FAOST_CODE %in% 5000,]

dat_plot <- dat_s  %>% mutate(sum = sum(mean)) 

p <- ggplot(dat_plot, aes(x=sum/2, y = mean, fill = var, width = sum))
p <- p + geom_bar(position="fill", stat="identity") 
p <- p + coord_polar("y")
p <- p + theme_minimal()
p <- p + theme(legend.position = "right")
p <- p + theme(text = element_text(size=11, family="PT Sans"))
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid.minor = element_blank())
p <- p + theme(panel.grid.major = element_blank())
p <- p + scale_fill_manual(values=rev(colPart5$Sub))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.key.height = unit(7, "mm"))
p <- p + theme(legend.key.width = unit(3, "mm"))
p <- p + theme(panel.grid=element_blank(), panel.border=element_blank())
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(plot.margin=unit(c(0,0,0,0),"mm"))
p
```

![plot of chunk P5desTOPRIGHT](figure/P5desTOPRIGHT-1.png) 

```r
# Caption
caption_text <- "Share of dietary energy supply, kcal/capita/day (2009-2011)"
 # --------------------------------------------------------------- #
```

</br> <p class='caption'>Share of dietary energy supply, kcal/capita/day (2009-2011)</p>


```r
# data

dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PCS.PDES.KCD3D)
top2015 <- dat %>% slice(1:20) %>% mutate(color = "2015")
top2000 <- dat %>% filter(FAOST_CODE %in% top2015$FAOST_CODE, Year == 2000) %>% mutate(color = "2000")
dat_plot <- rbind(top2015,top2000)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FBS.PCS.PDES.KCD3D),y=FBS.PCS.PDES.KCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p
```

![plot of chunk P5desLEFT](figure/P5desLEFT-1.png) 

```r
# Caption
caption_text <- "Dietary energy supply, top 20 countries in 2015"
```

</br> <p class='caption'>Dietary energy supply, top 20 countries in 2015</p>


```r
dat <- df[df$Year %in%  c(2000,2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, FBS.PCS.PDES.KCD3D)
bottom2015 <- dat %>% slice(1:20) %>% mutate(color = "2015")
bottom2000 <- dat %>% filter(FAOST_CODE %in% bottom2015$FAOST_CODE, Year == 2000) %>% mutate(color = "2000")
dat_plot <- rbind(bottom2015,bottom2000)

p <- ggplot(dat_plot, aes(x=reorder(SHORT_NAME, FBS.PCS.PDES.KCD3D),y=FBS.PCS.PDES.KCD3D))
p <- p + geom_point(aes(color=color),size = 3, alpha = 0.75)
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 2)[["Sub"]])
p <- p + coord_flip()
p <- p + labs(x="",y="kcal/cap/day")
p <- p + guides(color = guide_legend(nrow = 1))
p
```

![plot of chunk P5desRIGHT](figure/P5desRIGHT-1.png) 

```r
# Caption
caption_text <- "Dietary energy supply, bottom 20 countries in 2015"
```

</br> <p class='caption'>Dietary energy supply, bottom 20 countries in 2015</p>



```r
dat <- df[df$Year %in%  c(2000:2015) & df$FAOST_CODE < 5000,c("FAOST_CODE","Year","FAO_TABLE_NAME","FBS.PCS.PDES.KCD3D")]

dat <- dat[!is.na(dat$FBS.PCS.PDES.KCD3D),]
# Add region key and subset
dat <- left_join(dat,region_key)

dat <- dat[dat$FAOST_CODE != 348,]
dat$SHORT_NAME[dat$FAOST_CODE == 351] <- "China"

#dat <- dat[which(dat[[region_to_report]]),]

dat <- arrange(dat, -Year, -FBS.PCS.PDES.KCD3D)
top5_FAOST_CODE <- head(dat$FAOST_CODE, 5)
dat_plot <- dat %>%  filter(FAOST_CODE %in% top5_FAOST_CODE)


p <- ggplot(dat_plot, aes(x=Year,y=FBS.PCS.PDES.KCD3D,color=SHORT_NAME))
p <- p + geom_point() + geom_line()
p <- p + scale_color_manual(values=plot_colors(part = syb_part, 5)[["Sub"]])
p <- p + labs(x="",y="kcal/cap/day")
p
```

![plot of chunk P5desBOTTOM](figure/P5desBOTTOM-1.png) 

```r
# Caption
caption_text <- "Dietary energy supply"
#if (region_to_report == "COF") dat <- dat[dat$FAOST_CODE == 5000,]
 # --------------------------------------------------------------- #
```

</br> <p class='caption'>Dietary energy supply</p>



```r
dat <- df[df$Year %in%  2015 & df$FAOST_CODE < 5000,c("Year","FAOST_CODE","FS.DA.ADESA.PCT3D")]

dat <- dat[dat$FAOST_CODE != 41,]
dat$FAOST_CODE[dat$FAOST_CODE == 351] <- 41

map.plot <- left_join(dat,map.df)

# Add region key and subset

# map.plot <- map.plot[which(map.plot[[region_to_report]]),]


cat_data <- map.plot[!duplicated(map.plot[c("FAOST_CODE")]),c("FAOST_CODE","FS.DA.ADESA.PCT3D")]
cat_data$value_cat <- categories(x=cat_data$FS.DA.ADESA.PCT3D, n=5, manual=FALSE, method="jenks") # manualBreaks = c(0, 5, 15, 25, 35, 100),

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

![plot of chunk P5desMAP](figure/P5desMAP-1.png) 

```r
# Caption
 caption_text <- "This is the default caption if no region spesific is defined"
```

</br> <p class='caption'>This is the default caption if no region spesific is defined</p>

library(knitr)
options(scipen = 999)

#pdf.options(useDingbats = TRUE)

# knit_hooks$set(chunk = function(x, options) x) # do not wrap output in kframe
#
# syb_pdf = function(file, width, height) {
#   pdf(file, width = width, height = height, pointsize = 10, family = "PT Sans")
# }


opts_chunk$set(list(echo=TRUE,
                    eval=TRUE,
                    cache=FALSE,
                    warning=FALSE,
                    fig.caption = TRUE,
                    message=FALSE,
                    cache.path="~/fao_cache/regional15_web/",
                    dev="png",
                    results='asis')
)

# Why we have to short_text_ChartPage and short_text_ChartPage2 hooks etc......

knit_hooks$set(short_text_ChartPage = function(before, options, envir) {

  # challenge with hooks was that when eval=FALSE hook was still executed and produced error
  # Added the following line for each hook that stops the function IF there is FALSE in eval either
  # written directly or passed through the spread condition region gadget thing..

  if (options$short_text_ChartPage == FALSE) return()
  # Set fig size
  fig.width  =  3
  fig.height =  10
  if (!before) {
    return(paste0("<h1> ",spread_title," </h1> \n",
                  "<p>",short_text,"</p> \n"))
  }
})

knit_hooks$set(short_text_ChartPage2 = function(before, options, envir) {

  # challenge with hooks was that when eval=FALSE hook was still executed and produced error
  # Added the following line for each hook that stops the function IF there is FALSE in eval either
  # written directly or passed through the spread condition region gadget thing..

  if (options$short_text_ChartPage2 == FALSE) return()
  # Set fig size
  fig.width  =  3
  fig.height =  10
  if (!before) {
    return(paste0("<h1> ",spread_title," </h1> \n",
                  "<p>",short_text,"</p> \n"))
  }
})
#
#
knit_hooks$set(top_right_plot = function(before, options, envir) {

  if (options$top_right_plot == FALSE) return()

  if (before) {
    #return("")
  } else {

    return(paste0("</br> <p class='caption'>",caption_text,"</p>"))
  }
})
#
#
knit_hooks$set(top_right_minitable = function(before, options, envir) {

  if (options$top_right_minitable == FALSE) return()

  if (before) {
    #return("\\RightText{ \n")
  } else {

    return(paste0("</br> <p class='caption'>",caption_text,"</p>"))
  }
})
#
#
#
knit_hooks$set(left_plot = function(before, options, envir) {

  if (options$left_plot == FALSE) return()
  if (before) {
    #return("\\LeftChart{\\begin{chart} \n")
  } else {

    return(paste0("</br> <p class='caption'>",caption_text,"</p>"))
  }
})
#
knit_hooks$set(right_plot = function(before, options, envir) {

  if (options$right_plot == FALSE) return()
  if (before) {
    #return("\\RightChart{\\begin{chart} \n")
  } else {

    return(paste0("</br> <p class='caption'>",caption_text,"</p>"))
  }
})
#
knit_hooks$set(bottom_plot = function(before, options, envir) {

  if (options$bottom_plot == FALSE) return()
  if (before) {
    #return("\\BottomChart{\\begin{chart} \n")
  } else {

    return(paste0("</br> <p class='caption'>",caption_text,"</p>"))
    # return(paste0("\\end{chart}} \n \\end{ChartPage}"))
  }
})
#
#
# knit_hooks$set(bottom_plot2 = function(before, options, envir) {
#
#   if (options$bottom_plot2 == FALSE) return()
#   if (before) {
#     return("\\BottomChart{\\begin{chart} \n")
#   } else {
#
#     return(paste0("\\caption{",caption_text,"}","\\end{chart}} \n \\end{ChartPage2}"))
#     #return(paste0("\\end{chart}} \n \\end{ChartPage2}"))
#   }
# })
#
#
#
# # knit_hooks$set(end_chunk = function(before, options, envir) {
# #
# #      if (!before) {
# #        return("\\end{ChartPage}")
# #     }
# #
# # })
#
knit_hooks$set(map_plot = function(before, options, envir) {

  if (options$map_plot == FALSE) return()
  if (before) {
    #return("\\begin{figure} \n")
  } else {

    return(paste0("</br> <p class='caption'>",caption_text,"</p>"))
  }

})
#
# # remove all the kframe tags from all chunk outputs (as we are placing outputs in custom environments already!)
# local({
#   hook_chunk = knit_hooks$get('chunk')
#   knit_hooks$set(chunk = function(x, options) {
#     x = hook_chunk(x, options)
#     #if (options$results == 'asis' && !options$echo && options$fig.num == 0) {
#     if (options$results == 'asis') {
#       # remove all kframe's
#       gsub('\\\\(begin|end)\\{kframe\\}', '', x)
#     } else x
#   })
# })
#
#
#
#
#
#
#
#
#
#
#
#
#
#

top_right_plot_height <- 6
top_right_plot_width  <- 10

left_plot_height  <- 10
left_plot_width  <- 6

right_plot_height <- 10
right_plot_width <- 6

bottom_plot_height <- 6
bottom_plot_width  <- 10

# defaults
map_plot_width   <- 15
map_plot_height  <- 10
map.out.height  <- NULL
map.out.width   <- NULL
map.out.extra   <- NULL


# RAF
if (region_to_report == "RAF") map.fig.width  <- 6
if (region_to_report == "RAF") map.fig.height  <- 8
if (region_to_report == "RAF") map.out.height  <- "1.5\\columnwidth"
if (region_to_report == "RAF") map.out.extra  <- 'angle=0'

# RAP
if (region_to_report == "RAP") map.fig.width  <- 12
if (region_to_report == "RAP") map.fig.height  <- 8
if (region_to_report == "RAP") map.out.width  <- "1.7\\columnwidth"
if (region_to_report == "RAP") map.out.extra  <- 'angle=90'

# REU
if (region_to_report == "REU") map.fig.width  <- 12
if (region_to_report == "REU") map.fig.height  <- 6
if (region_to_report == "REU") map.out.width  <- "2\\columnwidth"
if (region_to_report == "REU") map.out.extra  <- 'angle=90'

# RNE
if (region_to_report == "RNE") map.fig.width  <- 7
if (region_to_report == "RNE") map.fig.height  <- 4
if (region_to_report == "RNE") map.out.height  <- ".6\\columnwidth"
if (region_to_report == "RNE") map.out.extra  <- 'angle=0'

# LAC
if (region_to_report == "LAC") map.fig.width  <- 8
if (region_to_report == "LAC") map.fig.height  <- 12
if (region_to_report == "LAC") map.out.width  <- "1.2\\columnwidth"
if (region_to_report == "LAC") map.out.extra  <- 'angle=0'


# GLO
if (region_to_report == "GLO") map.fig.width  <- 12
if (region_to_report == "GLO") map.fig.height  <- 8
if (region_to_report == "GLO") map.out.width  <- "1.7\\columnwidth"
if (region_to_report == "GLO") map.out.extra  <- 'angle=90'

# COF
if (region_to_report == "COF") map.fig.width  <- 12
if (region_to_report == "COF") map.fig.height  <- 8
if (region_to_report == "COF") map.out.width  <- "1.7\\columnwidth"
if (region_to_report == "COF") map.out.extra  <- 'angle=90'

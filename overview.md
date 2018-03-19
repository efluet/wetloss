R Markdown
----------

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

    # PLOT - wetland & peatland area 
    #source('./plots/lin_plot_stocker_sum_wet_peat_area.r')


    img1_path <- "C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/output/figures/line_plot_sum_nat_wet_20th.png"
    img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
    include_graphics(img1_path)

<img src="C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/output/figures/line_plot_sum_nat_wet_20th.png" width="50%" />

Including Plots
---------------

You can also embed plots, for example:

<img src="C:/Users/efluet/Dropbox/Chap3_holocene_global_wetland_loss/output/figures/gif/wetloss.gif" width="50%" />

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

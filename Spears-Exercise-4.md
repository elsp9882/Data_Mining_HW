    library(tidyverse)

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    library(LICORS)
    library(foreach)

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    library(mosaic)

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
    ##     quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

    library("RColorBrewer")
    library(wesanderson)
    library(janitor)

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

    library(kableExtra)

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

    library(jtools)
    library(arules)

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:mosaic':
    ## 
    ##     inspect, lhs, rhs

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

    library(arulesViz)

# Question 1

Here we aim to find out if any useful information about wine quality and
wine color can be extrapolated by the tools of Principal Components
Analysis and Kmeans++. We have data on 6497 different bottles of wine
from vinho verde vineyards. We start with Kmeans++ and use it to
segregate wines based upon pH levels and color. Then we use it to
cluster based upon quality. After that we use PCA in order to test
whether it is a better gauge of the relevant relationships. We find that
Kmeans++ provides us with more suitable plots for our purposes with this
data set.

    wine<- read.csv("wine.csv")

    X = wine%>%
      select(pH:alcohol)%>%
      scale

    colorz = wine$color %>% unique() %>% length()
    clust1 = kmeanspp(X, colorz, nstart = 25)


    data.frame(cluster = clust1$cluster,
               color = wine$color) %>% 
      count(cluster,color) %>% ggplot +
      geom_histogram(aes(x = factor(cluster), y = n, fill = color),
               stat = "identity") +
      labs(x = "Cluster", y = "By Wine Color", title = "Histogram of Clusters Delineated by Color and pH Level")

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-2-1.png)

We’ve sorted two clusters that provide us with information on how the
different wines are classified when clustered by pH levels. White wine
clearly dominates in cluster two. Cluster two probably consists of the
less acidic wines, since pH level are higher in red wines, and pH levels
are a measure of acidity.

    qualz <- wine$quality %>% unique() %>% length()
    clust2 <- kmeanspp(X, qualz, nstart = 25)
    SP2<-data.frame(cluster = clust2$cluster, quality = wine$quality) %>% 
      count(cluster,quality) %>% ggplot +
      geom_histogram(aes(x = factor(cluster), y = n, fill = quality), stat = "identity") + 
      labs(x = "Cluster", y = "Wine Quality", title = "Histogram of Clusters Delineated by Quality")

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

    SP2+scale_fill_gradient(low="blue", high="red")

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-3-1.png)
Again, we have a pretty go segmentation of wines based upon their
quality. We have seven clusters. Just by eyeballing, the sixth cluster
seems to have the most wins in the 8-9 rating range. Hence, whatever
features are used to categorize cluster six would be worth noting to
better understand what creates a higher quality wine.

    PCA_frame <- wine %>% 
      select(pH:alcohol) %>% 
      prcomp(rank=2,
             center=T,
             scale=T)
    PCA_frame$x %>% 
      as.data.frame %>% 
      cbind(wine %>% select(color)) %>%
      ggplot +
      geom_point(aes(x = PC1, y = PC2, color = color))+
      labs(title = "PCA Plot of Wine Colors")

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-4-1.png)
Although there is some differentiation here in terms of the red cluster
and the white cluster, this plot is virtually useless as there is
tremendous overlap between the two right in the middle of the plot.

    PCA_frame$x %>% 
      as.data.frame %>% 
      cbind(wine %>% select(quality)) %>% 
      ggplot + 
      geom_point(aes(x = PC1, y = PC2, color = quality))+
      labs(title = "PCA Plot of Quality")

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Once again, this graph isn’t very helpful in enabling us to decipher
anything. The pH levels are what we are using to categorize the PC
groupings of wine, but it’s difficult to extrapolate anything of
substance from this plot about the relationship between pH levels and
wine quality. Overall, it seems that kmeans++ did a better job of
providing us with useful plots and information about the relationship
between pH levels, wine quality, and color.

# Question 2

The purpose of this next analysis is to provide a report for
NutrientH20, which identifies any interesting market segments that
appear to stand out in their social-media audience.

    mapz(PC1)

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-7-1.png)
Here we see that variables like spam, adult, and online\_gaming are on
the opposite side of the PC spectrum from variables like religion,
parenting, and school. It’s difficult to gauge what aspect of the data
generated this spread, but perhaps “shareable content for the average
user” would be a decent approximation of what created this segmentation.

    mapz(PC2)

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-8-1.png)
In this second PCA plot we see that seemingly unrelated categories
occupy spaces much closer to one another. Religion and adult are both on
the positive end of the spectrum. What could the “ingredient” makeup of
this plot be? Well, on the negative side we have categories like:
cooking, photo\_sharing, shopping, fashion, and beauty. On the positive
side we have categories like: religion, sports\_fandom, school, news,
and parenting. Perhaps the level of controversy that these tweets stirs
up is contributing to the PCA mapping is organized here.

    mapz(PC3)

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-9-1.png)
I wanted to produce one more PCA plot. PC3 seems to have more stale
topics on the bottom end of the spectrum, whereas more invigorating and
fluffy categories makeup the top rungs.

    model<- lm(religion~family+politics+news+current_events+school+parenting, data=social)

    summ(model)

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
7882
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
religion
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
OLS linear regression
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
F(6,7875)
</td>
<td style="text-align:right;">
1347.58
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
R²
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Adj. R²
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Est.
</th>
<th style="text-align:right;">
S.E.
</th>
<th style="text-align:right;">
t val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
-0.00
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.18
</td>
<td style="text-align:right;">
0.86
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
family
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
20.80
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
politics
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
3.17
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
news
</td>
<td style="text-align:right;">
-0.06
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-6.39
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
current\_events
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
2.39
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
school
</td>
<td style="text-align:right;">
0.36
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
23.82
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
parenting
</td>
<td style="text-align:right;">
0.59
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
48.90
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: OLS
</td>
</tr>
</tfoot>
</table>

Here I created a linear model to try and predict the expected number of
posts about, arguably, the most controversial topic in the data set,
using the other most controversial topics in the data set. Everything
with the exception of the “news” category had a positive effect on the
predicted value of number of tweets about religion that a particular
subject puts out.

    model2<- lm(adult~ .-X, data=social)

    summ(model2)

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
7882
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
adult
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
OLS linear regression
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
F(35,7846)
</td>
<td style="text-align:right;">
31.44
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
R²
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Adj. R²
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Est.
</th>
<th style="text-align:right;">
S.E.
</th>
<th style="text-align:right;">
t val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
3.25
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
chatter
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
1.85
</td>
<td style="text-align:right;">
0.06
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
current\_events
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
0.32
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
travel
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
3.78
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
photo\_sharing
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-3.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
uncategorized
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
3.73
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
tv\_film
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-3.60
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
sports\_fandom
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-2.53
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
politics
</td>
<td style="text-align:right;">
-0.07
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-6.77
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
food
</td>
<td style="text-align:right;">
-0.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.99
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
family
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
2.60
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
home\_and\_garden
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
music
</td>
<td style="text-align:right;">
-0.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-0.16
</td>
<td style="text-align:right;">
0.87
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
news
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-1.74
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
online\_gaming
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
2.92
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
shopping
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-2.40
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
health\_nutrition
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-4.89
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
college\_uni
</td>
<td style="text-align:right;">
-0.04
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-3.29
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
sports\_playing
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-1.21
</td>
<td style="text-align:right;">
0.23
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
cooking
</td>
<td style="text-align:right;">
-0.01
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-1.22
</td>
<td style="text-align:right;">
0.22
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
eco
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
4.48
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
computers
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
business
</td>
<td style="text-align:right;">
-0.02
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
-0.68
</td>
<td style="text-align:right;">
0.50
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
outdoors
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
8.37
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
crafts
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
1.52
</td>
<td style="text-align:right;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
automotive
</td>
<td style="text-align:right;">
0.09
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
5.09
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
art
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
1.38
</td>
<td style="text-align:right;">
0.17
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
religion
</td>
<td style="text-align:right;">
-0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
-3.44
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
beauty
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.36
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
parenting
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
dating
</td>
<td style="text-align:right;">
-0.03
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
-2.43
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
school
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
2.37
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
personal\_fitness
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.76
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
fashion
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.96
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
small\_business
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
7.19
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
spam
</td>
<td style="text-align:right;">
6.15
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
26.59
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: OLS
</td>
</tr>
</tfoot>
</table>

I thought it might be of interest which variables do the most to help
predict the amount of posts an account makes for the “adult” category.
It appears that “spam” is the strongest predictor, which isn’t
surprising since spam and adult content are very closely intertwined.
Interestingly, the outdoors category has a fairly large and positive
predictive value at the 5% level for the predicted number of adult
tweets. Since posts with “adult” content probably belong to a certain
class of user, it might be helpful for NutrientH20 to know how to target
that market using this data.

# Question 3

The objective of the next case study is to assist a grocery store in
finding some interesting association rules through shopping trends based
upon customer basket data. We will help the grocery store see which
items are usually purchased in tandem with one another in order to
provide the grocer with the necessary information to better organize the
way in which they stock their shelves, putting food items in close
proximity to each other than are demonstrated through this data to be
purchased.

    plot(head(sub1, 100, by='lift'), method='graph')

    ## Warning: ggrepel: 5 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](Spears-Exercise-4_files/figure-markdown_strict/unnamed-chunk-13-1.png)
Here we see some examples of items that tend to be purchased together.
This can be assessed by proximity and the direction of the vectors. For
instance, we see baby food and pot plants are often purchased together.
It might not make sense to put these items in the same part of the
store, but it is useful information, and perhaps some advertising flyers
could be posted in the baby food aisle to target that sort of consumer.

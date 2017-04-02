LONG TERM IMPACTS OF BIOSOLIDS ON MWD
-------------------------------------

### Explore the data

    library("ggplot2")
    library(lme4)
    library(reshape2)
    library(MASS)
    library(dplyr)
    library(base)

    soil<-read.table("MWD.csv", sep = ",", header = T) 
    head(soil)

    ##   Sample  Date Block Treatment Transect  MWD
    ## 1      1 April    b1       bio        1 1.05
    ## 2      2 April    b1       bio        2 0.95
    ## 3      3 April    b1       bio        3 0.99
    ## 4      4 April    b3       bio        1 0.82
    ## 5      5 April    b3       bio        2 0.84
    ## 6      6 April    b3       bio        3 0.96

    str(soil)

    ## 'data.frame':    96 obs. of  6 variables:
    ##  $ Sample   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Date     : Factor w/ 4 levels "April","Aug",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Block    : Factor w/ 4 levels "b1","b2","b3",..: 1 1 1 3 3 3 2 2 2 4 ...
    ##  $ Treatment: Factor w/ 2 levels "bio","con": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Transect : int  1 2 3 1 2 3 1 2 3 1 ...
    ##  $ MWD      : num  1.05 0.95 0.99 0.82 0.84 0.96 1.11 1.25 1.11 1.23 ...

    with(soil,table(Treatment, Date))

    ##          Date
    ## Treatment April Aug June Oct
    ##       bio    12  12   12  12
    ##       con    12  12   12  12

    group_by(soil,Date,Treatment) %>%
      summarise(mean(MWD))

    ## Source: local data frame [8 x 3]
    ## Groups: Date [?]
    ## 
    ##     Date Treatment `mean(MWD)`
    ##   <fctr>    <fctr>       <dbl>
    ## 1  April       bio    1.051667
    ## 2  April       con    0.880000
    ## 3    Aug       bio    1.780000
    ## 4    Aug       con    1.550833
    ## 5   June       bio    1.741667
    ## 6   June       con    1.718333
    ## 7    Oct       bio    1.667500
    ## 8    Oct       con    1.255833

### Looking and understanding the data

> S550: Great exploratory analysis. But what is the motivation behind
> the qqnorm plot? It might not be required for the model we are using.
> Also, what information does the acf() plot give us? What do you want
> to show by acf(soil)?

> S550: Some comments above chunks of code would be useful, to state
> what some of the lesser-known functions do.

> S550: What you get by using this plot design?

    hist(soil$MWD, xlab="MWD", main = "Histogram of MWD")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    ggplot(aes(MWD, fill = Treatment, alpha = 0.4), data=soil) + geom_density() + facet_wrap(~Date)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    ggplot(aes(y=MWD, x=Treatment, fill = Treatment, alpha = 0.4), data=soil) + geom_boxplot() + geom_point() + facet_wrap(~Date)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    tapply(soil$MWD, soil$Treatment, mean)

    ##      bio      con 
    ## 1.560208 1.351250

    tapply(soil$MWD, soil$Treatment, sd)

    ##       bio       con 
    ## 0.3874274 0.3616547

    qqnorm(soil$MWD)
    qqline(soil$MWD)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-4.png)

    acf(soil)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-5.png)

    Transect.f<-as.factor(soil$Transect)
    plot.design(MWD~Treatment+Block+Date+Transect.f, data = soil, xlab="Treatment", ylab="MWD")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-6.png)

    Date.f = factor(soil$Date,levels(soil$Date)[c(1, 3, 2, 4)])
    ggplot(soil, aes(x = Date.f, y = MWD, group = Treatment, colour = Treatment)) +
      stat_summary(fun.y="mean", geom = "line") +
      labs(x = "Date", title = "Change in MWD over 4 Sampling Dates")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-7.png)

    interaction.plot(soil$Block,soil$Treatment,soil$MWD)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-8.png)

### Analysis

A simple model to start with:  
- Randomized complete block design (but block effect as random)  
- Ignore the transects and repeated measurements for now  
- Treat the transects as pseudo-replication

    soil.rcb1<-lmer(MWD~Treatment+(1|Block), soil)  
    summary(soil.rcb1)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Treatment + (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: 89.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8460 -0.8817  0.1860  0.6827  2.1580 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.004282 0.06544 
    ##  Residual             0.137167 0.37036 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   1.56021    0.06268  24.894
    ## Treatmentcon -0.20896    0.07560  -2.764
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Treatmentcn -0.603

    plot(soil.rcb1)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    soil.rcb2<-lmer(MWD~(1|Block), soil)
    summary(soil.rcb2)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: 93.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0288 -0.7462  0.1153  0.7449  2.3642 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.00387  0.06221 
    ##  Residual             0.14707  0.38349 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  1.45573    0.04999   29.12

    plot(soil.rcb2)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-4-2.png)

    anova(soil.rcb1,soil.rcb2)

    ## refitting model(s) with ML (instead of REML)

    ## Data: soil
    ## Models:
    ## soil.rcb2: MWD ~ (1 | Block)
    ## soil.rcb1: MWD ~ Treatment + (1 | Block)
    ##           Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
    ## soil.rcb2  3 95.224 102.92 -44.612   89.224                            
    ## soil.rcb1  4 89.807 100.06 -40.904   81.807 7.4166      1   0.006463 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Add Date to the model

    soil.date<-lmer(MWD~Treatment+Date+(1|Block), soil)
    summary(soil.date)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Treatment + Date + (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -5.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9440 -0.5482  0.0974  0.6313  2.3773 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.008169 0.09038 
    ##  Residual             0.043872 0.20946 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   1.07031    0.06578  16.270
    ## Treatmentcon -0.20896    0.04276  -4.887
    ## DateAug       0.69958    0.06047  11.570
    ## DateJune      0.76417    0.06047  12.638
    ## DateOct       0.49583    0.06047   8.200
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Trtmnt DateAg DateJn
    ## Treatmentcn -0.325                     
    ## DateAug     -0.460  0.000              
    ## DateJune    -0.460  0.000  0.500       
    ## DateOct     -0.460  0.000  0.500  0.500

    plot(soil.date)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    anova(soil.rcb1,soil.date)

    ## refitting model(s) with ML (instead of REML)

    ## Data: soil
    ## Models:
    ## soil.rcb1: MWD ~ Treatment + (1 | Block)
    ## soil.date: MWD ~ Treatment + Date + (1 | Block)
    ##           Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## soil.rcb1  4  89.807 100.065 -40.904   81.807                             
    ## soil.date  7 -12.149   5.802  13.074  -26.149 107.96      3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Add interaction effect

    soil$Treatment <- relevel(soil$Treatment, ref = "con")
    soil.mult<-lmer(MWD~Treatment*Date+(1|Block), soil)
    summary(soil.mult)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Treatment * Date + (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -8.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6096 -0.5821 -0.0017  0.6172  2.9457 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.008332 0.09128 
    ##  Residual             0.039961 0.19990 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error t value
    ## (Intercept)            0.88000    0.07357  11.961
    ## Treatmentbio           0.17167    0.08161   2.104
    ## DateAug                0.67083    0.08161   8.220
    ## DateJune               0.83833    0.08161  10.272
    ## DateOct                0.37583    0.08161   4.605
    ## Treatmentbio:DateAug   0.05750    0.11541   0.498
    ## Treatmentbio:DateJune -0.14833    0.11541  -1.285
    ## Treatmentbio:DateOct   0.24000    0.11541   2.079
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Trtmnt DateAg DateJn DatOct Trt:DA Trt:DJ
    ## Treatmentbi -0.555                                          
    ## DateAug     -0.555  0.500                                   
    ## DateJune    -0.555  0.500  0.500                            
    ## DateOct     -0.555  0.500  0.500  0.500                     
    ## Trtmntb:DtA  0.392 -0.707 -0.707 -0.354 -0.354              
    ## Trtmntb:DtJ  0.392 -0.707 -0.354 -0.707 -0.354  0.500       
    ## Trtmntb:DtO  0.392 -0.707 -0.354 -0.354 -0.707  0.500  0.500

    plot(soil.mult)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    anova(soil.date,soil.mult)

    ## refitting model(s) with ML (instead of REML)

    ## Data: soil
    ## Models:
    ## soil.date: MWD ~ Treatment + Date + (1 | Block)
    ## soil.mult: MWD ~ Treatment * Date + (1 | Block)
    ##           Df     AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
    ## soil.date  7 -12.149 5.8017 13.074  -26.149                            
    ## soil.mult 10 -17.931 7.7128 18.965  -37.931 11.782      3   0.008169 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Bootstrapping for confidence interval of coefficient

    b_fit <- list()
    for(i in 1:100) {
      b_sample <- dplyr::sample_n(soil, dim(soil)[1], replace = TRUE)
      b_coefs <- fixef(lmer(MWD~Treatment*Date+(1|Block), data = b_sample))
      b_fit[[i]] <- b_coefs
    }

    b_all_coef <- plyr::ldply(b_fit)
    sort(b_all_coef$Treatmentbio)

    ##   [1] 0.02285479 0.06271414 0.06626541 0.07466554 0.07849893 0.08452669
    ##   [7] 0.08579166 0.09103490 0.09345112 0.09667854 0.10125575 0.10670473
    ##  [13] 0.10688902 0.11145333 0.11225532 0.11242346 0.11281297 0.11729556
    ##  [19] 0.12263831 0.12445704 0.12615270 0.13116237 0.13140831 0.13161362
    ##  [25] 0.13301301 0.13585709 0.13664460 0.13703490 0.14222947 0.14464548
    ##  [31] 0.14592958 0.14790274 0.14884849 0.14984358 0.15091606 0.15554840
    ##  [37] 0.15624440 0.15654428 0.15823670 0.15928889 0.15969011 0.15997407
    ##  [43] 0.16076247 0.16324419 0.16326457 0.16400823 0.16763563 0.16821328
    ##  [49] 0.16869418 0.16980140 0.17076227 0.17173222 0.17218105 0.17374507
    ##  [55] 0.17403184 0.17495714 0.17605731 0.17673447 0.17878493 0.18269282
    ##  [61] 0.18277482 0.18448050 0.18613295 0.18623180 0.18907731 0.19111959
    ##  [67] 0.19140889 0.19551745 0.19906410 0.20060480 0.20468475 0.20669001
    ##  [73] 0.20797368 0.21072845 0.21117101 0.21831670 0.21859786 0.21901716
    ##  [79] 0.22049003 0.22222115 0.22402199 0.22471412 0.22909972 0.23186981
    ##  [85] 0.23395520 0.23405406 0.23423047 0.24208920 0.24539238 0.24558740
    ##  [91] 0.24886490 0.25184525 0.25391356 0.26694191 0.26784979 0.27908237
    ##  [97] 0.28694308 0.28887390 0.29442407 0.30744185

    # estimate with 95% Confidence interval is: 0.172 (0.054, 0.261) <- fix date in April
    sort(b_all_coef$DateJune)

    ##   [1] 0.7473638 0.7490303 0.7492238 0.7493818 0.7494323 0.7511714 0.7524007
    ##   [8] 0.7613956 0.7641358 0.7659098 0.7698785 0.7707112 0.7722972 0.7732972
    ##  [15] 0.7738997 0.7831186 0.7877581 0.7905079 0.7905294 0.7905564 0.7906000
    ##  [22] 0.7912203 0.7927874 0.7929811 0.7942105 0.7960666 0.7968968 0.7988416
    ##  [29] 0.7996804 0.8018073 0.8059027 0.8082343 0.8091062 0.8096079 0.8098503
    ##  [36] 0.8149494 0.8195483 0.8199541 0.8221952 0.8245142 0.8246118 0.8250987
    ##  [43] 0.8283811 0.8285365 0.8308272 0.8319858 0.8326788 0.8326968 0.8328728
    ##  [50] 0.8366004 0.8371731 0.8425431 0.8429293 0.8459157 0.8460966 0.8493188
    ##  [57] 0.8513793 0.8530460 0.8530980 0.8564172 0.8583128 0.8600037 0.8602952
    ##  [64] 0.8608137 0.8616272 0.8643055 0.8683174 0.8691148 0.8797361 0.8812231
    ##  [71] 0.8833531 0.8847381 0.8854559 0.8858295 0.8862393 0.8875192 0.8884649
    ##  [78] 0.8903407 0.8962859 0.9005152 0.9009613 0.9044398 0.9051590 0.9065641
    ##  [85] 0.9084674 0.9111438 0.9121554 0.9132701 0.9156382 0.9231342 0.9231821
    ##  [92] 0.9245615 0.9322976 0.9462088 0.9500359 0.9506472 0.9600922 0.9720314
    ##  [99] 0.9807653 0.9852560

    # estimate with 95% Confidence interval is: 0.838 (0.719, 0.956)
    sort(b_all_coef$`Treatmentbio:DateOct`)

    ##   [1] 0.01722960 0.04948789 0.06129530 0.06668937 0.08307105 0.08837219
    ##   [7] 0.09651327 0.10304392 0.10634853 0.10762804 0.11475013 0.12092682
    ##  [13] 0.13223132 0.13416471 0.13821702 0.14890867 0.15287758 0.15526115
    ##  [19] 0.16227755 0.16238547 0.16336995 0.16345269 0.16366834 0.16906555
    ##  [25] 0.17483425 0.17896147 0.18156733 0.18306792 0.18573659 0.18688989
    ##  [31] 0.18799608 0.18823881 0.18926336 0.18941688 0.19027859 0.19641472
    ##  [37] 0.19932969 0.20033686 0.20336592 0.21579515 0.22182028 0.22285681
    ##  [43] 0.22496931 0.22554670 0.22666252 0.22672420 0.22725908 0.22752670
    ##  [49] 0.22884446 0.22998451 0.23305928 0.23425722 0.23672539 0.23727443
    ##  [55] 0.23887669 0.23910308 0.24274498 0.24470162 0.24903971 0.25140974
    ##  [61] 0.25437817 0.25938144 0.26117492 0.26174434 0.26720658 0.27523770
    ##  [67] 0.27812469 0.27981879 0.28079933 0.28260611 0.28442710 0.28568115
    ##  [73] 0.28595436 0.28625146 0.28663073 0.28806795 0.28823375 0.30140631
    ##  [79] 0.30442167 0.30508404 0.30607775 0.30754595 0.30900384 0.31583499
    ##  [85] 0.32070347 0.32172142 0.32861893 0.32981815 0.33039047 0.33308782
    ##  [91] 0.34754326 0.35203154 0.35617383 0.37584070 0.37640509 0.40750828
    ##  [97] 0.41082722 0.42276436 0.46807503 0.49539710

    # estimate with 95% Confidence interval is: 0.240 (0.085, 0.409)

LONG TERM IMPACTS OF BIOSOLIDS ON PLANT COVER
---------------------------------------------

### Data Exploration

    pc <- read.csv("plant_cover.csv", header = T)
    str(pc)

    ## 'data.frame':    2669 obs. of  9 variables:
    ##  $ Project    : Factor w/ 1 level "OK Ranch Biosolids Resample": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Date       : Factor w/ 2 levels "June 21, 2016",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Block      : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Treatment  : Factor w/ 2 levels "Biosolids","Control": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Transect   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Plot       : int  2 2 2 2 2 2 2 2 2 3 ...
    ##  $ Species    : Factor w/ 50 levels "ACMI","ACRI",..: 1 3 9 11 29 33 39 47 48 3 ...
    ##  $ Cover.class: int  1 1 1 2 4 5 3 3 1 2 ...
    ##  $ Cover.value: num  2.5 2.5 2.5 15 62.5 85 37.5 37.5 2.5 15 ...

    pc$Block <- as.factor(pc$Block)

    # for whole dataset
    group_by(pc,Block,Treatment) %>%
      summarise(mean(Cover.value), sd(Cover.value))

    ## Source: local data frame [8 x 4]
    ## Groups: Block [?]
    ## 
    ##    Block Treatment `mean(Cover.value)` `sd(Cover.value)`
    ##   <fctr>    <fctr>               <dbl>             <dbl>
    ## 1      1 Biosolids            32.70764          35.28365
    ## 2      1   Control            17.30952          18.18493
    ## 3      2 Biosolids            28.90365          32.73254
    ## 4      2   Control            15.17857          17.42308
    ## 5      3 Biosolids            40.47071          38.97941
    ## 6      3   Control            21.96884          23.51373
    ## 7      4 Biosolids            33.51124          36.84526
    ## 8      4   Control            16.77356          20.23028

    table_sum <- tapply(pc$Cover.value,pc$Treatment, table)
    table_sum

    ## $Biosolids
    ## 
    ##  2.5   15 37.5 62.5   85 97.5 
    ##  405  265  111   78   91  158 
    ## 
    ## $Control
    ## 
    ##  2.5   15 37.5 62.5   85 97.5 
    ##  676  510  243   94   35    3

    ggplot(pc,aes(x=Cover.value,group=Treatment,fill=Treatment))+geom_histogram(position="dodge",binwidth=9)+theme_bw()+labs(title="Histogram of Cover Value for all Species")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    ggplot(aes(x = Block, y = Cover.value, group = Treatment, colour = Treatment), data = pc)+stat_summary(fun.y="mean", geom = "line")+labs(x = "Block", y = "Plant Cover Value", title = "Change in Cover Value over Different Blocks")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-8-2.png)

    # for subset POPR
    pc.subset <- subset(pc, Species == "POPR")

    table_sum_POPR <- tapply(pc.subset$Cover.value,pc.subset$Treatment, table)
    table_sum_POPR

    ## $Biosolids
    ## 
    ##  2.5   15 37.5 62.5   85 97.5 
    ##   19   28   20   15   16   17 
    ## 
    ## $Control
    ## 
    ## 2.5  15 
    ##   4   1

    pc.subset <- pc.subset[ ,c(3,4,9)]
    by_blockTrt <- group_by(pc.subset, Block, Treatment)
    dat.avg<-summarise(by_blockTrt, y.avg=sum(Cover.value)/50)
    add2control <- c("2","Control",0)
    dat.avg[8, ] <- add2control

    dat.avg$y.avg <- as.numeric(dat.avg$y.avg)
    ggplot(aes(x = Block, y = y.avg, group = Treatment, colour = Treatment), data = dat.avg) + geom_point() + geom_line() + labs(x = "Block", y = "Plant Cover Value", title = "Change in Cover Value of POPR over Different Blocks")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-8-3.png)

    ggplot(aes(y=y.avg, x=Treatment, fill = Treatment, alpha = 0.4), data=dat.avg) + geom_boxplot() + geom_point()+ labs(y = "POPR Cover Value", title = "Boxplot of POPR Cover Value")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-8-4.png)

### Diagnostics: checking assumptions of linear models

    plot(pc.subset$Treatment, pc.subset$Cover.value)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    # similar variability of cover value within two levels of treatment although strange behaviour of Control group

    qqnorm(pc.subset$Cover.value)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-9-2.png)

    # strange behaviour due to the fact that cover value is discrete
    hist(pc.subset$Cover.value)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-9-3.png)

    # not quite normal, heavy right tail

    ggplot(aes(y=Cover.value, x=Treatment, fill = Treatment, alpha = 0.4), data=pc.subset) + geom_boxplot() + geom_point() + facet_wrap(~Block)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-9-4.png)

    # almost same variance

    #pc_sub <- pc.subset[ ,c(3,4,5,9)]
    #acf(pc_sub)
    # no multicolinearity within explnatory variables

    # Residuals are checked in the next section after imposing linear model.

> S550: Here both cover value and cover class are categorical so what
> does linear regression and diagnostics of the linear regression
> actually give? How can we check multicolinearity using acf()?

### Analysis

    pc$Block <- as.factor(pc$Block)
    pc.subset <- subset(pc, Species == "POPR")
    pc.subset <- pc.subset[ ,c(3,4,9)]

    by_blockTrt <- group_by(pc.subset, Block, Treatment)
    dat.avg<-summarise(by_blockTrt, y.avg=sum(Cover.value)/50)
    add2control <- c("2","Control",0)
    dat.avg[8, ] <- add2control
    dat.avg

    ## Source: local data frame [8 x 3]
    ## Groups: Block [?]
    ## 
    ##    Block Treatment y.avg
    ## * <fctr>    <fctr> <chr>
    ## 1      1 Biosolids 23.45
    ## 2      1   Control  0.15
    ## 3      2 Biosolids  9.75
    ## 4      3 Biosolids 26.85
    ## 5      3   Control  0.05
    ## 6      4 Biosolids  43.4
    ## 7      4   Control   0.3
    ## 8      2   Control     0

    dat.avg$Treatment <- relevel(dat.avg$Treatment, ref = "Control")
    model_avg <- lm(as.numeric(y.avg) ~ Treatment, data = dat.avg)
    summary(model_avg)

    ## 
    ## Call:
    ## lm(formula = as.numeric(y.avg) ~ Treatment, data = dat.avg)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.1125  -0.6969  -0.0250   0.3781  17.5375 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)           0.125      4.891   0.026  0.98044   
    ## TreatmentBiosolids   25.738      6.916   3.721  0.00984 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.781 on 6 degrees of freedom
    ## Multiple R-squared:  0.6977, Adjusted R-squared:  0.6473 
    ## F-statistic: 13.85 on 1 and 6 DF,  p-value: 0.009838

    # MS(Treatment)/MS(Treatment:Block) = F statistic from lm output of adding-zero approach

### Correlation between Plant Cover and MWD

    a<-group_by(soil,Block,Treatment) %>%
      summarise(mean(MWD))
    b<-a$`mean(MWD)`
    mean.MWD<-c(b[1], b[2], b[3], b[5], b[6], b[7], b[8], b[4])
    mean.cv<-as.numeric(as.character(dat.avg$y.avg))
    cor(mean.MWD, mean.cv)

    ## [1] -0.6155239

### Appendix (model history)

#### Part 1

Changing baselines to test coefficients

    soil$Treatment <- relevel(soil$Treatment, ref = "con")
    soil$Date <- relevel(soil$Date, ref = "June")
    soil.mult<-lmer(MWD~Treatment*Date+(1|Block), soil)
    summary(soil.mult)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Treatment * Date + (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -8.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6096 -0.5821 -0.0017  0.6172  2.9457 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.008332 0.09128 
    ##  Residual             0.039961 0.19990 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error t value
    ## (Intercept)             1.71833    0.07357  23.355
    ## Treatmentbio            0.02333    0.08161   0.286
    ## DateApril              -0.83833    0.08161 -10.272
    ## DateAug                -0.16750    0.08161  -2.052
    ## DateOct                -0.46250    0.08161  -5.667
    ## Treatmentbio:DateApril  0.14833    0.11541   1.285
    ## Treatmentbio:DateAug    0.20583    0.11541   1.783
    ## Treatmentbio:DateOct    0.38833    0.11541   3.365
    ## 
    ## Correlation of Fixed Effects:
    ##              (Intr) Trtmnt DtAprl DateAg DatOct Trtmntb:DtAp Trtmntb:DtAg
    ## Treatmentbi  -0.555                                                      
    ## DateApril    -0.555  0.500                                               
    ## DateAug      -0.555  0.500  0.500                                        
    ## DateOct      -0.555  0.500  0.500  0.500                                 
    ## Trtmntb:DtAp  0.392 -0.707 -0.707 -0.354 -0.354                          
    ## Trtmntb:DtAg  0.392 -0.707 -0.354 -0.707 -0.354  0.500                   
    ## Trtmntb:DtO   0.392 -0.707 -0.354 -0.354 -0.707  0.500        0.500

    b_fit <- list()
    for(i in 1:100) {
      b_sample <- dplyr::sample_n(soil, dim(soil)[1], replace = TRUE)
      b_coefs <- fixef(lmer(MWD~Treatment*Date+(1|Block), data = b_sample))
      b_fit[[i]] <- b_coefs
    }

    b_all_coef <- plyr::ldply(b_fit)
    sort(b_all_coef$Treatmentbio)

    ##   [1] -0.2742733753 -0.2280298751 -0.2144549933 -0.1784977100 -0.1716271602
    ##   [6] -0.1468507931 -0.1444506228 -0.1379317606 -0.1358236996 -0.1337189405
    ##  [11] -0.1226902056 -0.1176048974 -0.1084486754 -0.1083874544 -0.1051056417
    ##  [16] -0.1014497489 -0.0971646127 -0.0904915203 -0.0894668152 -0.0887932031
    ##  [21] -0.0881956352 -0.0844571745 -0.0775099332 -0.0751338556 -0.0696900337
    ##  [26] -0.0666606985 -0.0649619239 -0.0560825777 -0.0558358644 -0.0542063568
    ##  [31] -0.0528828616 -0.0481943223 -0.0481025495 -0.0474012803 -0.0470532249
    ##  [36] -0.0376210747 -0.0350706830 -0.0310201256 -0.0304519247 -0.0298013111
    ##  [41] -0.0292903551 -0.0246401053 -0.0159015610 -0.0122276398 -0.0074404656
    ##  [46] -0.0023254148 -0.0014393213 -0.0005577836  0.0024550267  0.0032060359
    ##  [51]  0.0102614973  0.0123819494  0.0216054220  0.0252343632  0.0264728074
    ##  [56]  0.0291003769  0.0310573718  0.0313018208  0.0319716885  0.0372579994
    ##  [61]  0.0391477172  0.0423905798  0.0441608683  0.0450454547  0.0450941171
    ##  [66]  0.0462535711  0.0482471513  0.0605612817  0.0699390380  0.0735161537
    ##  [71]  0.0798508151  0.0804845850  0.0830445302  0.0882267672  0.0896326449
    ##  [76]  0.0902438825  0.0905168640  0.0952381140  0.1060637211  0.1069857003
    ##  [81]  0.1097255568  0.1241017269  0.1252878441  0.1260388102  0.1286785639
    ##  [86]  0.1292715393  0.1302377452  0.1510538440  0.1592892067  0.1600224620
    ##  [91]  0.1631670274  0.1637323964  0.1647522921  0.1805555045  0.2007473838
    ##  [96]  0.2028671329  0.2157192531  0.2227955306  0.2321181349  0.3593646690

    # estimate with 95% Confidence interval is: 0.023 (-0.235, 0.248) <- fix date in June

    soil$Date <- relevel(soil$Date, ref = "Aug")
    soil.mult<-lmer(MWD~Treatment*Date+(1|Block), soil)
    summary(soil.mult)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Treatment * Date + (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -8.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6096 -0.5821 -0.0017  0.6172  2.9457 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.008332 0.09128 
    ##  Residual             0.039961 0.19990 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error t value
    ## (Intercept)             1.55083    0.07357  21.078
    ## Treatmentbio            0.22917    0.08161   2.808
    ## DateJune                0.16750    0.08161   2.052
    ## DateApril              -0.67083    0.08161  -8.220
    ## DateOct                -0.29500    0.08161  -3.615
    ## Treatmentbio:DateJune  -0.20583    0.11541  -1.783
    ## Treatmentbio:DateApril -0.05750    0.11541  -0.498
    ## Treatmentbio:DateOct    0.18250    0.11541   1.581
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Trtmnt DateJn DtAprl DatOct Trt:DJ Trt:DA
    ## Treatmentbi -0.555                                          
    ## DateJune    -0.555  0.500                                   
    ## DateApril   -0.555  0.500  0.500                            
    ## DateOct     -0.555  0.500  0.500  0.500                     
    ## Trtmntb:DtJ  0.392 -0.707 -0.707 -0.354 -0.354              
    ## Trtmntb:DtA  0.392 -0.707 -0.354 -0.707 -0.354  0.500       
    ## Trtmntb:DtO  0.392 -0.707 -0.354 -0.354 -0.707  0.500  0.500

    b_fit <- list()
    for(i in 1:100) {
      b_sample <- dplyr::sample_n(soil, dim(soil)[1], replace = TRUE)
      b_coefs <- fixef(lmer(MWD~Treatment*Date+(1|Block), data = b_sample))
      b_fit[[i]] <- b_coefs
    }

    b_all_coef <- plyr::ldply(b_fit)
    sort(b_all_coef$Treatmentbio)

    ##   [1] -0.04489060  0.08428455  0.09554670  0.10771062  0.11262066
    ##   [6]  0.11620107  0.12088547  0.12419721  0.12726266  0.12952410
    ##  [11]  0.13717120  0.14101057  0.14147200  0.14365822  0.14601399
    ##  [16]  0.15080576  0.15370625  0.15601283  0.15733456  0.15833890
    ##  [21]  0.15873041  0.16026748  0.16079533  0.16219741  0.16698718
    ##  [26]  0.16793427  0.17049281  0.17467777  0.17854928  0.17976620
    ##  [31]  0.18914014  0.19016700  0.19102780  0.19238362  0.19718591
    ##  [36]  0.20156197  0.20303157  0.20312859  0.20620681  0.20825892
    ##  [41]  0.20988343  0.21284112  0.21371474  0.21529304  0.21563766
    ##  [46]  0.21582361  0.21695554  0.21875034  0.22102860  0.22347018
    ##  [51]  0.23184014  0.23186227  0.23448687  0.23830474  0.23840569
    ##  [56]  0.23942928  0.24485943  0.24498175  0.24620180  0.25013537
    ##  [61]  0.25166067  0.25419553  0.25588882  0.25611395  0.25730987
    ##  [66]  0.25782418  0.25916535  0.25977888  0.26188575  0.26245639
    ##  [71]  0.26471252  0.26618965  0.27180263  0.27500912  0.28470780
    ##  [76]  0.28961816  0.28999659  0.29095731  0.29380259  0.29499964
    ##  [81]  0.29630960  0.29768023  0.30541345  0.30771588  0.30780542
    ##  [86]  0.31089847  0.31099692  0.31519373  0.31622698  0.32381596
    ##  [91]  0.33988223  0.34586256  0.34700411  0.34721822  0.36308774
    ##  [96]  0.37885655  0.38081597  0.38719897  0.44280881  0.47687599

    # estimate with 95% Confidence interval is: 0.229 (0.058, 0.422) <- fix date in August

    soil$Date <- relevel(soil$Date, ref = "Oct")
    soil.mult<-lmer(MWD~Treatment*Date+(1|Block), soil)
    summary(soil.mult)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Treatment * Date + (1 | Block)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -8.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6096 -0.5821 -0.0017  0.6172  2.9457 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  Block    (Intercept) 0.008332 0.09128 
    ##  Residual             0.039961 0.19990 
    ## Number of obs: 96, groups:  Block, 4
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error t value
    ## (Intercept)             1.25583    0.07357  17.069
    ## Treatmentbio            0.41167    0.08161   5.044
    ## DateAug                 0.29500    0.08161   3.615
    ## DateJune                0.46250    0.08161   5.667
    ## DateApril              -0.37583    0.08161  -4.605
    ## Treatmentbio:DateAug   -0.18250    0.11541  -1.581
    ## Treatmentbio:DateJune  -0.38833    0.11541  -3.365
    ## Treatmentbio:DateApril -0.24000    0.11541  -2.079
    ## 
    ## Correlation of Fixed Effects:
    ##              (Intr) Trtmnt DateAg DateJn DtAprl Trtmntb:DtAg Trt:DJ
    ## Treatmentbi  -0.555                                                
    ## DateAug      -0.555  0.500                                         
    ## DateJune     -0.555  0.500  0.500                                  
    ## DateApril    -0.555  0.500  0.500  0.500                           
    ## Trtmntb:DtAg  0.392 -0.707 -0.707 -0.354 -0.354                    
    ## Trtmntb:DtJ   0.392 -0.707 -0.354 -0.707 -0.354  0.500             
    ## Trtmntb:DtAp  0.392 -0.707 -0.354 -0.354 -0.707  0.500        0.500

    b_fit <- list()
    for(i in 1:100) {
      b_sample <- dplyr::sample_n(soil, dim(soil)[1], replace = TRUE)
      b_coefs <- fixef(lmer(MWD~Treatment*Date+(1|Block), data = b_sample))
      b_fit[[i]] <- b_coefs
    }

    b_all_coef <- plyr::ldply(b_fit)
    sort(b_all_coef$Treatmentbio)

    ##   [1] 0.2066662 0.2932373 0.3011936 0.3113960 0.3139566 0.3139953 0.3219060
    ##   [8] 0.3378558 0.3388305 0.3422045 0.3434197 0.3514285 0.3519962 0.3613884
    ##  [15] 0.3642756 0.3676284 0.3805798 0.3806376 0.3840238 0.3900439 0.3920076
    ##  [22] 0.3922161 0.3970098 0.3981507 0.3982339 0.3984107 0.3985319 0.3995625
    ##  [29] 0.4004665 0.4038617 0.4093530 0.4097309 0.4124482 0.4146012 0.4150995
    ##  [36] 0.4155322 0.4191032 0.4192813 0.4195940 0.4202633 0.4205446 0.4215882
    ##  [43] 0.4253796 0.4257449 0.4262976 0.4269536 0.4271554 0.4277684 0.4288042
    ##  [50] 0.4301122 0.4341733 0.4349637 0.4372868 0.4380991 0.4382722 0.4386320
    ##  [57] 0.4402158 0.4437594 0.4442596 0.4463047 0.4494831 0.4495134 0.4506883
    ##  [64] 0.4517499 0.4522892 0.4533169 0.4563590 0.4571932 0.4624068 0.4626796
    ##  [71] 0.4636629 0.4657062 0.4662573 0.4675341 0.4716783 0.4750593 0.4765129
    ##  [78] 0.4798154 0.4806332 0.4813507 0.4852205 0.4877493 0.4889265 0.4957872
    ##  [85] 0.4960396 0.5001868 0.5029650 0.5055649 0.5073192 0.5174194 0.5190960
    ##  [92] 0.5226270 0.5242771 0.5272534 0.5294196 0.5322475 0.5393425 0.5420293
    ##  [99] 0.5462039 0.5651104

    # estimate with 95% Confidence interval is: 0.412 (0.265, 0.522) <- fix date in October

> S550: Good idea to build up the model in this way. But we can take out
> all the preliminary models when we write the final report.

A simple model to start with:  
- Randomized block design  
- Ignore the transects and repeated measurements for now  
- Treat the data collected from the transects as random samples  
- Assume no block interaction effects

Looking at the data in terms of treatment and block:

interaction.plot(soil*T**r**e**a**t**m**e**n**t*, *s**o**i**l*Block,
soil$MWD, xlab="Treatment", ylab="MWD", main="Change in MWD over
Treatments by Block")

boxcox(MWD~Block+Treatment, data=soil) \# To see if transformation on y
is needed soil.rbd\<-aov(MWD~Block+Treatment, soil) summary(soil.rbd)
plot(soil.rbd)

Adding Date to the model:

> S550: Why use boxcox? Is the transformation completely necessary? How
> much of a difference does it make in the fit? It makes the results
> less intuitive, so perhaps it's best to leave it out unless the
> linearity assumption is strongly violated.

> S550: Why use sqrt(MWD) in latter part but not in the upper part?

boxcox(MWD~Block+Treatment*Date, data=soil)
soil.rbd2\<-aov(sqrt(MWD)~Block+Treatment*Date, soil) summary(soil.rbd2)
plot(soil.rbd2)

Mixed-effects model:

Transect.f\<-as.factor(soil$Transect)
lmer1.1\<-lmer(MWD~Block*Date*Treatment+(1|Transect.f), soil)
summary(lmer1.1)

lmer1.2\<-lmer(MWD~Block\*Date+(1|Transect.f), soil)
anova(lmer1.1,lmer1.2)

> S550: The three-way intercation probably won't be necessary when
> choosing the final model. How sure are we that even the two-way
> interaction is necessary? Perhaps we could just do an additive model?

Mixed-effects model with nested factor:

lmer2.1\<-lmer(MWD~Block*Date*Treatment+(1|Treatment:Transect.f), soil)
summary(lmer2.1)

lmer2.2\<-lmer(MWD~Block\*Date+(1|Treatment:Transect.f), soil)
anova(lmer2.1,lmer2.2)

Mixed-effects model with nested factor and repeated measurements:  
(I am not sure if I formulate the model I want with correct R syntax. I
am thinking about fixed block effect, fixed treatment effect, random
transect effect, transect factor nested within treatment factor,
transect was measured repeatedly over date.)

lmer3.1\<-lmer(MWD~Block*Date*Treatment+(Date|Treatment:Transect.f),
soil) summary(lmer3.1)

lmer3.2\<-lmer(MWD~Block\*Date+(Date|Treatment:Transect.f), soil)
anova(lmer3.1,lmer3.2)

#### Part 2

1.  Model 1: all explanatory variables are fixed categorical variables

model1 \<- aov(Cover.value ~ Block.f \* Treatment \* Transect.f)
summary(model1) plot(resid(model1)) summary(lm(Cover.value ~
Treatment*Block.f*Transect.f))

Comment: Treatment effect seems to be significant. Sum of squares for
residuals is very large, indicating a lot of variation is unexplained
and the significance of treatment might not be true. No obvious pattern
from residual plot.

1.  Model 2: Transect is now a random effect, Block and Treatment are
    fixed effects

model2.1 \<- lmer(Cover.value ~ Treatment+Block.f+(1|Transect.f))
summary(model2.1) model2.2 \<- lmer(Cover.value ~
1+Block+(1|Transect.f)) anova(model2.2, model2.1)

> S550: Not sure if the response variable is categorical then usual
> linear mixed effect model will be worthy or not? Why you use Block.f
> for the model2.1 and Block for the model2.2? Also need to clarify
> about random intercept and random slope model.

Comment: Treatment effect is still significant at the same significance
level. From the lm output, A small proportion of randomness is explained
by Transect effects but variance of residuals is still quite large. The
residuals seem to be centered at 0 from the summary of lm output: scaled
residuals.

Question: I did not use aov() because I am not sure how to read summary
of aov output properly when there are three explanatory variables. It
contains strange behaviours. Not sure if aov output would be more useful
here.

1.  Model 3: Mixed effects models with nested structure

model3.1 \<- lmer(Cover.value ~
Treatment+Block.f+(1|Treatment:Transect.f)) summary(model3.1) model3.2
\<- lmer(Cover.value ~ 1+Block.f+(1|Treatment:Transect.f))
anova(model3.1, model3.2)

Comment: Treatment is still a significant factor in this mixed effect
nested model. Same as before, residual variance is still large.

1.  Quick comment on Ordinal regression model to incorporate that
    Cover.value is a discrete dependent variable

Cover.value.f \<- as.factor(Cover.value) model4.1 \<- polr(Cover.value.f
~ Treatment+Block.f+Transect.f, Hess = T) summary(model4.1) model4.2 \<-
polr(Cover.value.f ~ 1+Block.f+Transect.f, Hess = T) anova(model4.1,
model4.2)

I tried to include random effects in ordinal regression model but polr()
does not recognize the same syntax as above. In addition I get confused
when comparing two ordinal regression models as anova() shows a p-value
of 0. I think we should discuss whether it is enough to assume that
Cover.value is contineous before exploring more about ordinal
regression?

detach(pc)

    # pc.subset <- subset(pc, Species == "POPR")
    # subset.cv <- pc.subset[ ,c(3,4,9)]
    # 
    # Bio_B1_original1 <- 
    #   filter(subset.cv, Block == 1 & Treatment == "Biosolids") %>%
    #   select(Block)
    # zero_B <- list(rep(1, 19))
    # a<-rbind(zero_B, Bio_B1_original1)
    # 
    # Bio_B1_original2 <- 
    #   filter(subset.cv, Block == 1 & Treatment == "Biosolids") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Biosolids", 19))
    # b<-rbind(zero_T, Bio_B1_original2)
    # 
    # Bio_B1_original3 <- 
    #   filter(subset.cv, Block == 1 & Treatment == "Biosolids") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 19))
    # c<-rbind(zero_C, Bio_B1_original3)
    # 
    # Bio_B1 <- cbind(a,b,c)
    # 
    # #----
    # 
    # Bio_B2_original1 <- 
    #   filter(subset.cv, Block == 2 & Treatment == "Biosolids") %>%
    #   select(Block)
    # zero_B <- list(rep(2, 36))
    # a<-rbind(zero_B, Bio_B2_original1)
    # 
    # Bio_B2_original2 <- 
    #   filter(subset.cv, Block == 2 & Treatment == "Biosolids") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Biosolids", 36))
    # b<-rbind(zero_T, Bio_B2_original2)
    # 
    # Bio_B2_original3 <- 
    #   filter(subset.cv, Block == 2 & Treatment == "Biosolids") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 36))
    # c<-rbind(zero_C, Bio_B2_original3)
    # 
    # Bio_B2 <- cbind(a,b,c)
    # 
    # #------
    # 
    # Bio_B3_original1 <- 
    #   filter(subset.cv, Block == 3 & Treatment == "Biosolids") %>%
    #   select(Block)
    # zero_B <- list(rep(3, 21))
    # a<-rbind(zero_B, Bio_B3_original1)
    # 
    # Bio_B3_original2 <- 
    #   filter(subset.cv, Block == 3 & Treatment == "Biosolids") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Biosolids", 21))
    # b<-rbind(zero_T, Bio_B3_original2)
    # 
    # Bio_B3_original3 <- 
    #   filter(subset.cv, Block == 3 & Treatment == "Biosolids") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 21))
    # c<-rbind(zero_C, Bio_B3_original3)
    # 
    # Bio_B3 <- cbind(a,b,c)
    # 
    # #-----------------------
    # Bio_B4_original1 <- 
    #   filter(subset.cv, Block == 4 & Treatment == "Biosolids") %>%
    #   select(Block)
    # zero_B <- list(rep(4, 9))
    # a<-rbind(zero_B, Bio_B4_original1)
    # 
    # Bio_B4_original2 <- 
    #   filter(subset.cv, Block == 4 & Treatment == "Biosolids") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Biosolids", 9))
    # b<-rbind(zero_T, Bio_B4_original2)
    # 
    # Bio_B4_original3 <- 
    #   filter(subset.cv, Block == 4 & Treatment == "Biosolids") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 9))
    # c<-rbind(zero_C, Bio_B4_original3)
    # 
    # Bio_B4 <- cbind(a,b,c)
    # 
    # #-----------------------
    # 
    # Con_B1_original1 <- 
    #   filter(subset.cv, Block == 1 & Treatment == "Control") %>%
    #   select(Block)
    # zero_B <- list(rep(1, 47))
    # a<-rbind(zero_B, Con_B1_original1)
    # 
    # Con_B1_original2 <- 
    #   filter(subset.cv, Block == 1 & Treatment == "Control") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Control", 47))
    # b<-rbind(zero_T, Con_B1_original2)
    # 
    # Con_B1_original3 <- 
    #   filter(subset.cv, Block == 1 & Treatment == "Control") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 47))
    # c<-rbind(zero_C, Con_B1_original3)
    # 
    # Con_B1 <- cbind(a,b,c)
    # 
    # #----
    # 
    # a <- matrix(2, 50, 1)
    # 
    # b <- matrix("Control", 50,1)
    # 
    # c <- matrix(0, 50,1)
    # 
    # Con_B2<-data.frame(cbind(a,b,c))
    # colnames(Con_B2)[1]<-"Block"
    # colnames(Con_B2)[2]<-"Treatment"
    # colnames(Con_B2)[3]<-"Cover.value"
    # 
    # 
    # 
    # #-------
    # 
    # Con_B3_original1 <- 
    #   filter(subset.cv, Block == 3 & Treatment == "Control") %>%
    #   select(Block)
    # zero_B <- list(rep(3, 49))
    # a<-rbind(zero_B, Con_B3_original1)
    # 
    # Con_B3_original2 <- 
    #   filter(subset.cv, Block == 3 & Treatment == "Control") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Control", 49))
    # b<-rbind(zero_T, Con_B3_original2)
    # 
    # Con_B3_original3 <- 
    #   filter(subset.cv, Block == 3 & Treatment == "Control") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 49))
    # c<-rbind(zero_C, Con_B3_original3)
    # 
    # Con_B3 <- cbind(a,b,c)
    # 
    # 
    # #-------------------
    # 
    # 
    # Con_B4_original1 <- 
    #   filter(subset.cv, Block == 4 & Treatment == "Control") %>%
    #   select(Block)
    # zero_B <- list(rep(4, 49))
    # a<-rbind(zero_B, Con_B4_original1)
    # 
    # Con_B4_original2 <- 
    #   filter(subset.cv, Block == 4 & Treatment == "Control") %>%
    #   select(Treatment)
    # zero_T <- list(rep("Control", 49))
    # b<-rbind(zero_T, Con_B4_original2)
    # 
    # Con_B4_original3 <- 
    #   filter(subset.cv, Block == 4 & Treatment == "Control") %>%
    #   select(Cover.value)
    # zero_C <- list(rep(0, 49))
    # c<-rbind(zero_C, Con_B4_original3)
    # 
    # Con_B4 <- cbind(a,b,c)
    # 
    # #----------------------
    # dat <- rbind(Bio_B1, Bio_B2, Bio_B3, Bio_B4, Con_B1, Con_B2,Con_B3,Con_B4)
    # 
    # model_zero <- aov(as.numeric(Cover.value) ~ Treatment+Error(Treatment/Block), data = dat)
    # summary(model_zero)
    # 
    # detach(pc)
    # ```
    # 
    # ```{r}
    # pc.corr <- melt(tapply(pc$Cover.value, pc$Block, mean))
    # soil.june <- soil %>%
    #   filter(Date == "June")
    # soil.corr <- melt(tapply(soil.june$MWD, soil.june$Block, mean))
    # cor(pc.corr[2], soil.corr[2])

MWD and Plant Cover Value seem to have a strong negative correlation
which is strange because we expect to see at least a positive
correlation. The problem is that the two variable, MWD and Cover Value,
do not have the same length. MWD is measured in 4 months and is
calculated as an average of 3 transects. Cover Value is measured only
once in June and is calculated as an average of 5 transects. In the code
above, I simply take all average within blocks (4 measurements for each
MWD and plant cover data) and take only June measurements from MWD data.
We should fix this issue soon in next steps.

> S550: why use melt function here? only tapply gives the same result.
> But not sur about the process!

> S550: We might have to do a Spearman correlation (it's an option
> inside the cor() function), because one of the variables is ordinal.

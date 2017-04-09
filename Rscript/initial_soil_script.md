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

    ##   [1] 0.03188650 0.06806141 0.07444814 0.07810555 0.08465799 0.09358740
    ##   [7] 0.09390147 0.09770766 0.09820948 0.09958734 0.10053423 0.10435773
    ##  [13] 0.10838976 0.11059072 0.11287275 0.11723083 0.12479241 0.12623226
    ##  [19] 0.12679175 0.12710894 0.12895795 0.13158230 0.13171830 0.13244579
    ##  [25] 0.13587709 0.13588499 0.13615317 0.13716980 0.13908385 0.14084508
    ##  [31] 0.14141661 0.14358503 0.14545999 0.14739303 0.14803494 0.15016838
    ##  [37] 0.15223684 0.15359264 0.15388557 0.15412196 0.15457833 0.15657395
    ##  [43] 0.15728239 0.15751278 0.15913214 0.15924326 0.15975236 0.16225523
    ##  [49] 0.16325742 0.16421500 0.16547354 0.16718793 0.16739753 0.16866557
    ##  [55] 0.16931870 0.16960958 0.16968514 0.17111660 0.17127144 0.17145281
    ##  [61] 0.17166847 0.17251997 0.17746969 0.17968986 0.18137650 0.18417457
    ##  [67] 0.18538899 0.18670765 0.18879842 0.18990685 0.19257592 0.19438480
    ##  [73] 0.19703104 0.19720624 0.19829661 0.19872713 0.20283343 0.20578201
    ##  [79] 0.20733166 0.20823937 0.20905533 0.21052172 0.21063413 0.21084856
    ##  [85] 0.21112972 0.21115282 0.21159031 0.21205164 0.21511780 0.21823414
    ##  [91] 0.22273877 0.22447297 0.22936564 0.22952545 0.22982647 0.23262149
    ##  [97] 0.23866146 0.24042786 0.25718157 0.27009865

    # estimate with 95% Confidence interval is: 0.172 (0.054, 0.261) <- fix date in April
    sort(b_all_coef$DateJune)

    ##   [1] 0.7023154 0.7138035 0.7295257 0.7297372 0.7376355 0.7384465 0.7413947
    ##   [8] 0.7550173 0.7557373 0.7560254 0.7600517 0.7672960 0.7710089 0.7713885
    ##  [15] 0.7750953 0.7774987 0.7787152 0.7803292 0.7818989 0.7828139 0.7842963
    ##  [22] 0.7857535 0.7864967 0.7879134 0.7926677 0.7937158 0.7938081 0.7942129
    ##  [29] 0.7951559 0.7964564 0.7985440 0.8018615 0.8067179 0.8084150 0.8088901
    ##  [36] 0.8104841 0.8109075 0.8122027 0.8178864 0.8182578 0.8193137 0.8200212
    ##  [43] 0.8207436 0.8208969 0.8224576 0.8244335 0.8252732 0.8281983 0.8304980
    ##  [50] 0.8323550 0.8325308 0.8365528 0.8376301 0.8396978 0.8408534 0.8421161
    ##  [57] 0.8427007 0.8440002 0.8451006 0.8492181 0.8512762 0.8518864 0.8524255
    ##  [64] 0.8524878 0.8538687 0.8565076 0.8584625 0.8589488 0.8617665 0.8640093
    ##  [71] 0.8669836 0.8691360 0.8705878 0.8708184 0.8717928 0.8764589 0.8775833
    ##  [78] 0.8776638 0.8784550 0.8786796 0.8789121 0.8789634 0.8802499 0.8822142
    ##  [85] 0.8825799 0.8892695 0.8893145 0.8914512 0.8916516 0.8929786 0.8941129
    ##  [92] 0.8984868 0.9026160 0.9044353 0.9100507 0.9117765 0.9201018 0.9291178
    ##  [99] 0.9576627 0.9677925

    # estimate with 95% Confidence interval is: 0.838 (0.719, 0.956)
    sort(b_all_coef$`Treatmentbio:DateOct`)

    ##   [1] 0.08300786 0.09295122 0.09376073 0.10935416 0.11412674 0.12170363
    ##   [7] 0.12719406 0.12723875 0.13554189 0.13569348 0.14629758 0.16142394
    ##  [13] 0.17256896 0.17617033 0.17843637 0.17845368 0.18080262 0.18140290
    ##  [19] 0.18279391 0.18536119 0.18559142 0.18576084 0.18678357 0.18686058
    ##  [25] 0.18692621 0.18951514 0.18990533 0.19000085 0.19682089 0.20552960
    ##  [31] 0.20560659 0.20598477 0.20680477 0.20732602 0.20913994 0.21011891
    ##  [37] 0.21096780 0.21396912 0.21870170 0.21938587 0.22246383 0.22488437
    ##  [43] 0.22667978 0.22751665 0.22815678 0.23119804 0.23590962 0.23851516
    ##  [49] 0.24138085 0.24268121 0.24318493 0.24358292 0.24483680 0.24656216
    ##  [55] 0.24702814 0.24795945 0.24956582 0.25151469 0.25382778 0.25695160
    ##  [61] 0.25740062 0.25811782 0.26044619 0.26062249 0.26350801 0.26662365
    ##  [67] 0.27059861 0.27428736 0.27525226 0.28038485 0.28196138 0.28220329
    ##  [73] 0.28415322 0.28439798 0.28457004 0.28887157 0.28951011 0.30002518
    ##  [79] 0.30088493 0.31580460 0.31611814 0.32020214 0.32161455 0.32394977
    ##  [85] 0.32828295 0.33845432 0.34456658 0.35055488 0.35121092 0.35731029
    ##  [91] 0.36026758 0.36076109 0.36347473 0.36532843 0.38244062 0.40202295
    ##  [97] 0.40610516 0.40904353 0.40929322 0.41631291

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

### Correlation between Plant Cover and MWD (using the whole dataset, not a specific species)

    # use original dataset without relevel
    soil<-read.table("MWD.csv", sep = ",", header = T)
    Transect.f<-as.factor(soil$Transect)
    Date.f = factor(soil$Date,levels(soil$Date)[c(1, 3, 2, 4)])
    pc <- read.csv("plant_cover.csv", header = T)
    pc$Block <- as.factor(pc$Block)
    pc.subset <- subset(pc, Species == "POPR")
    pc.subset <- pc.subset[ ,c(3,4,9)]
    mean_mwd <- group_by(soil,Block,Treatment) %>%
      summarise(mean(MWD))
    x <- mean_mwd$`mean(MWD)`

    cv_b_tr <- group_by(pc, Block, Treatment)
    mean_cv <- summarise(cv_b_tr,sum(Cover.value)/50)
    y<-mean_cv$`sum(Cover.value)/50`

    cor(x,y)

    ## [1] 0.4517191

### Correlation between POPR Plant Cover and MWD

    mean_mwd <- group_by(soil,Block,Treatment) %>%
      summarise(mean(MWD))
    x <- mean_mwd$`mean(MWD)`
    mean.MWD <- c(x[1], x[2], x[3], x[5], x[6], x[7], x[8], x[4])
    #re-arrange the order to match with the order of dat.avg

    mean.cv <- as.numeric(as.character(dat.avg$y.avg))
    cor(mean.MWD, mean.cv)

    ## [1] 0.6447241

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

    ##   [1] -0.2162468674 -0.1853359552 -0.1831974289 -0.1616155092 -0.1448543373
    ##   [6] -0.1429582371 -0.1342941073 -0.1211909275 -0.1162634779 -0.1126228615
    ##  [11] -0.1092391382 -0.0970343714 -0.0959040265 -0.0895964342 -0.0855836751
    ##  [16] -0.0839857775 -0.0838851443 -0.0825332546 -0.0825109689 -0.0793921609
    ##  [21] -0.0778684213 -0.0777736975 -0.0758230176 -0.0665881560 -0.0653850898
    ##  [26] -0.0629415482 -0.0580831053 -0.0522801570 -0.0493421456 -0.0449818805
    ##  [31] -0.0435865103 -0.0323120490 -0.0264127864 -0.0242137805 -0.0115550854
    ##  [36] -0.0114798290 -0.0079207853 -0.0074267060 -0.0061902850 -0.0061100962
    ##  [41] -0.0051776685 -0.0038730829 -0.0011069535 -0.0007022050 -0.0002770805
    ##  [46]  0.0027436937  0.0046940814  0.0047138356  0.0121392993  0.0126721946
    ##  [51]  0.0138192955  0.0228579808  0.0240123722  0.0259326536  0.0299142551
    ##  [56]  0.0343526576  0.0372027174  0.0376390031  0.0404719017  0.0478687810
    ##  [61]  0.0487487269  0.0502997512  0.0509602663  0.0571783440  0.0685314360
    ##  [66]  0.0709436341  0.0710754592  0.0723106376  0.0761657601  0.0786226398
    ##  [71]  0.0806875162  0.0816426257  0.0856762134  0.0858639559  0.0889365709
    ##  [76]  0.0902417491  0.0914470576  0.0923343521  0.0956406890  0.0977447565
    ##  [81]  0.1063907826  0.1107933808  0.1244553904  0.1261972368  0.1333132908
    ##  [86]  0.1350801141  0.1387737177  0.1395631745  0.1427605429  0.1522187891
    ##  [91]  0.1525106496  0.1703815305  0.1796867261  0.1817719658  0.1851851667
    ##  [96]  0.1935179667  0.2009205805  0.2069822430  0.2129971390  0.2945515480

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

    ##   [1] 0.05938285 0.07882858 0.07964160 0.09162769 0.11315287 0.11369008
    ##   [7] 0.11633926 0.11639242 0.12615443 0.13045054 0.13315626 0.13772010
    ##  [13] 0.14269959 0.14678680 0.14726964 0.14937557 0.15116465 0.15434531
    ##  [19] 0.16274781 0.16297006 0.16342251 0.16600627 0.16936940 0.17042118
    ##  [25] 0.17238906 0.17318519 0.17817288 0.17975937 0.18065996 0.18096229
    ##  [31] 0.18523471 0.18535707 0.18562190 0.19081594 0.19302426 0.19457196
    ##  [37] 0.19509172 0.19647859 0.19779009 0.20017814 0.20358237 0.21288135
    ##  [43] 0.21331374 0.21361978 0.22376507 0.22388113 0.22416782 0.22701499
    ##  [49] 0.22822622 0.22929423 0.23066975 0.23108178 0.23137329 0.23678996
    ##  [55] 0.23857807 0.23858747 0.23955764 0.24477106 0.24654686 0.25112189
    ##  [61] 0.25118952 0.25343891 0.25391226 0.25590423 0.25724469 0.26145939
    ##  [67] 0.26157943 0.26379856 0.26426971 0.26536521 0.26603826 0.26662716
    ##  [73] 0.26962613 0.27266511 0.27415821 0.27731221 0.27983729 0.28121190
    ##  [79] 0.28226370 0.28285766 0.28464079 0.28681173 0.28790256 0.28945958
    ##  [85] 0.29245968 0.30564697 0.30601638 0.31097597 0.31397842 0.31528535
    ##  [91] 0.31553613 0.32050972 0.32538082 0.33437915 0.34182043 0.34551759
    ##  [97] 0.35445721 0.36139987 0.36610731 0.42080533

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

    ##   [1] 0.2764277 0.2860354 0.3076093 0.3090798 0.3137796 0.3231111 0.3233351
    ##   [8] 0.3236448 0.3256382 0.3295493 0.3317370 0.3382610 0.3384090 0.3422096
    ##  [15] 0.3462981 0.3471136 0.3471339 0.3479115 0.3486508 0.3490434 0.3502745
    ##  [22] 0.3515750 0.3523509 0.3535667 0.3577162 0.3613672 0.3701723 0.3709009
    ##  [29] 0.3710129 0.3720519 0.3734850 0.3761786 0.3780502 0.3801526 0.3808360
    ##  [36] 0.3824808 0.3840511 0.3845458 0.3942715 0.3952292 0.4044246 0.4049277
    ##  [43] 0.4063143 0.4069554 0.4083685 0.4115776 0.4136557 0.4215590 0.4232517
    ##  [50] 0.4239256 0.4271017 0.4291703 0.4292128 0.4309579 0.4310451 0.4326880
    ##  [57] 0.4332001 0.4339854 0.4350693 0.4416014 0.4418000 0.4431066 0.4443373
    ##  [64] 0.4444882 0.4458019 0.4461315 0.4480843 0.4498900 0.4499669 0.4529975
    ##  [71] 0.4541011 0.4550008 0.4557016 0.4565364 0.4575780 0.4577253 0.4630992
    ##  [78] 0.4664358 0.4670533 0.4691114 0.4698490 0.4725492 0.4746245 0.4749782
    ##  [85] 0.4763840 0.4769988 0.4770084 0.4775458 0.4782785 0.4895013 0.4914107
    ##  [92] 0.4964870 0.5013097 0.5036045 0.5079854 0.5109354 0.5190548 0.5194142
    ##  [99] 0.5226323 0.5382364

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

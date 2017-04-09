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

    ##   [1] 0.02952434 0.05852333 0.06248711 0.07833483 0.09583377 0.09600276
    ##   [7] 0.09714090 0.10417621 0.10606973 0.11102686 0.11228286 0.11628422
    ##  [13] 0.12906055 0.12979186 0.13003191 0.13042195 0.13058333 0.13244519
    ##  [19] 0.13376897 0.13487287 0.13785759 0.14155879 0.14380009 0.14741403
    ##  [25] 0.15037947 0.15224913 0.15267130 0.15473615 0.15632206 0.15665349
    ##  [31] 0.15678635 0.15762134 0.16003430 0.16042280 0.16143099 0.16235154
    ##  [37] 0.16571257 0.16621431 0.16731039 0.16758444 0.16876280 0.17123701
    ##  [43] 0.17428980 0.17529970 0.17639788 0.17820191 0.17938871 0.18041628
    ##  [49] 0.18201000 0.18207899 0.18284913 0.18355015 0.18605334 0.18691639
    ##  [55] 0.18721584 0.18727263 0.18822828 0.18837135 0.19023186 0.19104994
    ##  [61] 0.19174588 0.19242133 0.19261147 0.19408821 0.20226236 0.20422265
    ##  [67] 0.20504859 0.20565520 0.20641627 0.20985461 0.21238930 0.21402547
    ##  [73] 0.21547893 0.21563866 0.21675201 0.21723010 0.22076102 0.22374860
    ##  [79] 0.22418409 0.22604014 0.22680356 0.22763596 0.23486978 0.23785445
    ##  [85] 0.23967042 0.24156994 0.24352562 0.25092956 0.25433977 0.25518953
    ##  [91] 0.25826048 0.25853323 0.25874810 0.25903766 0.26170277 0.27715186
    ##  [97] 0.28395196 0.28877237 0.31516004 0.32798503

    # estimate with 95% Confidence interval is: 0.172 (0.054, 0.261) <- fix date in April
    sort(b_all_coef$DateJune)

    ##   [1] 0.6873463 0.7311222 0.7337687 0.7376415 0.7455973 0.7535441 0.7583418
    ##   [8] 0.7613683 0.7649770 0.7669519 0.7794845 0.7799214 0.7820202 0.7825185
    ##  [15] 0.7874900 0.7899372 0.7948917 0.7951136 0.8003342 0.8020063 0.8021511
    ##  [22] 0.8033206 0.8068802 0.8146386 0.8173665 0.8185411 0.8191017 0.8197378
    ##  [29] 0.8231662 0.8253469 0.8266161 0.8288179 0.8289456 0.8290328 0.8292648
    ##  [36] 0.8298250 0.8302361 0.8311260 0.8339702 0.8344563 0.8345038 0.8351808
    ##  [43] 0.8370506 0.8375096 0.8375161 0.8391125 0.8410253 0.8411748 0.8414138
    ##  [50] 0.8432697 0.8473549 0.8474731 0.8479276 0.8479881 0.8496524 0.8515784
    ##  [57] 0.8527618 0.8529237 0.8542925 0.8551947 0.8556499 0.8563303 0.8573078
    ##  [64] 0.8589727 0.8651198 0.8664810 0.8670947 0.8674446 0.8693299 0.8708073
    ##  [71] 0.8745123 0.8746407 0.8782529 0.8792896 0.8802734 0.8830426 0.8883451
    ##  [78] 0.8895741 0.8911952 0.8912257 0.8927315 0.8929779 0.8985763 0.9029745
    ##  [85] 0.9065713 0.9117198 0.9121872 0.9129647 0.9134755 0.9143660 0.9147064
    ##  [92] 0.9157981 0.9220475 0.9246464 0.9256193 0.9532978 0.9605715 0.9883158
    ##  [99] 0.9935383 1.0014381

    # estimate with 95% Confidence interval is: 0.838 (0.719, 0.956)
    sort(b_all_coef$`Treatmentbio:DateOct`)

    ##   [1] 0.01979415 0.03371864 0.05942723 0.06141283 0.08196304 0.10011939
    ##   [7] 0.11132207 0.11179652 0.11245349 0.11266583 0.11689097 0.11924378
    ##  [13] 0.12157081 0.12175978 0.12300368 0.12824997 0.13350510 0.14731453
    ##  [19] 0.15369428 0.15433778 0.15563591 0.15566020 0.16101324 0.16326150
    ##  [25] 0.16841731 0.17054557 0.17420849 0.17628360 0.17828058 0.18055227
    ##  [31] 0.18215512 0.18578588 0.18657480 0.19460261 0.19724906 0.19761366
    ##  [37] 0.20360626 0.20865497 0.20889042 0.21011638 0.21138163 0.21187944
    ##  [43] 0.21313248 0.21378820 0.21522792 0.21766693 0.21971243 0.22108052
    ##  [49] 0.22149117 0.22480882 0.22618219 0.22917995 0.23268847 0.23360759
    ##  [55] 0.23369547 0.23465216 0.23498422 0.23944861 0.24214925 0.24222716
    ##  [61] 0.24354135 0.24423458 0.24766162 0.25073273 0.25236861 0.25309146
    ##  [67] 0.25383115 0.25589723 0.25775062 0.25981059 0.26717682 0.27245706
    ##  [73] 0.27628100 0.28893098 0.29571584 0.29599113 0.29785843 0.29840822
    ##  [79] 0.30058584 0.30175249 0.30987413 0.31216882 0.31743131 0.32310393
    ##  [85] 0.33581637 0.34348938 0.34622070 0.35758490 0.35840983 0.35862510
    ##  [91] 0.35957422 0.36245590 0.36901095 0.36934137 0.37585529 0.37746492
    ##  [97] 0.39461502 0.39578499 0.40688071 0.42372885

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

    mean_mwd <- group_by(soil,Block,Treatment) %>%
      summarise(mean(MWD))
    x <- mean_mwd$`mean(MWD)`

    cv_b_tr <- group_by(pc, Block, Treatment)
    mean_cv <- summarise(cv_b_tr,sum(Cover.value)/50)
    y<-mean_cv$`sum(Cover.value)/50`

    cor(x,y)

    ## [1] -0.9196432

### Correlation between POPR Plant Cover and MWD

    #re-arrange the order to match with the order of dat.avg
    mean.MWD <- c(x[1], x[2], x[3], x[5], x[6], x[7], x[8], x[4])
    mean.cv <- as.numeric(as.character(dat.avg$y.avg))
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

    ##   [1] -0.325838308 -0.260034787 -0.234208681 -0.219122470 -0.188719704
    ##   [6] -0.183250772 -0.153559478 -0.146482510 -0.133938030 -0.131819053
    ##  [11] -0.125818447 -0.096594929 -0.094882665 -0.094671431 -0.092261725
    ##  [16] -0.087608416 -0.087047477 -0.081364686 -0.079742888 -0.077417613
    ##  [21] -0.075780998 -0.075707217 -0.075177282 -0.074732372 -0.074056222
    ##  [26] -0.058036891 -0.057078506 -0.056934174 -0.056630945 -0.055567497
    ##  [31] -0.051136839 -0.049660045 -0.047859393 -0.043833936 -0.043648677
    ##  [36] -0.039825776 -0.039652408 -0.033481222 -0.031371559 -0.029589444
    ##  [41] -0.028767880 -0.026723228 -0.018269919 -0.015727869 -0.010341955
    ##  [46] -0.006914230 -0.004625774 -0.003536666  0.005652257  0.009985089
    ##  [51]  0.013080534  0.024317393  0.024488205  0.025098801  0.027577789
    ##  [56]  0.029648790  0.030611343  0.035793895  0.040903721  0.046997964
    ##  [61]  0.047641020  0.052798440  0.053207719  0.060736562  0.061582426
    ##  [66]  0.067093692  0.069142103  0.070027059  0.076005875  0.076168901
    ##  [71]  0.080817253  0.080883509  0.087435543  0.087749474  0.089600107
    ##  [76]  0.090992586  0.091306236  0.098333979  0.099971557  0.101254054
    ##  [81]  0.101992782  0.103321228  0.116198799  0.124104134  0.127239603
    ##  [86]  0.133488593  0.144514800  0.145277778  0.151385611  0.165196617
    ##  [91]  0.171169036  0.171239037  0.173675498  0.178082568  0.199580431
    ##  [96]  0.212949204  0.216121301  0.216691813  0.235600140  0.298838340

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

    ##   [1] 0.07313876 0.07486525 0.09686283 0.09693123 0.10104249 0.10517361
    ##   [7] 0.10815898 0.10951238 0.10956923 0.11895140 0.11958244 0.12121017
    ##  [13] 0.12210744 0.12400618 0.12907182 0.15171760 0.15496785 0.15561990
    ##  [19] 0.15925828 0.15991585 0.16114396 0.16156970 0.16170426 0.16276173
    ##  [25] 0.16678078 0.16989366 0.17339442 0.17799117 0.17956965 0.18318952
    ##  [31] 0.18358802 0.18481386 0.18562779 0.18703795 0.18895271 0.18998864
    ##  [37] 0.19052898 0.19293739 0.19520480 0.19586991 0.20003086 0.20095356
    ##  [43] 0.20350606 0.20441404 0.20455674 0.20923781 0.21017361 0.21856298
    ##  [49] 0.22282169 0.22400247 0.22496246 0.22562137 0.22748271 0.22879332
    ##  [55] 0.22939342 0.22944831 0.23052139 0.23194041 0.23605818 0.24374753
    ##  [61] 0.24607843 0.24920888 0.25106802 0.25174479 0.25360067 0.25646829
    ##  [67] 0.25656187 0.26188916 0.26527834 0.26557741 0.26690309 0.26843993
    ##  [73] 0.26852747 0.27379073 0.27551773 0.28350194 0.28368700 0.28396916
    ##  [79] 0.28454847 0.29485751 0.29551033 0.29618998 0.29674708 0.29698578
    ##  [85] 0.29931505 0.30631848 0.30722566 0.31738170 0.32723651 0.32911162
    ##  [91] 0.34256208 0.34477912 0.34524252 0.34723662 0.35081459 0.36133130
    ##  [97] 0.36360970 0.38524760 0.38904261 0.40601074

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

    ##   [1] 0.1961396 0.2847561 0.2916493 0.2951880 0.3013386 0.3061330 0.3093815
    ##   [8] 0.3292080 0.3358320 0.3386845 0.3451313 0.3503575 0.3516566 0.3570839
    ##  [15] 0.3596287 0.3605216 0.3683039 0.3694746 0.3694854 0.3720370 0.3728188
    ##  [22] 0.3762061 0.3772086 0.3786689 0.3840569 0.3848261 0.3851582 0.3859549
    ##  [29] 0.3898788 0.3953356 0.3956853 0.3970290 0.3981687 0.4033142 0.4048704
    ##  [36] 0.4056373 0.4075245 0.4076302 0.4123269 0.4137409 0.4165550 0.4184707
    ##  [43] 0.4193893 0.4210495 0.4215755 0.4218077 0.4228520 0.4243464 0.4252575
    ##  [50] 0.4254331 0.4267824 0.4291980 0.4299983 0.4365517 0.4378612 0.4394997
    ##  [57] 0.4403479 0.4408999 0.4446429 0.4455179 0.4485602 0.4505153 0.4508905
    ##  [64] 0.4513096 0.4560417 0.4578183 0.4578660 0.4579503 0.4594227 0.4642199
    ##  [71] 0.4647046 0.4668637 0.4678165 0.4687918 0.4715993 0.4736383 0.4749068
    ##  [78] 0.4751981 0.4766716 0.4768618 0.4793056 0.4846193 0.4852799 0.4855032
    ##  [85] 0.4910959 0.4922175 0.4936195 0.4971585 0.5000939 0.5003176 0.5005000
    ##  [92] 0.5060114 0.5077466 0.5234945 0.5320531 0.5353977 0.5639145 0.5726210
    ##  [99] 0.5833664 0.5954655

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

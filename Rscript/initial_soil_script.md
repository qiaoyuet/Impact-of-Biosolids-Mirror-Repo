LONG TERM IMPACTS OF BIOSOLIDS ON SOILS
---------------------------------------

### Explore the data

    library("ggplot2")
    library(lme4)
    library(reshape2)
    library(MASS)
    library(dplyr)

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

    hist(soil$MWD, xlab="MWD", main = "Histogram of MWD")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    ggplot(aes(x=soil$Treatment,y=soil$MWD), data=soil) + geom_boxplot() + labs(y="MWD", x="Treatment", main="Boxplot of MWD for the Two Treatment Groups")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    tapply(soil$MWD, soil$Treatment, mean)

    ##      bio      con 
    ## 1.560208 1.351250

    tapply(soil$MWD, soil$Treatment, sd)

    ##       bio       con 
    ## 0.3874274 0.3616547

    Transect.f<-as.factor(soil$Transect)
    plot.design(MWD~Treatment+Block+Date+Transect.f, data = soil, xlab="Treatment", ylab="MWD")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    ggplot(soil, aes(x = Date, y = MWD, group = Treatment, colour = Treatment)) +
    stat_summary(fun.y="mean", geom = "line") +
    labs(x = "Date", title = "Change in MWD over 4 Sampling Dates")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-3-4.png)

### Analysis

A simple model to start with:  
- Randomized block design  
- Ignore the transects and repeated measurements for now  
- Treat the data collected from the transects as random samples  
- Assume no block interaction effects

Looking at the data in terms of treatment and block:

    interaction.plot(soil$Treatment, soil$Block, soil$MWD, xlab="Treatment", ylab="MWD", main="Change in MWD over Treatments by Block")

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    boxcox(MWD~Block+Treatment, data=soil) # To see if transformation on y is needed

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    soil.rbd<-aov(MWD~Block+Treatment, soil)
    summary(soil.rbd)

    ##             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Block        3  0.720  0.2399   1.749 0.16254   
    ## Treatment    1  1.048  1.0479   7.640 0.00691 **
    ## Residuals   91 12.482  0.1372                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    plot(soil.rbd)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-5-2.png)![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-5-3.png)![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-5-4.png)![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-5-5.png)

Adding Date to the model:

    boxcox(MWD~Block+Treatment*Date, data=soil)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    soil.rbd2<-aov(sqrt(MWD)~Block+Treatment*Date, soil)
    summary(soil.rbd2)

    ##                Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Block           3 0.1226  0.0409   6.159 0.000771 ***
    ## Treatment       1 0.1842  0.1842  27.776 1.02e-06 ***
    ## Date            3 1.6443  0.5481  82.639  < 2e-16 ***
    ## Treatment:Date  3 0.0868  0.0289   4.361 0.006610 ** 
    ## Residuals      85 0.5638  0.0066                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    plot(soil.rbd2)

![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-6-2.png)![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-6-3.png)![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-6-4.png)![](initial_soil_script_files/figure-markdown_strict/unnamed-chunk-6-5.png)

Mixed-effects model:

    lmer1<-lmer(MWD~Block*Date*Treatment+(1|Transect.f), soil)
    summary(lmer1)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Block * Date * Treatment + (1 | Transect.f)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -12.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.94470 -0.43421  0.08463  0.46431  1.96034 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  Transect.f (Intercept) 0.001995 0.04467 
    ##  Residual               0.026700 0.16340 
    ## Number of obs: 96, groups:  Transect.f, 3
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error t value
    ## (Intercept)                    0.996667   0.097802  10.191
    ## Blockb2                        0.160000   0.133418   1.199
    ## Blockb3                       -0.123333   0.133418  -0.924
    ## Blockb4                        0.183333   0.133418   1.374
    ## DateAug                        0.500000   0.133418   3.748
    ## DateJune                       0.820000   0.133418   6.146
    ## DateOct                        0.696667   0.133418   5.222
    ## Treatmentcon                  -0.056667   0.133418  -0.425
    ## Blockb2:DateAug                0.126667   0.188681   0.671
    ## Blockb3:DateAug                0.543333   0.188681   2.880
    ## Blockb4:DateAug                0.243333   0.188681   1.290
    ## Blockb2:DateJune               0.063333   0.188681   0.336
    ## Blockb3:DateJune              -0.416667   0.188681  -2.208
    ## Blockb4:DateJune              -0.166667   0.188681  -0.883
    ## Blockb2:DateOct               -0.166667   0.188681  -0.883
    ## Blockb3:DateOct               -0.076667   0.188681  -0.406
    ## Blockb4:DateOct               -0.080000   0.188681  -0.424
    ## Blockb2:Treatmentcon          -0.143333   0.188681  -0.760
    ## Blockb3:Treatmentcon          -0.003333   0.188681  -0.018
    ## Blockb4:Treatmentcon          -0.313333   0.188681  -1.661
    ## DateAug:Treatmentcon          -0.066667   0.188681  -0.353
    ## DateJune:Treatmentcon         -0.006667   0.188681  -0.035
    ## DateOct:Treatmentcon          -0.243333   0.188681  -1.290
    ## Blockb2:DateAug:Treatmentcon   0.153333   0.266835   0.575
    ## Blockb3:DateAug:Treatmentcon  -0.390000   0.266835  -1.462
    ## Blockb4:DateAug:Treatmentcon   0.273333   0.266835   1.024
    ## Blockb2:DateJune:Treatmentcon -0.073333   0.266835  -0.275
    ## Blockb3:DateJune:Treatmentcon  0.326667   0.266835   1.224
    ## Blockb4:DateJune:Treatmentcon  0.366667   0.266835   1.374
    ## Blockb2:DateOct:Treatmentcon  -0.080000   0.266835  -0.300
    ## Blockb3:DateOct:Treatmentcon   0.090000   0.266835   0.337
    ## Blockb4:DateOct:Treatmentcon   0.003333   0.266835   0.012

    ## 
    ## Correlation matrix not shown by default, as p = 32 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##   vcov(x)     if you need it

Mixed-effects model with nested factor:

    lmer2<-lmer(MWD~Block*Date*Treatment+(1|Treatment:Transect.f), soil)
    summary(lmer2)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Block * Date * Treatment + (1 | Treatment:Transect.f)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -11.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.94982 -0.45801  0.07191  0.46131  1.97537 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev.
    ##  Treatment:Transect.f (Intercept) 0.001419 0.03767 
    ##  Residual                         0.027277 0.16516 
    ## Number of obs: 96, groups:  Treatment:Transect.f, 6
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error t value
    ## (Intercept)                    0.996667   0.097802  10.191
    ## Blockb2                        0.160000   0.134850   1.187
    ## Blockb3                       -0.123333   0.134850  -0.915
    ## Blockb4                        0.183333   0.134850   1.360
    ## DateAug                        0.500000   0.134850   3.708
    ## DateJune                       0.820000   0.134850   6.081
    ## DateOct                        0.696667   0.134850   5.166
    ## Treatmentcon                  -0.056667   0.138313  -0.410
    ## Blockb2:DateAug                0.126667   0.190707   0.664
    ## Blockb3:DateAug                0.543333   0.190707   2.849
    ## Blockb4:DateAug                0.243333   0.190707   1.276
    ## Blockb2:DateJune               0.063333   0.190707   0.332
    ## Blockb3:DateJune              -0.416667   0.190707  -2.185
    ## Blockb4:DateJune              -0.166667   0.190707  -0.874
    ## Blockb2:DateOct               -0.166667   0.190707  -0.874
    ## Blockb3:DateOct               -0.076667   0.190707  -0.402
    ## Blockb4:DateOct               -0.080000   0.190707  -0.419
    ## Blockb2:Treatmentcon          -0.143333   0.190707  -0.752
    ## Blockb3:Treatmentcon          -0.003333   0.190707  -0.017
    ## Blockb4:Treatmentcon          -0.313333   0.190707  -1.643
    ## DateAug:Treatmentcon          -0.066667   0.190707  -0.350
    ## DateJune:Treatmentcon         -0.006667   0.190707  -0.035
    ## DateOct:Treatmentcon          -0.243333   0.190707  -1.276
    ## Blockb2:DateAug:Treatmentcon   0.153333   0.269701   0.569
    ## Blockb3:DateAug:Treatmentcon  -0.390000   0.269701  -1.446
    ## Blockb4:DateAug:Treatmentcon   0.273333   0.269701   1.013
    ## Blockb2:DateJune:Treatmentcon -0.073333   0.269701  -0.272
    ## Blockb3:DateJune:Treatmentcon  0.326667   0.269701   1.211
    ## Blockb4:DateJune:Treatmentcon  0.366667   0.269701   1.360
    ## Blockb2:DateOct:Treatmentcon  -0.080000   0.269701  -0.297
    ## Blockb3:DateOct:Treatmentcon   0.090000   0.269701   0.334
    ## Blockb4:DateOct:Treatmentcon   0.003333   0.269701   0.012

    ## 
    ## Correlation matrix not shown by default, as p = 32 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##   vcov(x)     if you need it

Mixed-effects model with nested factor and repeated measurements:  
(I am not sure if I formulate the model I want with correct R syntax. I
am thinking about fixed block effect, fixed treatment effect, random
transect effect, transect factor nested within treatment factor,
transect was measured repeatedly over date.)

    lmer3<-lmer(MWD~Block*Date*Treatment+(Date|Treatment:Transect.f), soil)
    summary(lmer3)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MWD ~ Block * Date * Treatment + (Date | Treatment:Transect.f)
    ##    Data: soil
    ## 
    ## REML criterion at convergence: -19.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.23958 -0.43206  0.06591  0.54342  1.55277 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance  Std.Dev. Corr             
    ##  Treatment:Transect.f (Intercept) 3.959e-06 0.00199                   
    ##                       DateAug     4.139e-03 0.06434   0.68            
    ##                       DateJune    1.918e-02 0.13850   0.62  1.00      
    ##                       DateOct     1.992e-03 0.04464  -1.00 -0.69 -0.63
    ##  Residual                         2.228e-02 0.14926                   
    ## Number of obs: 96, groups:  Treatment:Transect.f, 6
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error t value
    ## (Intercept)                    0.996667   0.086182  11.565
    ## Blockb2                        0.160000   0.121869   1.313
    ## Blockb3                       -0.123333   0.121869  -1.012
    ## Blockb4                        0.183333   0.121869   1.504
    ## DateAug                        0.500000   0.127403   3.925
    ## DateJune                       0.820000   0.145761   5.626
    ## DateOct                        0.696667   0.124564   5.593
    ## Treatmentcon                  -0.056667   0.121879  -0.465
    ## Blockb2:DateAug                0.126667   0.172348   0.735
    ## Blockb3:DateAug                0.543333   0.172348   3.153
    ## Blockb4:DateAug                0.243333   0.172348   1.412
    ## Blockb2:DateJune               0.063333   0.172348   0.367
    ## Blockb3:DateJune              -0.416667   0.172348  -2.418
    ## Blockb4:DateJune              -0.166667   0.172348  -0.967
    ## Blockb2:DateOct               -0.166667   0.172348  -0.967
    ## Blockb3:DateOct               -0.076667   0.172348  -0.445
    ## Blockb4:DateOct               -0.080000   0.172348  -0.464
    ## Blockb2:Treatmentcon          -0.143333   0.172348  -0.832
    ## Blockb3:Treatmentcon          -0.003333   0.172348  -0.019
    ## Blockb4:Treatmentcon          -0.313333   0.172348  -1.818
    ## DateAug:Treatmentcon          -0.066667   0.180176  -0.370
    ## DateJune:Treatmentcon         -0.006667   0.206137  -0.032
    ## DateOct:Treatmentcon          -0.243333   0.176160  -1.381
    ## Blockb2:DateAug:Treatmentcon   0.153333   0.243737   0.629
    ## Blockb3:DateAug:Treatmentcon  -0.390000   0.243737  -1.600
    ## Blockb4:DateAug:Treatmentcon   0.273333   0.243737   1.121
    ## Blockb2:DateJune:Treatmentcon -0.073333   0.243737  -0.301
    ## Blockb3:DateJune:Treatmentcon  0.326667   0.243737   1.340
    ## Blockb4:DateJune:Treatmentcon  0.366667   0.243737   1.504
    ## Blockb2:DateOct:Treatmentcon  -0.080000   0.243737  -0.328
    ## Blockb3:DateOct:Treatmentcon   0.090000   0.243737   0.369
    ## Blockb4:DateOct:Treatmentcon   0.003333   0.243737   0.014

    ## 
    ## Correlation matrix not shown by default, as p = 32 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##   vcov(x)     if you need it

Comparing models:

    anova(lmer1,lmer3)

    ## refitting model(s) with ML (instead of REML)

    ## Data: soil
    ## Models:
    ## lmer1: MWD ~ Block * Date * Treatment + (1 | Transect.f)
    ## lmer3: MWD ~ Block * Date * Treatment + (Date | Treatment:Transect.f)
    ##       Df     AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
    ## lmer1 34 -42.640 44.548 55.320  -110.64                         
    ## lmer3 43 -34.516 75.751 60.258  -120.52 9.8756      9     0.3606

    anova(lmer2,lmer3)

    ## refitting model(s) with ML (instead of REML)

    ## Data: soil
    ## Models:
    ## lmer2: MWD ~ Block * Date * Treatment + (1 | Treatment:Transect.f)
    ## lmer3: MWD ~ Block * Date * Treatment + (Date | Treatment:Transect.f)
    ##       Df     AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
    ## lmer2 34 -40.620 46.568 54.310  -108.62                         
    ## lmer3 43 -34.516 75.751 60.258  -120.52 11.896      9     0.2192

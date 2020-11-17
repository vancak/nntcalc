### PANSS SIMULATION ###

set.seed(123)

n = 300
tr0 = round( rnorm(n, 96, 20), 0 )
co0 = round( rnorm(n, 96, 20), 0 )

# btr = rbinom(n, 1, prob = 0.6)
# bco = rbinom(n, 1, prob = 0.2)

# mean(tr0); mean(co0)
# 
# tr1 = round( ifelse(btr == 1, 0.5 * tr0, tr0), 0 )
# co1 = round( ifelse(bco == 1, 0.5 * co0, co0), 0 )

tr1 = 96 - 0.1 * tr0 ^ (1.2) + rnorm(n, 0, 20)
co1 = 96 - 0.01 * co0 ^ (1.2) + rnorm(n, 0, 20)

mean(tr1); sd(tr1) 
mean(co1); sd(co1)

out1 = tr1 - 0.7*tr0
out0 = tr0 - 0.7*co0

hist(out1, breaks = 100, col = "blue", freq = FALSE)
hist(out0, breaks = 100, col = "blue", freq = FALSE)
sd(out1)
sd(out0)

library(devtools)

# install_github("https://github.com/vancak/NNTcalculator")
library(nntcalc)

nnt_l( type      = "mle",
        treat     = out1,
        control   = out0,
        cutoff    = 0,
        equal.var = TRUE,
        dist      = "normal",
        decrease  = TRUE )

nnt_l( type      = "laupacis",
       treat     = out1,
       control   = out0,
       cutoff    = 0,
       dist      = "none",
       decrease  = TRUE )

nnt_l( type      = "fl",
       treat     = out1,
       control   = out0,
       cutoff    = 0,
       equal.var = TRUE,
       dist      = "normal",
       decrease  = TRUE )

PANSS_DAT = data.frame( outcome  = c(out1, out0),
                        baseline = c(tr0, co0),
                        group    = c(rep(1, n), rep(0, n)) )

nnt_x(  model    = "linreg",
        response = PANSS_DAT$outcome,
        x        = PANSS_DAT$baseline,
        cutoff   = 0,
        group    = PANSS_DAT$group,
        decrease = TRUE,
        adj      = 120,
        data     = PANSS_DAT )


######################################
### Cox-Exp CONTINUOUS X -BASELINE ###
######################################
lam_0   = 0.002
lam_cen = 0.004

# surv prob. increases with age and sat score
# female has larger surv prob. 
T_1 = rexp( n, rate = lam_0 * exp( - 7 + 0.1 * tr0  ) )
T_0 = rexp( n, rate = lam_0 * exp( - 5.5 + 0.1 * co0  ) )

mean(T_1);   mean(T_0)
median(T_1); median(T_0)

C_1 = rexp( n, rate = 1.5 * lam_cen )

obstime_1 = round( ifelse( T_1 <= C_1, T_1, C_1 ), 10)
status_1  = ifelse( T_1 <= C_1, 1, 0 )

mean(status_1)
mean(obstime_1)

C_0 = rexp( n, rate = 50 * lam_cen)  

obstime_0 = round( ifelse( T_0 <= C_0, T_0, C_0 ), 10)
status_0  = ifelse( T_0 <= C_0, 1, 0 )

mean(status_0)
mean(obstime_0)

library(survival)

panss_surv = data.frame( time     = round( c( obstime_1, obstime_0 ), 4),
                         status   = c( status_1,  status_0  ),
                         group    = c( rep(1, n), rep(0, n) ),
                         baseline = c(tr0, co0), 
                         outcome  = round( c(out1, out0), 0 ) )


nnt_survreg( response = panss_surv$time,
             status   = panss_surv$status,
             x        = panss_surv$baseline,
             group    = panss_surv$group,
             adj      = 100,
             time.point = 2,
             data     = panss_surv )

###########
bb = read.csv("panss_surv.csv")

nnt_survreg( response = bb$time,
             status   = bb$status,
             x        = bb$baseline,
             group    = bb$group,
             adj      = 80,
             time.point = 7,
             data     = bb )


nnt_l( type      = "mle",
       treat     = panss_surv[panss_surv$group == 1, "outcome"],
       control   = panss_surv[panss_surv$group == 0, "outcome"],
       cutoff    = 0,
       equal.var = TRUE,
       dist      = "normal",
       decrease  = TRUE )


nnt_l( type      = "laupacis",
       treat     = panss_surv[panss_surv$group == 1, "outcome"],
       control   = panss_surv[panss_surv$group == 0, "outcome"],
       cutoff    = 0,
       equal.var = TRUE,
       decrease  = TRUE )


nnt_l( type      = "fl",
       treat     = panss_surv[panss_surv$group == 1, "outcome"],
       control   = panss_surv[panss_surv$group == 0, "outcome"],
       cutoff    = 0,
       equal.var = TRUE,
       dist      = "normal",
       decrease  = TRUE )


nnt_x(  model    = "linreg",
        response = panss_surv$outcome,
        x        = panss_surv$baseline,
        cutoff   = 0,
        group    = panss_surv$group,
        decrease = TRUE,
        adj      = 120,
        data     = panss_surv )

write.csv( panss_surv, file = "panss_surv.csv", row.names = FALSE )

write.csv( data.frame( out_tr  = out1,
                       out_co  = out0, 
                       base_tr = tr0, 
                       base_co = co0),
           file      = "panss_fl.csv",
           row.names = FALSE )
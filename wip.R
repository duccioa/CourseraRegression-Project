par(mfrow = c(2,6))
plot(wt,mpg, col = am)
plot(cyl, mpg, col = am)
plot(disp, mpg)
plot(hp, mpg)
plot(drat, mpg)
plot(wt, mpg)
plot(qsec, mpg)
plot(vs, mpg)
plot(am, mpg)
plot(gear, mpg)
plot(carb, mpg)

mpg <- mtcars$mpg
wt <- mtcars$wt
cyl <- mtcars$cyl
am <- mtcars$am
df <- data.frame(mpg, wt, cyl, am)

g1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(aes(colour = am)) + geom_smooth(method = "lm", aes(group = 1))
g2 <- ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point(aes(colour = am)) + geom_smooth(method = "lm", aes(group = 1))
g3 <- ggplot(df, aes(x = am, y = mpg)) + geom_point(aes(colour = am)) + geom_smooth(method = "lm", aes(group = 1))




plot_graph <- function() {
    
    names_var <- names(mtcars)
    names_var <- names_var[names_var != "mpg"]
    g <- list()
    for(i in names_var){
        g[[i]] <- ggplot(mtcars, aes(x = mtcars[,i], y = mpg)) + 
            geom_point(data = mtcars, aes(colour = factor(am))) + 
            geom_smooth(method = "lm", aes(group = 1))
        
        
    }
    g
}




test <- list()
test[[1]] <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(data = mtcars, aes(colour = factor(am))) + geom_smooth(method = "lm", aes(group = 1))
test[[2]] <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(data = mtcars, aes(colour = factor(am))) + geom_smooth(method = "lm", aes(group = 1))
test[[3]] <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(data = mtcars, aes(colour = factor(am))) + geom_smooth(method = "lm", aes(group = 1))



plot_ggplot <- function(dataset, predictor, response, col){
    ggplot(dataset, aes(x = predictor, y = response)) + 
        geom_point(data = dataset, aes(colour = factor(col))) + 
        geom_smooth(method = "lm", aes(group = 1))
}


plot_ggplot <- function(predictor){
    print(ggplot(mtcars, aes(x = predictor, y = mpg)) + 
        geom_point() + 
        geom_smooth(method = "lm", aes(group = 1)))
}
lapply(colnames(mtcars), plot_ggplot)


attach(mtcars)
df <- data.frame(names = row.names(mtcars), mpg = mpg, cyl = factor(cyl), disp = disp, hp = hp, drat = drat, wt = wt, qsec = qsec, vs = factor(vs), am = factor(am), gear = factor(gear), carb = factor(carb))
detach(mtcars)

doPlot = function(car_name) {
    dum <- df[df$names == car_name,]
    ggobj = ggplot(data = dum, aes(type, expenses)) + geom_bar()
    print(ggobj)
    ggsave(sprintf("%s.pdf", sel_name))
}
lapply(unique(df$name), doPlot)
########## V2.0 ############################################

plot_graph <- function() {
    require(dplyr)
    require(ggplot2)
    names_var <- names(select(mtcars, -mpg))
    out <- NULL
    g <- ggplot(mtcars, aes(y = mpg))
    for(i in 1:length(names_var)){
       
        g <- g + aes_string(x = names_var[i]) + geom_point(aes(colour = factor(am))) + geom_smooth(method = "lm", aes(group = 1))
        print(g)
        out[[i]] <- g
    }
    out <<- out
}


names_var <- names(mtcars[, -1])
for(i in 1:length(names_var)){
    g <- ggplot(mtcars, aes(x = mtcars[, i], y = mpg)) + 
        geom_point(aes(colour = factor(am))) + 
        geom_smooth(method = "lm", aes(group = 1))+
        xlab(names_var[i])
    print(g)
}


####### v 3.0 ######



   
names_var <- names(select(mtcars, -mpg))
out <- NULL
g <- ggplot(mtcars, aes(y = mpg))
for(i in 1:length(names_var)){
    
    g <- g + aes_string(x = names_var[i]) + geom_point(aes(colour = factor(am))) + geom_smooth(method = "lm", aes(group = 1))
    
    out[[i]] <- g
}
grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]], nrow = 3)



###exercise from the book
data("Seatbelts")
df <- as.data.frame(Seatbelts)
df$outcome[df$DriversKilled > 119] <- 1
df$outcome[df$DriversKilled <= 119] <- 0
df_glm <- glm(outcome ~ kms + PetrolPrice + law, family = "binomial", data = df)
plot(df_glm)

#####################################
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl); mtcars$vs <- factor(mtcars$vs) 
mtcars$am <- factor(mtcars$am); mtcars$gear <- factor(mtcars$gear) 
mtcars$carb <- factor(mtcars$carb)

fit_all <- lm(mpg ~ ., mtcars)
summary(fit_all)
fit_am <- lm(mpg ~ am, mtcars)
summary(fit_am)
fit_am_wt <- lm(mpg ~ am + wt, mtcars)
summary(fit_am_wt)
fit_am_wt_drat <- lm(mpg ~ am + wt + drat, mtcars)
summary(fit_am_wt_drat)
fit_am_wt_drat_qsec <- lm(mpg ~ am + wt + drat + qsec, mtcars)
summary(fit_am_wt_drat_qsec)
fit_am_wt_drat_qsec_gear <- lm(mpg ~ am + wt + drat + qsec + gear, mtcars)
summary(fit_am_wt_drat_qsec_gear)
anova(fit_am, fit_am_wt, fit_am_wt_drat, fit_am_wt_drat_qsec, fit_am_wt_drat_qsec_gear)


fit_all <- lm(mpg ~ ., mtcars)
summary(fit_all)
fit_am <- lm(mpg ~ am, mtcars)
summary(fit_am)
fit_am_wt <- update(fit_am, mpg ~ am + wt)
summary(fit_am_wt)
fit_am_wt_drat <- update(fit_am_wt, mpg ~ am + wt + drat)
summary(fit_am_wt_drat)
#fit_am_wt_qsec <- update(fit_am_wt, mpg ~ am + wt + qsec)
#summary(fit_am_wt_qsec)

anova(fit_am, fit_am_wt, fit_am_wt_qsec)

fit_am_wt_drat_qsec <- update(fit_am_wt_drat, mpg ~ am + wt + drat + qsec)
anova(fit_am, fit_am_wt, fit_am_wt_qsec, fit_am_wt_drat_qsec)
fit_am_wt_drat_qsec_cyl <- update(fit_am_wt_drat, mpg ~ am + wt + drat + qsec + cyl)
anova(fit_am, fit_am_wt, fit_am_wt_qsec, fit_am_wt_drat_qsec, fit_am_wt_drat_qsec_cyl)

fit1 <- update(fit_am, mpg ~ am + wt)
fit2 <- update(fit_am, mpg ~ am + wt + cyl)
fit3 <- update(fit_am, mpg ~ am + wt + cyl + disp)
fit4 <- update(fit_am, mpg ~ am + wt + cyl + disp + hp)
fit5 <- update(fit_am, mpg ~ am + wt + cyl + disp + hp + drat)
fit6 <- update(fit_am, mpg ~ am + wt + cyl + disp + hp + drat + qsec)
fit7 <- update(fit_am, mpg ~ am + wt + cyl + disp + hp + drat + qsec + vs)
fit8 <- update(fit_am, mpg ~ am + wt + cyl + disp + hp + drat + qsec + vs + gear)
fit9 <- update(fit_am, mpg ~ am + wt + cyl + disp + hp + drat + qsec + vs + gear + carb)
anova(fit_am, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9)



###after literature review
fit <- lm(mpg ~ wt, mtcars)
summary(fit)
fit1 <- update(fit, mpg ~ wt + cyl)
summary(fit1)
fit2 <- update(fit, mpg ~ wt + cyl + am)
summary(fit2)

###
fit_interaction <- lm(mpg ~ am + wt + am*wt, mtcars)
summary(fit_interaction)
plot(mtcars$wt, mtcars$mpg, col = as.factor(mtcars$am))
abline(fit_interaction$coef[1],  fit_interaction$coef[3])
abline(fit_interaction$coef[1]+fit_interaction$coef[2],  fit_interaction$coef[3]+fit_interaction$coef[4], col = "red")


#install.packages("randomizr")
#install.packages("estimatr")
#install.packages("ri2")
#install.packages("tidyverse")

library(randomizr)
library(estimatr)
library(ri2)
library(tidyverse)

rm(list=ls())

setwd("~/Dropbox/Teaching LSE/Workshops/Mannheim/Application_RI")

#Let's take the example from the white board

dat<-data.frame(Z = c(1, 0, 1, 0, 0, 1), Y=c(4.5, 5, 4.5, 4.5, 4, 6))

Z <- dat$Z
Y <- dat$Y

#DIM
ate_hat_dim <- difference_in_means(Y ~ Z, data = dat)
tidy(ate_hat_dim)

# Randomization Inference -------------------------------------------------

# Write your own loop

# Step 1: Make Hypothesized Outcomes under sharp null hypothesis

dat <-
  dat %>%
  mutate(
    Y0_star = Y,
    Y1_star = Y
  )


# exactly zero treatment effect for every unit
with(dat, Y1_star - Y0_star)

# Step 2: set up loop

sims <- 20
simulated_ates <- rep(NA, sims)
set.seed(123)

?complete_ra

for (i in 1:sims) {
  # Do a random assignment
  dat <-
    dat %>%
    mutate(
      Z_sim = complete_ra(N = 6, m = 3),
      Y_sim = Y1_star * Z_sim + Y0_star * (1 - Z_sim)
    )
  
  fit_sim <- difference_in_means(Y_sim ~ Z_sim, data = dat)
  simulated_ates[i] <- fit_sim$coefficients
  
}

# Step 3: take a look at the null distribution and compare it to the observed value

ate_hat_obs <- ate_hat_dim$coefficients

qplot(simulated_ates) + geom_vline(xintercept = ate_hat_obs, color = "red")

p_value_ri <- mean(abs(simulated_ates) >= ate_hat_dim$coefficients)
p_value_ri

p_value_ri_greater <- mean((simulated_ates) >= ate_hat_dim$coefficients)
p_value_ri_greater 


#Using ri2

?conduct_ri

set.seed(123) #set random number seed

ra_declaration <- declare_ra(N = 6, m = 3) #declare your random assignment

ra_declaration

prob_mat <- ra_declaration$probabilities_matrix
head(prob_mat)

fit_ri <- conduct_ri(Y ~ Z, declaration = ra_declaration, sims=20, data = dat) #conduct all possible assignments and DiM under the sharp null

plot(fit_ri) #plot the sampling distribution of the simulated ATEs
summary(fit_ri) #two-tailed p-value

plot(fit_ri,p = "upper")
summary(fit_ri, p = "upper") #one-tailed p-value

#save plots

pdf(paste("ri2_graph.pdf"),w=6,h=5)
plot(fit_ri)
dev.off()

png("ri2_graph.png")
plot(fit_ri)
dev.off()


#Use replication data from Foos and de Rooij (AJPS, 2017)

#install.packages(readstata13)
library(readstata13)

dat2<-read.dta13("canvassing.dta")

dat2 <-
  dat2 %>%
  mutate(
    Y = ifelse(turnout == "voted", 1, 0),
    blocks=pid,
    Z=treatment)

with(dat2, table(blocks, Z))

block_m<- with(dat2, tapply(Z,blocks,sum))

set.seed(123) #set random number seed

ra_declaration2 <- declare_ra(N = 3848, blocks=dat2$blocks, block_m = block_m) #declare block random assignment

ra_declaration2 #check random assignment

fit_ri2 <- conduct_ri(Y ~ Z, declaration = ra_declaration2, sims=5000, IPW=TRUE, data = dat2) #conduct randomization inference

plot(fit_ri2) #plot the sampling distribution of the simulated ITTs
summary(fit_ri2) #two-tailed p-value
summary(fit_ri2, p = "upper") #one-tailed p-value

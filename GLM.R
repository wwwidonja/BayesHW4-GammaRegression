# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(posterior)
library(mcmcse)
library(ggdist)
library(caret)
library(dplyr)
library(cowplot)
library(ggthemes)
library(RColorBrewer)
# Initial data exploration -----------------------------------------------------


#Import the data 
data <- read.csv("./data/videogame_sales.csv")
data


## Draw gridplot, showing the distributions of different combinations
# on the log scale.

ggplot(data=data, aes(x=Sales, fill=Platform))+
  geom_density() +
  facet_grid(Genre~Platform) +scale_x_log10()+
  theme_fivethirtyeight() +
  scale_fill_fivethirtyeight()+
  theme(legend.position="none",
        axis.title = element_text(),
        strip.text = element_text(size=10, face='bold'),
        strip.placement = "outside") + ggtitle('Observed distribution of Sales') + xlab('Sales (log scale)')

#Define a counter helper function, used in the plot
give.n <- function(x){
  return(c(y = -0.3, label = length(x))) 
}

#Draw violin plot, showing initial distribution of data between
#genres and platforms
ggplot(data=data, aes(x = Genre, y = Sales, fill = Genre)) + 
  geom_jitter(aes(color=Genre), height = 0, width = 0.3, alpha=0.5)+
  geom_violin(scale="count")+
  ylim(-0.3, 7.7)+
  facet_wrap(~Platform, strip.position="bottom") +
  stat_summary(fun.data=give.n, geom="text", size=5, position=position_dodge(width=0.75))+
  theme_fivethirtyeight() +
  scale_fill_fivethirtyeight()+
  scale_color_fivethirtyeight()+
  theme(legend.position="none",
                                      axis.title = element_text(),
                                      strip.text = element_text(size=10, face='bold'),
                                      strip.placement = "outside") + ggtitle('Observed distribution of Sales') + xlab('Genre/Platform')


## Modelling and data prep -----------------------------------------------------

# Get 2 models
model_genre <- cmdstan_model("./models/gamma_genre.stan")
model_platform <- cmdstan_model("./models/gamma_platform.stan")

#one-hot encode the categoric variables
nd <- dummyVars(" ~ .", data=data)
final_df <- data.frame(predict(nd, newdata=data))

#extract the columns needed for each model
data_platform <- final_df %>% select(PlatformPS, PlatformXbox, Sales)
data_genre <- final_df %>% select(GenreShooter, Sales)



### Sampling for platform
sd_platform <- list(n = length(data_platform$Sales), sales = data_platform$Sales, Xbox=data_platform$PlatformXbox, PS=data_platform$PlatformPS)
fit_platform <- model_platform$sample(
  data = sd_platform,
  seed = 1
)

## Diagnostics: Due to large amount of \mu parameters, we examine each mcmc_trace plot
#on its own. Please change "intercept" for parameters of interest.
mcmc_trace(fit_platform$draws("intercept"))
fit$summary()

#### Sampling for GENRE
sd_genre <- list(n = length(data_genre$Sales), sales = data_genre$Sales, isShooter=data_genre$GenreShooter)
fit_genre <- model_genre$sample(
  data = sd_genre,
  seed = 1
)

## Diagnostics: Due to large amount of \mu parameters, we examine each mcmc_trace plot
#on its own. Please change "intercept" for parameters of interest.
mcmc_trace(fit_genre$draws("intercept"))
fit$summary()


# Calculate the \mu parameters. In instances where the column was not selected,
# in the model, we only take the intercept.
plat_xbone <- exp(fit_platform$draws("intercept") + fit_platform$draws("b_xbox"))
plat_ps <- exp(fit_platform$draws("intercept") +fit_platform$draws("b_ps"))
plat_pc <- exp(fit_platform$draws("intercept"))
genre_shoot <- exp(fit_genre$draws("intercept") +fit_genre$draws("b_isShooter"))
genre_rpg <- exp(fit_genre$draws("intercept"))

# Make dataframes
df_genre <- as_draws_df(fit_genre$draws())
df_platform <- as_draws_df(fit_platform$draws())

### Get posterior distributions

platform <- ggplot() + geom_density(aes(x=plat_xbone, color='XBox'), size=1, linetype=2) +
  geom_density(aes(x=plat_ps, color='PS'), size=1, linetype=2)+
  geom_density(aes(x=plat_pc, color='PC'), size=1, linetype=2)+
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight()+
  theme(axis.title = element_text()) + xlab(expression(mu)) +
  ggtitle('Platform importance')

genre <- ggplot() + geom_density(aes(x=genre_shoot, color='Shooter'), size=1, linetype=2) +
  geom_density(aes(x=genre_rpg, color='RPG'), size=1, linetype=2) +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight()+
  theme(axis.title = element_text()) + xlab(expression(mu)) +
  ggtitle('Genre importance')

plot_grid(platform, genre, ncol=2)

### calculate confidence in our hypothesised statements:

#s1: Xbox is most profitable
xbox_importance <- mcse(
  plat_xbone > plat_ps 
  & plat_xbone > plat_pc
  )

#s2: PC is least profitable
pc_bad <- mcse(
  plat_pc < plat_xbone 
    & plat_pc < plat_ps
  )

#s3: Shooters are more profitable than RPGs
shoot_good <- mcse(
  genre_shoot > genre_rpg
)

#s4: Xbox shooters are the most profitable.
best <- mcse(
  genre_shoot > genre_rpg 
  & plat_pc < plat_xbone 
  & plat_ps < plat_xbone 
)

#Put our solutons into a dataframe for simpled drawing.
dfx <- data.frame(est=c(xbox_importance$est, shoot_good$est, pc_bad$est, best$est),
                  se=c(xbox_importance$se, shoot_good$se, pc_bad$se, best$se),
                  statements = c("Xbox is the most \nprofitable platform",'Shooters are more\n profitable than RPGs',
                                 'PC is the least \nprofitable platform',
                                 'Shooters on Xbox\n
                                 are the most profitable'))

###OPTIONAL: Override dataframe with 'CX' labels as seen in report.
dfx <- data.frame(est=c(xbox_importance$est, shoot_good$est, pc_bad$est, best$est),
                  se=c(xbox_importance$se, shoot_good$se, pc_bad$se, best$se),
                  statements = c("C2",'C3',
                                 'C1',
                                 'C4'))



# Define the number of wanted colors (due to scale before not having enough colors)
nb.cols <- 4
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)


#Plot our confidence
ggplot(data=dfx) + geom_col(aes(y=reorder(statements,est), x=est, fill=statements)) +
  
  geom_errorbarh(aes(xmin=est-se, xmax=est+se, y=statements,), height=0.2, size=1, linetype=1, position=position_dodge(0.9), ) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=mycolors)+ 
  theme(legend.position="none",
                                       axis.title = element_text()) + ylab('Statements') + xlab('Confidence') + ggtitle('Quantification of conclusion certainty')
  

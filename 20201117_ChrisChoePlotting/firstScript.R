library(ggplot2)
library(Hmisc)
library(readr)
TACMIF <- read_csv("Documents/Mouse TAC MI/TACMIF.csv")

###Helper Function to calculate standard deviation, mean

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
##End Helper Function
calcType <- data_summary(TACMIF, varname="HR",
                         groupnames=c("Type", "Sex"))

head(calcType)

##Generate pointrange

##ggplot(calcType, aes(x=Sex, y=HR, group=Type, color=Type)) +
##  geom_pointrange(aes(ymin=HR-sd, ymax=HR+sd))

##Generate HR

HR <- ggplot(TACMIF, aes(x=Type, y=HR)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 700)

HR + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate BW

BW <- ggplot(TACMIF, aes(x=Type, y=BW)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 30)

BW + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate HW

HW <- ggplot(TACMIF, aes(x=Type, y=HW)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 0.2)

HW + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate HWR

HWR <- ggplot(TACMIF, aes(x=Type, y=HWR)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 30)

HWR + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate EDV

EDV <- ggplot(TACMIF, aes(x=Type, y=EDV)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 120)

EDV + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate ESV

ESV <- ggplot(TACMIF, aes(x=Type, y=ESV)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 80)

ESV + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate EF

EF <- ggplot(TACMIF, aes(x=Type, y=EF)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 100)

EF + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate BSA

BSA <- ggplot(TACMIF, aes(x=Type, y=BSA)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 0.01)

BSA + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate SV

SV <- ggplot(TACMIF, aes(x=Type, y=SV)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0,40)

SV + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate SVI

SVI <- ggplot(TACMIF, aes(x=Type, y=SVI)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 4500)

SVI + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate CO

CO <- ggplot(TACMIF, aes(x=Type, y=CO)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 25)

CO + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate CI

CI <- ggplot(TACMIF, aes(x=Type, y=CI)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 2750)

CI + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate VTI

VTI <- ggplot(TACMIF, aes(x=Type, y=VTI)) +
  geom_dotplot(binaxis='y', stackdir='center') + ylim(0, 350)

VTI + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                   geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate Peak_Vel

Peak_Vel <- ggplot(TACMIF, aes(x=Type, y=Peak_Vel)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 10)

Peak_Vel + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                        geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate Peak_Grad

Peak_Grad <- ggplot(TACMIF, aes(x=Type, y=Peak_Grad)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, )

Peak_Grad + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                         geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Generate Tibia
##library(dplyr)
##Tibia %>% arrange(Type) %>% mutate(name = factor(name, levels = c("Sham-Het", "HET", "Sham-WT", "WT"))) %>%
##  ggplot(TACMIF, aes(x=Type, y=Tibia)) + geom_dotplot(binaxis='y', stackdir='center')

##Tibia + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
##                 	geom="errorbar", color="red", width=0.2) +
##  stat_summary(fun.y=mean, geom="point", color="red")

Tibia <- ggplot(TACMIF, aes(x=Type, y=Tibia)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 20)

Tibia + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                     geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="Documents/MouseTacYChangeF/")



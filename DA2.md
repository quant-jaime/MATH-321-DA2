# MATH-321-DA2

# CHAPTER 13 COMPLETE BLOCK DESIGN 

#importing data
count = c( 86, 90, 88, 87, 98, 94, 93, 89, 96, 90, 91, 92, 97, 95, 91,92, 91, 93, 95, 95 )
count

treat0 = c( "Control", "Arasan", "Spergon", "Semesan", "Fermate" )
treat0


treatment = factor( rep( treat0, each=4 ) )
treatment
block = factor(rep(1:4, 5))
block

seed = data.frame(count, treatment, block)
seed


library(lattice)
library(ggplot2)

## help get the xyplot, incase it doesnt work
dev.off()

## for each block, construct the scatterplot of count vs treatment
xyplot(count ~ treatment)

## combining all blocks of count vs treatment 
xyplot(count ~ factor(treatment, levels = treat0 ))


model = aov(count ~ treatment + block)
summary(model)

levels(treatment)

#which seed disinfectants are stat sig different from the cotrol
# column two, is the control. so we add (-1)
# all other ones are +1

#"Arasan"  "Control" "Fermate" "Semesan" "Spergon"
w1 = c(1,-1,0,0,0)
w2 = c(0,-1,1,0,0)
w3 = c(0,-1,0,1,0)
w4 = c(0,-1,0,0,1)

#under fixed effect model, test for treatment effect
# use fit.contrast 

fit.contrast(model, 'treatment', w1)
fit.contrast(model, 'treatment', w2)
fit.contrast(model, 'treatment', w3)
fit.contrast(model, 'treatment', w4)


# construct treatment means using model.tables
# defines grand mean/for each varaible 
model.tables(model, 'means')

# construct 95% CI on the treatment means
emmeans(model, 'treatment')

#construct the fitted vs residuals plot 
# not alot of evidence of heter 
plot(model, which = 1)

levels(treatment)

model(treatment, 'block')


# test for a stat sig effect on the difference between the control vs all varaibles 
w = c(1/4,1,1/4, 1/4, 1/4)
fit.contrast(model, 'treatment', w)

model = aov(count ~ treatment + block)
summary(model)

#treatment and block are both fixed-effect 
model1 = aov(count ~ treatment*block)
summary(model1)

#latin sqaures, each observation appears in each row/column once
# good for only one treatment level, use for combination of blocking levels

#standard latin square 
latin(4, random = FALSE)

# random 

latin(4)


# (g) cows are fixed
# standard latin square = latin(4, random = FALSE)
# (e) trying to find a treatment effect, we dont hope to
#have a row/column effect. we dont hope to see a difference there


########

library(daewr)
b = bioeqv

#Latin Square Design


Subject = b$Subject
Period = b$Period
Treat = b$Treat
AUC = b$AUC

is.factor(Subject)
is.factor(Period)
is.factor(Treat)
#a) do the subject represent random sample form some population?
#NO

#b)Algebracily determine the DOF for anova table
#g = 3, g-1 = 2

#d.f for estimating VAR is (g-1)(g-2) = (3-1)(2-1) = 2

#c) all fixed effect 
model = aov(AUC ~ Treat + Subject + Period)
summary(model)

## determine the treatment means, using the model.tables function
model.tables(model, 'means')

#construct 95% CI on the treatment means
library(emmeans)
emmeans(model, "Treat")

#construct 95% simultaneous CI on each difference between two
#treatment means
TukeyHSD(model, 'Treat')

plotDist('df', 2, 2)

#f-value of Subject
shadeDist(0.258, 'df', 2,2, lower.tail = FALSE)
pf(0.258, 2,2, lower.tail = FALSE)




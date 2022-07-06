rm(list=ls())

### Set working directory

#setwd("D:/Dropbox/Bolivia_Project/chequea bolivia long")
setwd('~/cam.blanes Dropbox/Camila Blanes/Bolivia_Project/chequea bolivia long/')
options("scipen"=15)



### Load R packages

library(foreign)
library(readstata13)
library(RItools)
library(blockTools)
library(nbpMatching)
library(dplyr)
library(tidyverse)
library(broom)

### Read in the data to randomize, label each dataset, and combine as a single "citizens" dataset
data <- read_csv('processed_data.csv') %>% select(-X1) %>%
  filter(NUMERO != 791) %>%
  filter(NUMERO != 790)


cov1 <- c('index_casos', 'caracteristica_falsa', 'index_genero',
          'index_conocimiento_falso', 'index_verificacion', 
          'index_exposure', 'index_demographics')

### Determine blocks of different sizes; force separate blocking by "type" and then pick blocks
### to minimize the Mahalanobis distance between individuals (denoted by "NUMERO") 
### according to the covariates in the variable list "cov"; the "optGreedy" algorithm specifies 
### how the blocks are chosen, with this algorithm picking the most similar cases to form the 
### first block and so on; other algorithm are available (e.g. "optimal"), but often only work 
### for binary treatment assignments. 

# Blocks

block1 <- block(data, n.tr = 32, id.vars = c("NUMERO"), block.vars = cov1, 
                algorithm="optGreedy", distance = "mahalanobis", #groups = "type",
                valid.range = c(0,500), verbose = F); block

blockid1 <- createBlockIDs(block1, data, id.var = "NUMERO"); blockid1
citizens1 <- cbind(data, blockid1)

# counts <- citizens1 %>%
#   group_by(blockid1)%>%
#   count()
# 
# citizens1 <- citizens1 %>%
#   filter(blockid1 != 16 )

block2 <- block(citizens1, n.tr =16, id.vars = c("NUMERO"), block.vars = cov1, 
                algorithm="optGreedy", distance = "mahalanobis", groups = c('blockid1'),
                valid.range = c(0,500), verbose = F); block
blockid2 <- createBlockIDs(block2, citizens1, id.var = "NUMERO"); 
citizens2 <- cbind(citizens1, blockid2)

block3 <- block(citizens2, n.tr =8, id.vars = c("NUMERO"), block.vars = cov1, 
                algorithm="optGreedy", distance = "mahalanobis", groups = c('blockid2'),
                valid.range = c(0,500), verbose = F); block
blockid3 <- createBlockIDs(block3, citizens2, id.var = "NUMERO"); 
citizens3 <- cbind(citizens2, blockid3)

block4 <- block(citizens3, n.tr =4, id.vars = c("NUMERO"), block.vars = cov1, 
                algorithm="optGreedy", distance = "mahalanobis", groups = c('blockid3'),
                valid.range = c(0,500), verbose = F); block
blockid4 <- createBlockIDs(block4, citizens3, id.var = "NUMERO"); 
citizens4 <- cbind(citizens3, blockid4)

### Randomize treatment assignments within each block
assign2 <- assignment(block4, seed = 987654321); assign2
# assign2 <- assignment(block2, seed = 987654321); assign2
# assign3 <- assignment(block3, seed = 12345); assign3



### Extract individual treatment assignments, labelling treatment 0,1,2,3 in our case of 4 treatments

# Blocks of 
treatments1 <- c()
for (i in unique(citizens4$blockid4)) {
  end <- eval(parse(text=paste("assign2$assg$`",i,"`", sep="")))
  treatments1 <- rbind(treatments1, end)
} ; treatments1

assignments1 <- rbind(cbind(as.vector(treatments1[,1]),0), 
                      cbind(as.vector(treatments1[,2]),1), 
                      cbind(as.vector(treatments1[,3]),2), 
                      cbind(as.vector(treatments1[,4]),3))

assignments1 <- na.omit(assignments1)
colnames(assignments1) <- c("NUMERO", "treatment"); assignments1


### Put everything together

# Blocks of 16 individuals
journalist_randomization1 <- as.data.frame(merge(citizens4, assignments1, id="NUMERO"))
journalist_randomization1 <- journalist_randomization1[,c("NUMERO", "blockid1",
                                                          "blockid2",
                                                          "blockid3",
                                                          "blockid4","treatment")]

dim(journalist_randomization1)
table(journalist_randomization1$blockid1)
table(journalist_randomization1$blockid2)
table(journalist_randomization1$blockid3)
table(journalist_randomization1$blockid4)
table(journalist_randomization1$treatment)

### Write as Stata file
write.dta(journalist_randomization1, "within_blocks16_type1.dta")


############################## 
##### Randomization for type 2
##############################
stata_file <- read.dta13('../citizen survey/Responses/Data_First_Randomization_Locked.dta')

data_type2 <- stata_file %>%
  filter(February8 == "Only Chequea") %>%
  select(startdate:envio_mensaje, 
         WhatsApp, 
         February8, 
         file_source,
         NUMERO,
         FECHA)

cov2 <- c('FECHA')

data_type2 <- data_type2[1:304,]

block1 <- block(data_type2, n.tr = 32, id.vars = c("NUMERO"), block.vars = cov2, 
                algorithm="optGreedy", distance = "mahalanobis", #groups = "type",
                valid.range = c(0,500), verbose = F); block
blockid1 <- createBlockIDs(block1, data_type2, id.var = "NUMERO"); blockid1
citizens1 <- cbind(data_type2, blockid1)

block2 <- block(citizens1, n.tr = 16, id.vars = c("NUMERO"), block.vars = cov2, 
                algorithm="optGreedy", distance = "mahalanobis", groups = c('blockid1'),
                valid.range = c(0,500), verbose = F); block
blockid2 <- createBlockIDs(block2, citizens1, id.var = "NUMERO");  blockid2
citizens2 <- cbind(citizens1, blockid2)

block3 <- block(citizens2, n.tr = 8, id.vars = c("NUMERO"), block.vars = cov2, 
                algorithm="optGreedy", distance = "mahalanobis", groups = c('blockid2'),
                valid.range = c(0,500), verbose = F); block
blockid3 <- createBlockIDs(block3, citizens2, id.var = "NUMERO");  blockid3
citizens3 <- cbind(citizens2, blockid3)

block4 <- block(citizens3, n.tr = 4, id.vars = c("NUMERO"), block.vars = cov2, 
                algorithm="optGreedy", distance = "mahalanobis", groups = c('blockid3'),
                valid.range = c(0,500), verbose = F); block
blockid4 <- createBlockIDs(block4, citizens3, id.var = "NUMERO");  blockid4
citizens4 <- cbind(citizens3, blockid4)

assign1 <- assignment(block4, seed = 987654321); assign1

treatments1 <- c()
for (i in unique(citizens4$blockid4)) {
  end <- eval(parse(text=paste("assign1$assg$`",i,"`", sep="")))
  treatments1 <- rbind(treatments1, end)
} ; treatments1

assignments1 <- rbind(cbind(as.vector(treatments1[,1]),0), 
                      cbind(as.vector(treatments1[,2]),1), 
                      cbind(as.vector(treatments1[,3]),2), 
                      cbind(as.vector(treatments1[,4]),3))
assignments1 <- na.omit(assignments1)
colnames(assignments1) <- c("NUMERO", "treatment"); assignments1

journalist_randomization1 <- as.data.frame(merge(citizens4, assignments1, id="NUMERO"))
journalist_randomization1 <- journalist_randomization1[,c("NUMERO", "blockid1",
                                                          "blockid2",
                                                          "blockid3","blockid4","treatment")]
table(journalist_randomization1$blockid1)
table(journalist_randomization1$blockid2)
table(journalist_randomization1$blockid3)
table(journalist_randomization1$blockid4)
table(journalist_randomization1$treatment)

write.dta(journalist_randomization1, "16_within_blocks_type2.dta")

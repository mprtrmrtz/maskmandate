
rm(list = ls())
setwd('/Users/mortezamaleki/Library/CloudStorage/GoogleDrive-mprtrmrtz@gmail.com/My Drive/Research/Mask Mandate/')
df <- fread("Data/ToMerge/df.csv")

# EL-IN vs EL-MC relationship: Stage 1

stage_1_model_1 <- lm(data = df, df$`EL-MC` ~ df$`EL-IN`)
summary(stage_1_model_1)

stage_1_model_2 <- lm(data = df, df$`EL-MC` ~ df$`EL-IN` + 
                        `PP-DW`  +   `PS-PD`+
                        `C19-CC`  + 
                        `HC-PW`  + 
                        `EC-PO`  + 
                        `HC-HO`  + 
                        `EC-UN`)
summary(stage_1_model_2)


stargazer(stage_1_model_1,stage_1_model_2, type = "latex", 
          omit = c("EC-UN", "HC-HO", "EC-PO", "HC-PW", "C19-CC", "PS-PD", "PP-DW"), 
          dep.var.labels = "Education Levels (EL-MC)", 
          add.lines = list(c("Included Controls", "No", "Yes")),
          covariate.labels = c('EL-IN', "Constant"), 
          omit.stat = c('n'), 
          notes = "Control variables were removed from Model 2 for clarification"
          )






#*******************************************************************************




summary(lm(data = df, df$`C19-MC` ~ df$`EL-IN` + 
             `PP-DW`  +   `PS-PD`+ df$`EL-MC` +
             `C19-CC`  + 
             `HC-PW`  + 
             `EC-PO`  + 
             `HC-HO`  + 
             `EC-UN`))


summary(lm(data = df, df$`C19-MO` ~ df$`EL-IN` + 
             `PP-DW`  +   `PS-PD`+ df$`EL-MC` +
             `C19-CC`  + 
             `HC-PW`  + 
             `EC-PO`  + 
             `HC-HO`  + 
             `EC-UN`))

x <- lm(data = df, df$`C19-VC` ~ 
             `PP-DW`  +   `PS-PD`+ df$`EL-MC` +
             `C19-CC`  + 
             `HC-PW`  + 
             `EC-PO`  + 
             `HC-HO`  + 
             `EC-UN` | df$`EL-IN` + 
          `PP-DW`  +   `PS-PD`+
          `C19-CC`  + 
          `HC-PW`  + 
          `EC-PO`  + 
          `HC-HO`  + 
          `EC-UN`)


summary(x, diagnostics = T)














































library(TMB)
dyn.load(dynlib('multiGLM'))
names(getLoadedDLLs())
help(saveRDS, help_type='html')
library(purrr)
help(pwalk, help_type='html')
csilentat(crayon::blue(clisymbols::symbol$star), 'Loading DLL\n')

S <- matrix(c(0.2, 0.15, 0.1, 0.2,
              0.15, 0.3, 0.2, 0.1,
              0.1, 0.2, 0.4, 0.15,
              0.2, 0.1, 0.15, 0.5), 4, 4)
chol(S)

i <- '-1'
class(i)
abs(as.numeric(i))

write.table(rbind(rnorm(5)), file='tmp.txt', append=TRUE,
            col.names=FALSE)

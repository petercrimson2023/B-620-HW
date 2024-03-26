library(magrittr)

# prepare the toy data

hcover =c(0.06,0.07,0.06,0.07,0.07,0.06, 0.02,0.02,0.01)
pcdocs =c(0.02, 0.01,0.02,0.01,0.02,0.01,0.04,0.04,0.05)
A =c(1,1,1,1,0,0,0,0,0)
mrate =c(11,14, 24, 20, 26, 20, 3, 7,8)

data = cbind(A, mrate,hcover, pcdocs ) %>% as.data.frame()

data

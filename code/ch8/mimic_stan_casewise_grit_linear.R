grit1 = read.csv("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/data.csv",header=T,sep="")

# set of models
# factor score for grit, use regression
# factor score for grit, use boosting
# sem model with predictors, latent variable for grit
# sem model with latent variable for predictors, latent variable for grit
# sem trees with grit as latent variable
# sem forests with grit as latent variable

levels(grit1$O10)

grit1$O10[grit1$O10 == "iOS" | grit1$O10 == "Linux"| grit1$O10 == "Macintosh" | grit1$O10 == "Windows"] = NA

grit2 <- grit1[complete.cases(grit1),c(3:14,31:92)]

grit2 = data.matrix(grit2)
grit2[,74] = grit2[,74] - 2

# take sample of 1000 for each
set.seed(1)
ids1 = sample(1:nrow(grit2),2000)
grit.train = grit2[ids1[1:1000],]
grit.test = grit2[ids1[1001:2000],]
grit.pop = grit2[-ids1,]

library(lavaan)

library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# try SEM model

mimic.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
agree =~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10
consc =~ C1+C2+C3+C4+C5+C6+C7+C8+C9+C10
open =~ O1+O2+O3+O4+O5+O6+O7+O8+O9+O10
extra =~ E1+E2+E3+E4+E5+E6+E7+E8+E9+E10
neuro =~ N1+N2+N3+N4+N5+N6+N7+N8+N9+N10
grit1 + grit2 ~ education+urban+gender+engnat+age+hand+religion+orientation+race+voted+married+familysize+
agree+consc+open+extra+neuro
"
mimic.out2 = cfa(mimic.grit2,grit.train)
summary(mimic.out2,fit=T,rsquare=T)

demo = grit.train[,13:24]
X = grit.train[,c(1:12,25:74)]
N <- nrow(X)

dat <- list(
  N = N,
  X = X,
  demo=demo)


mod.stan <-"
data{
int N; // sample size
matrix[N,62] X; // data matrix of order [N,P]
matrix[N,12] demo; // data matrix of order [N,P]
}

parameters{
matrix[N,7] FS; // factor scores, matrix of order [N,D]
vector<lower=0>[62] sigma;
vector[58] lam;
vector[62] alpha;
vector[12] beta1;
vector[12] beta2;
vector[10] beta_lv;
cov_matrix[5] psi_five;
cov_matrix[2] psi_two;
}

transformed parameters{

vector[62] mu[N];
vector[2] mu2[N];


for(i in 1:N){
mu[i,1] = alpha[1] + 1*FS[i,1];
mu[i,2] = alpha[2] + 1*FS[i,2];
mu[i,3] = alpha[3] + lam[1]*FS[i,1];
mu[i,4] = alpha[4] + lam[2]*FS[i,2];
mu[i,5] = alpha[5] + lam[3]*FS[i,1];
mu[i,6] = alpha[6] + lam[4]*FS[i,2];
mu[i,7] = alpha[7] + lam[5]*FS[i,1];
mu[i,8] = alpha[8] + lam[6]*FS[i,2] + lam[11]*FS[i,1];
mu[i,9] = alpha[9] + lam[7]*FS[i,1] + lam[12]*FS[i,2];
mu[i,10] = alpha[10] + lam[8]*FS[i,2];
mu[i,11] = alpha[11] + lam[9]*FS[i,1] + lam[13]*FS[i,2];
mu[i,12] = alpha[12] + lam[10]*FS[i,2];

mu[i,13] = alpha[13] + 1*FS[i,3];
mu[i,14] = alpha[14] + lam[14]*FS[i,3];
mu[i,15] = alpha[15] + lam[15]*FS[i,3];
mu[i,16] = alpha[16] + lam[16]*FS[i,3];
mu[i,17] = alpha[17] + lam[17]*FS[i,3];
mu[i,18] = alpha[18] + lam[18]*FS[i,3];
mu[i,19] = alpha[19] + lam[19]*FS[i,3];
mu[i,20] = alpha[20] + lam[20]*FS[i,3];
mu[i,21] = alpha[21] + lam[21]*FS[i,3];
mu[i,22] = alpha[22] + lam[22]*FS[i,3];

mu[i,23] = alpha[23] + 1*FS[i,4];
mu[i,24] = alpha[24] + lam[23]*FS[i,4];
mu[i,25] = alpha[25] + lam[24]*FS[i,4];
mu[i,26] = alpha[26] + lam[25]*FS[i,4];
mu[i,27] = alpha[27] + lam[26]*FS[i,4];
mu[i,28] = alpha[28] + lam[27]*FS[i,4];
mu[i,29] = alpha[29] + lam[28]*FS[i,4];
mu[i,30] = alpha[30] + lam[29]*FS[i,4];
mu[i,31] = alpha[31] + lam[30]*FS[i,4];
mu[i,32] = alpha[32] + lam[31]*FS[i,4];

mu[i,33] = alpha[33] + 1*FS[i,5];
mu[i,34] = alpha[34] + lam[32]*FS[i,5];
mu[i,35] = alpha[35] + lam[33]*FS[i,5];
mu[i,36] = alpha[36] + lam[34]*FS[i,5];
mu[i,37] = alpha[37] + lam[35]*FS[i,5];
mu[i,38] = alpha[38] + lam[36]*FS[i,5];
mu[i,39] = alpha[39] + lam[37]*FS[i,5];
mu[i,40] = alpha[40] + lam[38]*FS[i,5];
mu[i,41] = alpha[41] + lam[39]*FS[i,5];
mu[i,42] = alpha[42] + lam[40]*FS[i,5];

mu[i,43] = alpha[43] + 1*FS[i,6];
mu[i,44] = alpha[44] + lam[41]*FS[i,6];
mu[i,45] = alpha[45] + lam[42]*FS[i,6];
mu[i,46] = alpha[46] + lam[43]*FS[i,6];
mu[i,47] = alpha[47] + lam[44]*FS[i,6];
mu[i,48] = alpha[48] + lam[45]*FS[i,6];
mu[i,49] = alpha[49] + lam[46]*FS[i,6];
mu[i,50] = alpha[50] + lam[47]*FS[i,6];
mu[i,51] = alpha[51] + lam[48]*FS[i,6];
mu[i,52] = alpha[52] + lam[49]*FS[i,6];

mu[i,53] = alpha[53] + 1*FS[i,7];
mu[i,54] = alpha[54] + lam[50]*FS[i,7];
mu[i,55] = alpha[55] + lam[51]*FS[i,7];
mu[i,56] = alpha[56] + lam[52]*FS[i,7];
mu[i,57] = alpha[57] + lam[53]*FS[i,7];
mu[i,58] = alpha[58] + lam[54]*FS[i,7];
mu[i,59] = alpha[59] + lam[55]*FS[i,7];
mu[i,60] = alpha[60] + lam[56]*FS[i,7];
mu[i,61] = alpha[61] + lam[57]*FS[i,7];
mu[i,62] = alpha[62] + lam[58]*FS[i,7];

mu2[i,1] =  beta1'*demo[i,]' + beta_lv[1]*FS[i,3]+ beta_lv[2]*FS[i,4]+ beta_lv[3]*FS[i,5]+ beta_lv[4]*FS[i,6]+ beta_lv[5]*FS[i,7];
mu2[i,2] =  beta2'*demo[i,]' + beta_lv[6]*FS[i,3]+ beta_lv[7]*FS[i,4]+ beta_lv[8]*FS[i,5]+ beta_lv[9]*FS[i,6]+ beta_lv[10]*FS[i,7];


}
}

model{
sigma ~ gamma(2,2);
alpha ~ normal(0,1);

for(i in 1:N){  
  for(j in 1:62){
    X[i,j] ~ normal(mu[i,j],pow(sigma[j],0.5));
  }
FS[i,1:2] ~ multi_normal(mu2[i,1:2],psi_two);
FS[i,3:7] ~ multi_normal(rep_vector(0,5),psi_five);
}
}
"


system.time(mimic.model <- stan(model_code=mod.stan,iter=500,
              data = dat,chains=4,cores=4,
              pars=c("sigma","lam","alpha","beta1","beta2","beta_lv")))

print(mimic.model)
round(summary(mimic.model)$summary[50:112,],3)
plot(mimic.model,pars="lam[1]",plotfun="trace")

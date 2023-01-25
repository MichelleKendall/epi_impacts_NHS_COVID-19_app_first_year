data { 
  int num_data; 
  int num_basis; 
  real n_ref;
  real s_ref;
  int P[num_data]; 
  int N[num_data]; 
  vector[num_data] X; 
  vector[31] reverse_delaydistr;
  matrix[num_basis, num_data] B; 
} 
 
parameters { 
  row_vector[num_basis] n_rel; 
  real n_abs; 
  real n0; 
  row_vector[num_basis] s_rel; 
  real s_abs; 
  real s0; 
  real <lower=0> invphi_noise;
} 
 
transformed parameters { 
  row_vector[num_basis] n; 
  row_vector[num_basis] s; 
  vector[num_data] N_exp; 
  vector[num_data] P_exp; 
  vector[num_data] S_exp; 
  n = n_rel*n_abs;
  s = s_rel*s_abs;
  N_exp = exp( n0*X + to_vector(n*B) ); 
  S_exp = exp( s0*X + to_vector(s*B) ); 
  for(day in 31:num_data) {
      P_exp[day] = dot_product( segment( N_exp .* S_exp, day-31+1, 31), reverse_delaydistr ) ;
  }
    for(day in 1:30) {
      P_exp[day] = P_exp[31] ; 
  }
} 
 
model { 
  n_abs ~ cauchy(0, fabs(log(5*n_ref)));
  s_abs ~ cauchy(0, fabs(log(5*s_ref)));
  n_rel ~ normal(0, 1);
  s_rel ~ normal(0, 1);
  invphi_noise ~ cauchy(0, 1) T[0,];
  for(day in 31:num_data) {
    P[day] ~ neg_binomial_2( P_exp[day] , 1/invphi_noise );
    N[day] ~ neg_binomial_2( N_exp[day] , 1/invphi_noise );
  }
}


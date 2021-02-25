
# parameters: gamma, beta, delta, phi, con
# variable: price (for each brand and week)
# init conditions for inv_0 and q_0


price # T x B matrix

data_h = NULL
for (h in 1:H) {
  inv = inv_0
  q = q_0
  data_t = NULL
  for (t in 1:T) {
    inv = inv_f(inv, q, con)
    i = i_f(gamma, inv, con) # binary: 1 = purchase incidence in t, 0 otherwise
    if (i == 1) {
      b = b_f(beta, delta, price[t,]) # munitnomial
      q = q_f(phi) # pos integer
    } else {
      b = 0
      q = 0
    }
    data_t[[t]] = data.table(h = h, t = t, inv = inv, i = i, b = b, q = q)
  }
  data_h[[h]] = rbindlist(data_t)
  
}
data = rbindlist(data_h)



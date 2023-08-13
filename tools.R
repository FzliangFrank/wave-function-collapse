#The trouble of this function is if I ever need a customisable way of update
# Variable it is going to be difficult
reso = 3
rule_matrix = matrix(sample(LETTERS[1:3], reso * reso, replace=T), reso,reso)
rule_matrix

identity = unique(c(rule_matrix)) |> sort()
rule_mtx = match(rule_matrix, identity)
r_dim = dim(rule_matrix)
dim(rule_mtx) = r_dim


rule_lookup_tbl = rule_mtx |> 
  c() |> 
  near_by(rule_mtx) |> 
  apply(1, \(x) ifelse(x==0, 0, rule_mtx[x])) |> 
  t() |> 
  cbind(value=c(rule_mtx)) |> 
  data.frame() |> 
  tidyr::pivot_longer(
    cols= c(everything(), -value),
    values_to = 'nei_value'
  ) |> 
  filter(nei_value != 0)
rule_lookup_tbl 


mtx_empty = matrix(rep(0, 100), 10,10)
mtx = mtx_empty

mtx[sample(seq(length(mtx)), 4)] = sample(seq(length(identity)), 4, replace = T)
cell_seed = which(mtx!=0)
cell_new = cell_seed |> 
  near_by(dim(mtx)) |> 
  c() |> unique()
surounding = cell_new |> # surroundings
  near_by(dim(mtx)) |>
  apply(1, \(x) {
    ifelse(x==0,x,mtx[x])
  }) |> 
  t() |> 
  cbind(idx = c(cell_new)) |> 
  data.frame() |> 
  tidyr::pivot_longer(
    c(everything(),-idx),
    values_to = 'nei_value'
  )
surounding |>
  filter(
    idx !=0
  ) |> 
  left_join(
    rule_lookup_tbl,
    by= c(
      "name" = "name",
      "nei_value"="nei_value"
      )
    ) |> 
  group_by(idx) |> 
  slice_sample(n = 1)
  

start_time <- Sys.time()

source("package_dependencies.R")

source("demo_tidying.R")

source("demo_CLarbor_with_uncontrolled.R")

source("demo_CLarbor_with_controlled.R")

source("demo_CLarbor_graph.R")

source("demo_make_dist_matrix.R")

source("demo_viz_dist.R")

source("demo_viz_neighbournets.R")

source("demo_CLarbor_dist_matrix_differences.R")

end_time <- Sys.time()

time_lapsed <- end_time - start_time

cat("It took", round(time_lapsed, digits = 2), "minutes to run this.")
source("~/Documents/EBS181/complete_final_proj.R") # source code to plot final graphs

#####
# RUN AND PLOT

#Parameters
tau <- 0.044
duration_I <- 3.1*24
n_prisoners <- 500
n_guards <- 175
n_agents <- n_prisoners + n_guards
room_capacity <- 20
n_rooms <- n_prisoners/room_capacity
timesteps = 30*24 # 1 month, converted to hours
guard_import_rate = (0.004/(24))
infect_guard_init = 1

# Run 10 times for each scenario, and summarize results
result_stat_quo <- run_multiple_simulations(n_sims = 10, strategic_clustering = FALSE, guard_prisoner_contact = 0.5)
summary_status_quo <- summarize_results(result_stat_quo)

result_reduced_contacts <- run_multiple_simulations(n_sims = 10, strategic_clustering = FALSE, guard_prisoner_contact = 0.1)
summary_reduced_contacts <- summarize_results(result_reduced_contacts)

result_strategic <- run_multiple_simulations(n_sims = 10, strategic_clustering = TRUE, guard_prisoner_contact = 0.1)
summary_strategic <- summarize_results(result_strategic)
View(summary_strategic)
# Generate plots
multi_run_plots <- plot_multiple_runs()
multi_run_plots$sq_plot_multi
multi_run_plots$red_plot_multi
multi_run_plots$clust_plot_multi


# Create table of results
table_summary <- create_table(summary_status_quo, summary_reduced_contacts, summary_strategic)

kableExtra::kable(table_summary, format = "html", caption = "Comparison of Intervention Scenarios (Averaged Over 10 Simulation Runs)") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kableExtra::add_header_above(c(" " = 1, "Status Quo" = 2, "Reduced Contact" = 2, "Strategic Clustering" = 2))


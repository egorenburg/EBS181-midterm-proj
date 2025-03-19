# Install socmod library from github.
if (!("socmod" %in% installed.packages())) {
  devtools::install_github("CSS4S/socmod")
}

library(socmod)
library(ggplot2)
library(igraph)
library(kableExtra)

##### 
# FUNCTIONS TO CREATE MODEL

# Initialize SEIR agent: add role, room, immune, and infectious_time attributes
SEIRAgent <- R6::R6Class(classname = "SEIRAgent",
  inherit = socmod::Agent,  # inherit from the base Agent class
  public = list(
    role = NULL,
    room = NULL,
    immune = FALSE,
    infectious_time = 0,

    initialize = function(behavior, name = NA, role = NULL, room = NULL) {
      super$initialize(behavior = behavior, name = name)  
      self$role = role
      self$room = room
      self$curr_behavior = behavior  
      self$next_behavior = behavior  
      self$immune = FALSE
      self$infectious_time = 0
      }
    )
  )

#ABM builder
make_example_abm <- function(strategic_clustering = FALSE, ...) {
  
  agent_names <- paste0("agent_", 1:n_agents)
  
  # Assign roles
  agent_roles <- c(rep("guard", n_guards), rep("prisoner", n_agents - n_guards))
  guard_indices <- which(agent_roles == "guard")
  prisoner_indices <- which(agent_roles == "prisoner")
  
  # Assign S or I status, with one guard initially infected
  agent_behaviors <- rep("Legacy", n_agents) # Start with all Legacy (susceptible)
  num_infected_guards <- 1
  if (num_infected_guards > 0) {   # Randomly select initial infected guard
    infected_guard_indices <- sample(guard_indices, num_infected_guards)
    agent_behaviors[infected_guard_indices] <- "Adaptive"
  }

  # Assign rooms (guards don't have rooms)
  n_prisoners <- n_agents - n_guards
  
  prisoners_per_room <- ceiling(n_prisoners / n_rooms)
  prisoner_rooms <- rep(1:n_rooms, each = prisoners_per_room)[1:n_prisoners]
  prisoner_rooms <- sample(prisoner_rooms)
  agent_rooms <- c(rep(NA, n_guards), prisoner_rooms)
  
  make_example_agents <- function() {
    agents <- purrr::map(
      1:n_agents, function(a_idx) { 
        SEIRAgent$new(behavior = agent_behaviors[a_idx], 
                      name = agent_names[a_idx],
                      role = agent_roles[a_idx],
                      room = agent_rooms[a_idx]) 
      }
    )
    return (agents)
  }
  
  agents <- make_example_agents()
  
  # Print initial state distribution
  initial_states <- sapply(agents, function(a) a$curr_behavior)
  initial_guard_states <- sapply(agents[guard_indices], function(a) a$curr_behavior)
  print("Initial state distribution:")
  print(table(initial_states))
  print("Initial guard state distribution:")
  print(table(initial_guard_states))
  
  # Initialize network
  socnet <- igraph::make_empty_graph(n_agents, directed = FALSE)
  
  igraph::V(socnet)$name <- agent_names
  igraph::V(socnet)$role <- agent_roles
  igraph::V(socnet)$room <- agent_rooms
  

  if (strategic_clustering) {
    # Strategic guard clustering implementation for guard connections
    # Assign guards to rooms
    guards_per_room <- ceiling(n_guards / n_rooms) # 125/25 = 7 guards per room
    guard_room_assignments <- rep(1:n_rooms, each = guards_per_room)[1:n_guards]
    
    # Connect guards to other guards in the same room
    for (room in 1:n_rooms) {
      room_guards <- guard_indices[guard_room_assignments == room]
      if (length(room_guards) > 1) {
        guard_pairs <- combn(room_guards, 2)
        guard_edges <- as.vector(guard_pairs)
        socnet <- igraph::add_edges(socnet, edges = guard_edges)
      }
      
      # Connect guards to prisoners in their assigned room
      room_prisoners <- which(agent_rooms == room)
      for (guard in room_guards) {
        edges <- cbind(rep(guard, length(room_prisoners)), room_prisoners)
        socnet <- igraph::add_edges(socnet, edges = as.vector(edges))
      }
    }
  }
  else {
    # Status quo guard connections
    # Initialize connections between all guards
    guard_indices <- which(agent_roles == "guard")
    guard_pairs <- combn(guard_indices, 2)
    guard_edges <- as.vector(guard_pairs)
    socnet <- igraph::add_edges(socnet, edges = guard_edges)
    
    # Add connections from guards to some % of prisoners
    prisoner_indices <- which(agent_roles == "prisoner")
    for (guard in guard_indices) {
      to_pick = round(length(prisoner_indices) * (guard_prisoner_contact)) 
      to_add = sample(prisoner_indices, to_pick)
      edges = cbind(rep(guard, to_pick), to_add)
      socnet <- igraph::add_edges(socnet, edges = edges)
    }
  }
  # Roommate initialization is the same in all scenarios
  # Add connections between roommates
  for (room in unique(prisoner_rooms)) {
    roommates <- which(agent_rooms == room) # indices of all roommates
    if (length(roommates) > 1) {
      roommate_pairs <- combn(roommates, 2)
      socnet <- igraph::add_edges(socnet, edges = as.vector(roommate_pairs))
    }
  }
  
  # Add edges from prisoners randomly to 1/10 of other prisoners
  for (prisoner in prisoner_indices) {
    to_pick = round(length(prisoner_indices) * (1/10)) 
    to_add = sample(prisoner_indices, to_pick)
    edges = cbind(rep(prisoner, to_pick), to_add)
    socnet <- igraph::add_edges(socnet, edges = edges)
  }
  
  # Make and return ABM
  abm <- socmod::AgentBasedModel$new(agents = agents, network = socnet, ...)
  return (abm)
}

# Partner selection, with weights based on agent attributes
weighted_partner_selection <- function(a, model, strategic_clustering = FALSE) {
  neighbors <- a$neighbors$agents
  
  # Handle case where agent has no neighbors
  if (length(neighbors) == 0) {
    return(NULL)
  }
  
  # Set weights for different types of interactions
  guard_guard_w = 3
  guard_prisoner_w = 1
  prisoner_prisoner_w = 1
  roommates_w = 10

  if (strategic_clustering) {
    guard_guard_w = 7
    guard_prisoner_w = 7
    }
    
  weights <- rep(1, length(neighbors))
  
  for (i in seq_along(neighbors)) {
    neighbor <- neighbors[[i]]
    
    # Assign weights for guard interacting with guard or prisoner 
    if (a$role == "guard") {
      if (neighbor$role == "guard") {
        weights[i] <- guard_guard_w
      } else {
        weights[i] <- guard_prisoner_w
      }
      # Assign weights for prisoner interacting with guard, roommate, or other prisoner
    } else {
      if (neighbor$role == "guard") {
        weights[i] <- guard_prisoner_w
      } else if (a$room == neighbor$room) {
        weights[i] <- roommates_w
      } else {
        weights[i] <- prisoner_prisoner_w
      }
    }
  }
  
  # Select partner based on normalized weights
  weights <- weights / sum(weights)  
  selected_partner <- neighbors[[sample(1:length(neighbors), 1, prob = weights)]]
  return(selected_partner)
}

# Transmission, where transmission can occur if agent is susceptible, partner is infected, and agent is not immune, and it does occur based on the probability oft transmission
seir_transmission <- function(agent, partner, model) {
  
  if (agent$curr_behavior == "Legacy" && partner$curr_behavior == "Adaptive" && agent$immune == FALSE) {
    if (runif(1) < tau) {
      agent$next_behavior <- "Adaptive" # transmission occurred
      # Debug output
      cat("Transmission occurred! Counter before:", total_infected_counter, "\n")
      # Make sure the variable exists in the global environment
      if (!exists("total_infected_counter", envir = .GlobalEnv)) {
        assign("total_infected_counter", 0, envir = .GlobalEnv)
        cat("Created missing counter variable\n")
      }
      
      total_infected_counter <<- total_infected_counter + 1 # increment total infections
      # Debug output
      cat("Counter after:", get("total_infected_counter", envir = .GlobalEnv), "\n")
      
    } 
  } 
}

# Iterate model. Infected agents increment their counter of time in I state, and if they reach 3.1 days, they recover and become immune. Guards have a probability of importing an infection.
seir_model_step <- function(model) {
  for (agent in model$agents) {
      if (agent$curr_behavior == "Adaptive") {
        agent$infectious_time <- agent$infectious_time + 1 # increment time in I

        # Agent becomes immune if passes duration of infection
        if (agent$infectious_time > duration_I) {
          agent$next_behavior <- "Legacy"
          agent$infectious_time = 0
          agent$immune = TRUE
          }
      }
    # For guards, there is probability of import rate of infection
    if (agent$role == "guard") {
      if (runif(1) < guard_import_rate) {
        agent$next_behavior <- "Adaptive"
      }
    }
  }

  # Basic learning step, next behaviors and payoffs become current.
  iterate_learning_model(model)
}

######
# FUNCTIONS TO RUN AND PLOT MODEL

# Run a single simulation with parameters to differentiate scenarios
run_single_simulation <- function(strategic_clustering, guard_prisoner_contact) {
  # Reset global counter for this simulation
  cat("Initializing counter to 0\n")
  assign("total_infected_counter", 0, envir = .GlobalEnv)
  
  # Create ABM with specified parameters
  abm <- make_example_abm(strategic_clustering = strategic_clustering)
  
  # Run the model
  result <- run(abm, timesteps,
                function(a, model) weighted_partner_selection(a, model, strategic_clustering),
                seir_transmission, seir_model_step)
  # Get the updated global counter value
  infection_count <- get("total_infected_counter", envir = .GlobalEnv)
  cat("Final infection count:", infection_count, "\n")
  
  # Return key metrics
  return(list(
    total_infected = infection_count,
    max_infected = max(result$output$A),
    max_i_time = which.max(result$output$A),
    timeseries = result$output$A
  ))
}

# Run multiple simulations for a scenario
run_multiple_simulations <- function(n_sims, strategic_clustering, guard_prisoner_contact) {
  
  results <- list(
    total_infected = numeric(n_sims),
    max_infected = numeric(n_sims),
    max_i_time = numeric(n_sims),
    timeseries = matrix(0, nrow = n_sims, ncol = timesteps + 1)
  )
  
  # Run each simulation
  for (i in 1:n_sims) {
    cat("Running simulation", i, "of", n_sims, "\n")
    assign("guard_prisoner_contact", guard_prisoner_contact, envir = .GlobalEnv)
    sim_result <- run_single_simulation(strategic_clustering, guard_prisoner_contact)
    
    # Store results
    results$total_infected[i] <- sim_result$total_infected
    results$max_infected[i] <- sim_result$max_infected
    results$max_i_time[i] <- sim_result$max_i_time
    results$timeseries[i, 1:length(sim_result$timeseries)] <- sim_result$timeseries
  }
  
  return(results)
}


# Summarize results over multiple runs
summarize_results <- function(results) {
  summary <- list(
    total_infected_mean = mean(results$total_infected),
    total_infected_sd = sd(results$total_infected),
    total_infected_range = range(results$total_infected),
    max_infected_mean = mean(results$max_infected),
    max_infected_sd = sd(results$max_infected),
    max_infected_range = range(results$max_infected),
    max_i_time_mean = mean(results$max_i_time),
    max_i_time_sd = sd(results$max_i_time)
  )
  
  # Get average timeseries and confidence intervals
  ts_mean <- apply(results$timeseries, 2, mean)
  ts_ci_lower <- apply(results$timeseries, 2, function(x) quantile(x, 0.025))
  ts_ci_upper <- apply(results$timeseries, 2, function(x) quantile(x, 0.975))
  
  summary$timeseries_mean <- ts_mean
  summary$timeseries_ci_lower <- ts_ci_lower
  summary$timeseries_ci_upper <- ts_ci_upper
  
  return(summary)
}


# Plot results from multiple runs with confidence intervals
plot_multiple_runs <- function() {
  times <- 0:timesteps
  
  # Plot for Status Quo
  sq_df <- data.frame(
    time = times,
    mean = summary_status_quo$timeseries_mean,
    lower = summary_status_quo$timeseries_ci_lower,
    upper = summary_status_quo$timeseries_ci_upper
  )
  
  sq_plot_multi <- ggplot(sq_df, aes(x = time/24, y = mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(
      title="COVID-19 Spread in Prison Population",
      subtitle=paste("Status Quo Scenario: guards contact 50% of prisoners (10 runs)"),
      x="Time (days)",
      y="Number of Infected Individuals",
      caption=paste("Model parameters:", n_prisoners, "prisoners,", n_guards, "guards,", n_rooms, "rooms | Transmission rate: 0.044 | Recovery time: 3.1 days")
    ) +
    scale_x_continuous(
      breaks=seq(0, timesteps/24, by=2)  # tick marks every 2 days
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face="bold", size=14),
      plot.subtitle = element_text(size=12),
      axis.title = element_text(face="bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(color="lightgrey", fill=NA, linewidth=0.5),
      plot.caption = element_text(hjust=0, size=8)
    )
    
  # Plot for Reduced Contacts
  red_df <- data.frame(
    time = times,
    mean = summary_reduced_contacts$timeseries_mean,
    lower = summary_reduced_contacts$timeseries_ci_lower,
    upper = summary_reduced_contacts$timeseries_ci_upper
  )
  
  red_plot_multi <- ggplot(red_df, aes(x = time/24, y = mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(
      title="COVID-19 Spread in Prison Population",
      subtitle=paste("Reduced Guard Contact Scenario: guards contact 10% of prisoners"),
      x="Time (days)",
      y="Number of Infected Individuals",
      caption=paste("Model parameters:", n_prisoners, "prisoners,", n_guards, "guards,", n_rooms, "rooms | Transmission rate: 0.044 | Recovery time: 3.1 days")
    ) +
    scale_x_continuous(
      breaks=seq(0, timesteps/24, by=2)  # tick marks every 2 days
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face="bold", size=14),
      plot.subtitle = element_text(size=12),
      axis.title = element_text(face="bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(color="lightgrey", fill=NA, linewidth=0.5),
      plot.caption = element_text(hjust=0, size=8)
    )
    
  # Plot for Strategic Clustering
  clust_df <- data.frame(
    time = times,
    mean = summary_strategic$timeseries_mean,
    lower = summary_strategic$timeseries_ci_lower,
    upper = summary_strategic$timeseries_ci_upper
  )

  clust_plot_multi <- ggplot(clust_df, aes(x = time/24, y = mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(
      title="COVID-19 Spread in Prison Population",
      subtitle=paste("Strategic Guard Clustering Scenario (10 runs)"),
      x="Time (days)",
      y="Number of Infected Individuals",
      caption=paste("Model parameters:", n_prisoners, "prisoners,", n_guards, "guards,", n_rooms, "rooms | Transmission rate: 0.044 | Recovery time: 3.1 days")
    ) +
    scale_x_continuous(
      breaks=seq(0, timesteps/24, by=2)  # tick marks every 2 days
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face="bold", size=14),
      plot.subtitle = element_text(size=12),
      axis.title = element_text(face="bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(color="lightgrey", fill=NA, linewidth=0.5),
      plot.caption = element_text(hjust=0, size=8)
    )
  return(list(sq_plot_multi = sq_plot_multi, red_plot_multi = red_plot_multi, clust_plot_multi = clust_plot_multi)) 
} 


# Create table to display results 
create_table <- function(summary_status_quo, summary_reduced_contacts, summary_strategic) {
  # Create a data frame for the table
  table_data <- data.frame(
    Metric = c("Total Infections", "Maximum Concurrent Infections", "Peak Day"),
    
    # Status Quo
    SQ_Mean = c(
      round(summary_status_quo$total_infected_mean, 1),
      round(summary_status_quo$max_infected_mean, 1),
      round(summary_status_quo$max_i_time_mean/24, 1)
    ),
    SQ_SD = c(
      round(summary_status_quo$total_infected_sd, 1),
      round(summary_status_quo$max_infected_sd, 1),
      round(summary_status_quo$max_i_time_sd/24, 1)
    ),
    
    # Reduced Contacts
    RC_Mean = c(
      round(summary_reduced_contacts$total_infected_mean, 1),
      round(summary_reduced_contacts$max_infected_mean, 1),
      round(summary_reduced_contacts$max_i_time_mean/24, 1)
    ),
    RC_SD = c(
      round(summary_reduced_contacts$total_infected_sd, 1),
      round(summary_reduced_contacts$max_infected_sd, 1),
      round(summary_reduced_contacts$max_i_time_sd/24, 1)
    ),
    
    # Strategic Clustering
    SC_Mean = c(
      round(summary_strategic$total_infected_mean, 1),
      round(summary_strategic$max_infected_mean, 1),
      round(summary_strategic$max_i_time_mean/24, 1)
    ),
    SC_SD = c(
      round(summary_strategic$total_infected_sd, 1),
      round(summary_strategic$max_infected_sd, 1),
      round(summary_strategic$max_i_time_sd/24, 1)
    )
  )
  
  # Rename columns for clarity
  colnames(table_data) <- c(
    "Metric", 
    "Mean", "SD", 
    "Mean", "SD", 
    "Mean", "SD"
  )
  
  return(table_data)
}

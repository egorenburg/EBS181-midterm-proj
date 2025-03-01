# Install socmod library from github.
if (!("socmod" %in% installed.packages())) {
  devtools::install_github("CSS4S/socmod")
}

library(socmod)
library(ggplot2)
library(igraph)

# FUNCTIONS

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
  
  ####
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
      total_infected_counter <<- total_infected_counter + 1 # increment total infections
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


#PARAMETERS
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

# RUN MODEL

# status quo scenario
total_infected_counter <- 0
guard_prisoner_contact = 0.5
abm_status_quo <- make_example_abm(strategic_clustering = FALSE)

result_status_quo <- run(abm_status_quo, timesteps,
                         function(a, model) weighted_partner_selection(a, model, FALSE),
                         seir_transmission, seir_model_step)

sq_total_infected = total_infected_counter
sq_max_infected = max(result_status_quo$output$A)
sq_max_i_time = which(result_status_quo$output$A == sq_max_infected)


# Reduced contacts scenario
total_infected_counter <- 0
guard_prisoner_contact = 0.1
abm_status_quo <- make_example_abm(strategic_clustering = FALSE)

result_reduced_contacts <- run(abm_status_quo, timesteps,
                               function(a, model) weighted_partner_selection(a, model, FALSE),
                               seir_transmission, seir_model_step)

red_total_infected = total_infected_counter
red_max_infected = max(result_reduced_contacts$output$A)
red_max_i_time = which(result_reduced_contacts$output$A == red_max_infected)


# Strategic clustering scenario  
total_infected_counter <- 0
abm_strategic <- make_example_abm(strategic_clustering = TRUE)

result_strategic <- run(abm_strategic, timesteps, 
                        function(a, model) weighted_partner_selection(a, model, TRUE), 
                        seir_transmission, seir_model_step)

clust_total_infected <- total_infected_counter
clust_max_infected = max(result_strategic$output$A)
clust_max_i_time = which(result_strategic$output$A == clust_max_infected)

# PLOT
source("~/Documents/EBS181/final_proj_plotting.R") # source code to plot final graphs
sq_plot
reduced_plot
strategic_plot

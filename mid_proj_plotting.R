#PLOT GRAPHS

#status quo plot
sq_plot <- ggplot(result_status_quo$output, aes(x=t/24, y=A)) + # convert hours to days for x axis
  geom_line(color="#E41A1C", linewidth=1) +  
  geom_area(fill="#E41A1C", alpha=0.2) +  
  labs(
    title="COVID-19 Spread in Prison Population",
    subtitle=paste("Status Quo Scenario: guards contact 50% of prisoners"),
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
  ) +
  # Text annotation for total infections counter 
  annotate("label", x = max(result_status_quo$output$t/24) * 0.8, y = max(result_status_quo$output$A) * 0.95, 
           label = paste("Total Infections:", sq_total_infected),
           size = 4.5, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines")) +
  # Text annotation for maximum infections
  annotate("label", x = max(result_status_quo$output$t/24) * 0.8, y = max(result_status_quo$output$A) * 0.85, 
           label = paste("Maximum Infected:", sq_max_infected),
           size = 4.5, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines")) +
  # Text annotation for time of maximum infections
  annotate("label", x = max(result_status_quo$output$t/24) * 0.8, y = max(result_status_quo$output$A) * 0.75, 
           label = paste("Peak Day:", round(sq_max_i_time/24, 1)),
           size = 4.5, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines"))

sq_plot

# Reduced connections plot
reduced_plot <- ggplot(result_reduced_contacts$output, aes(x=t/24, y=A)) + # convert hours to days for x axis
  geom_line(color="#E41A1C", linewidth=1) +  
  geom_area(fill="#E41A1C", alpha=0.2) +  
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
  ) +
  # Text annotation for total infections counter
  annotate("label", x = max(result_reduced_contacts$output$t/24) *0.8, y = max(result_reduced_contacts$output$A) * 0.95, 
           label = paste("Total Infections:", red_total_infected),
           size = 4.5, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines")) +
  # Text annotation for maximum infections
  annotate("label", x = max(result_reduced_contacts$output$t/24) * 0.8, y = max(result_reduced_contacts$output$A) * 0.85, 
           label = paste("Maximum Infected:", red_max_infected),
           size = 4.5, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines")) +
  # Text annotation for time of maximum infections
  annotate("label", x = max(result_reduced_contacts$output$t/24) * 0.8, y = max(result_reduced_contacts$output$A) * 0.75, 
           label = paste("Peak Day:", round(red_max_i_time/24, 1)),
           size = 4.5, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines"))

reduced_plot

##### Strategic plot
strategic_plot <- ggplot(result_strategic$output, aes(x=t/24, y=A)) + # convert hours to days for x axis
  geom_line(color="#E41A1C", linewidth=1) +  
  geom_area(fill="#E41A1C", alpha=0.2) +  
  labs(
    title="COVID-19 Spread in Prison Population",
    subtitle=paste("Strategic Guard Clustering Scenario"),
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
  ) + 
  # Text annotation for total infections counter 
  annotate("label", x = max(result_strategic$output$t/24) * 0.58, y = max(result_strategic$output$A) * 1, 
           label = paste("Total Infections:", total_infected_counter),
           size = 4, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines")) +
  # Text annotation for maximum infections
  annotate("label", x = max(result_strategic$output$t/24) * 0.58, y = max(result_strategic$output$A) * 0.9, 
           label = paste("Maximum Infected:", clust_max_infected),
           size = 4, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines")) +
  # Text annotation for time of maximum infections
  annotate("label", x = max(result_strategic$output$t/24) * 0.58, y = max(result_strategic$output$A) * 0.8, 
           label = paste("Peak Day:", round(clust_max_i_time/24, 1)),
           size = 4, fontface = "bold", fill = "white", color = "#E41A1C",
           label.padding = unit(0.5, "lines"), label.r = unit(0.3, "lines"))

strategic_plot
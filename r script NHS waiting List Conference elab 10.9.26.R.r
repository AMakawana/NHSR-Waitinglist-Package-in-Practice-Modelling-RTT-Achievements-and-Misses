
## load packages (install if required)
library(NHSRwaitinglist)
library(ggplot2)
library(dplyr)



## define variables
demand <- 270
capacity <- 200
scen2_cap <- 250
waiting_time_target <- 18
factor <- qexp(0.92)



## current waiting list
start_date_current_WL <- as.Date("2026-01-05")

weeks <- seq(start_date_current_WL, by = "week", length.out = 26)

patients <- c(
  1259,1315,1400,1472,1604,1639,1714,1826,1994,2069,2135,2243,2470,2531,2582,2630,2671,2708,2819,2839,2906,2918,2949,2972,3071,3098

)

original_wl <- data.frame(
  dates = weeks,
  queue_size = patients,
  type = "Historic WL"
)


ggplot(original_wl, aes(x = dates, y = queue_size)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Example Waiting List Time Series",
    x = "Week",
    y = "Waiting List Size"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



## Demonstrated Capacity
# demand - ((wl size previously - wl size now)/weeks between the 2 dates)
wl_size_previously <- 1259
wl_size_now <- 3098
weeks_between <- 25

demand - ((wl_size_previously - wl_size_now)/weeks_between)



## Target Mean Wait
target_mean_wait <- calc_target_mean_wait(18, factor = factor)
target_mean_wait



## Target Queue Size
target_queue_size <- calc_target_queue_size(demand, waiting_time_target, factor)
target_queue_size

TQS <- (demand * 18)/factor
TQS


## Add TQS to chart
ggplot(original_wl, aes(x = dates, y = queue_size)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +

  geom_hline(
    yintercept = TQS,
    linetype = "dashed",
    linewidth = 1
  ) +

  annotate(
    "text",
    x = min(original_wl$dates),
    y = TQS,
    label = "Target Waiting List Size for 92% seen by 18 weeks",
    hjust = 0,
    vjust = -0.5
  ) +

  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Example Waiting List Time Series",
    x = "Week",
    y = "Waiting List Size"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



## Scenario 1: Project what will likely happen if demand and capacity stays as they are
referral_date <- as.Date("2026-07-06") # a week after our last known waiting list value

current_wl <- ## builds a "waiting list" for us with the specified number of open pathways - check out the vignettes on the github page for more ways to create the starting list
  data.frame(
    Referral = rep(referral_date, 3098), # last known waiting list value
    Removal = rep(as.Date(NA), 3098)
  )

end_march_27 <- as.Date("2027-03-31") # when do we want the simulation to run until

sim_DND <- wl_queue_size(
  wl_simulator(
    start_date = referral_date,
    end_date = end_march_27,
    demand = demand,
    capacity = capacity,
    waiting_list = current_wl
  )
)

# wl simulator:
# calcualtes the total demand over the timeperiod and create a poisson distribution where the mean = this total demand. Then draw 1 realised
# demand number from th epoisson distribution (this gives is the randomness and variablity, so each time we run the simulation we will get
# slightly different outputs)
# then make a vector of daily dates at random across the timeperiod and whatever the relaised demand number is, randomly assign that number
# of patients a referral date within the random dates across timeperiod
# see the github page R/ for more info https://github.com/nhs-r-community/NHSRwaitinglist/blob/main/R/create_waiting_list.R

# we wrap this in wl_queue_size which takes the patient-level waiting list and turns it into a daily count of how many patients are still
# waiting. It creates every date between the start and end date then for each day it counts how many patients were referred/added to the
# waiting list and keeps a running total of all referrals up to that day.Then each day it counts how many patients were removed from the
# waiting list and keeps a running total of all removals. Then calculates Queue size = cumulative referrals − cumulative removals


sim_DND$type <- "Capacity: DND"


## plot DND scenario
df_plot <- bind_rows(
  original_wl,
  sim_DND
)

linetype_map <- c(
  "Historic WL" = "solid",
  "Capacity: DND" = "dashed"
)

linewidth_map <- c(
  "Historic WL" = 1.2,
  "Capacity: DND" = 1.6
)

ggplot(
  df_plot,
  aes(
    x = dates,
    y = queue_size,
    colour = type,
    group = type
  )
) +

  geom_line(
    aes(
      linetype = type,
      linewidth = type
    )
  ) +

  scale_linewidth_manual(values = linewidth_map) +
  scale_linetype_manual(values = linetype_map) +

  scale_colour_manual(values = c(
    "Historic WL" = "#000000",
    "Capacity: DND" = "#6A6A68"
  )) +

  # Target queue size
  geom_hline(
    yintercept = TQS,
    linetype = "dashed",
    colour = "black"
  ) +

  annotate(
    "text",
    x = max(df_plot$dates) - lubridate::days(14),
    y = TQS + 130,
    label = stringr::str_wrap(
      "Target Waiting List Size for 92% seen by 18 weeks",
      width = 30
    ),
    hjust = 1,
    colour = "black",
    size = 3
  ) +

  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.01, 0.03))
  ) +

  labs(
    title = "Example Simulated Waiting List Trajectory",
    subtitle = paste0(
      "Weekly Referrals = ", demand,
      "   Baseline Capacity = ", capacity
    ),
    x = "Date",
    y = "Queue Size"
  ) +

  theme_minimal() +

  guides(
    colour = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +

  theme(
    legend.title = element_blank()
  )





## Scenario 2: Project what will likely happen with a predetermined capacity increase
referral_date <- as.Date("2026-07-06")

current_wl <-
  data.frame(
    Referral = rep(referral_date, 3098),
    Removal = rep(as.Date(NA), 3098)
  )

end_march_27 <- as.Date("2027-03-31")

sim_increase_cap_250 <- wl_queue_size(
  wl_simulator(
    start_date = referral_date,
    end_date = end_march_27,
    demand = demand,
    capacity = scen2_cap , ## everything remains the same as DND except the capacity
    waiting_list = current_wl
  )
)

sim_increase_cap_250$type <- "Capacity: Increase by 50"



## plot scenario 2
df_plot <- bind_rows(
  original_wl,
  sim_DND,
  sim_increase_cap_250
)

linetype_map <- c(
  "Historic WL" = "solid",
  "Capacity: DND" = "dashed",
  "Capacity: Increase by 50" = "dashed"
)

linewidth_map <- c(
  "Historic WL" = 1.2,
  "Capacity: DND" = 1.6,
  "Capacity: Increase by 50" = 1.6
)


ggplot(
  df_plot,
  aes(
    x = dates,
    y = queue_size,
    colour = type,
    group = type
  )
) +

  geom_line(
    aes(
      linetype = type,
      linewidth = type
    )
  ) +

  scale_linewidth_manual(
    values = linewidth_map
  ) +

  scale_linetype_manual(
    values = linetype_map
  ) +

  scale_colour_manual(
    values = c(
      "Historic WL" = "#000000",
      "Capacity: DND" = "#6A6A68",
      "Capacity: Increase by 50" = "#377EB8"
    )
  ) +

  # Target queue size
  geom_hline(
    yintercept = TQS,
    linetype = "dashed",
    colour = "black"
  ) +

  annotate(
    "text",
    x = max(df_plot$dates) - lubridate::days(14),
    y = TQS + 130,
    label = stringr::str_wrap(
      "Target Waiting List Size for 92% seen by 18 weeks",
      width = 30
    ),
    hjust = 1,
    colour = "black",
    size = 3
  ) +

  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.01, 0.12))
  ) +


  labs(
    title = "Example Simulated Waiting List Trajectory",
    subtitle = paste0(
      "Weekly Referrals = ", demand,
      "   Baseline Capacity = ", capacity
    ),
    x = "Date",
    y = "Queue Size"
  ) +

  theme_minimal() +

  guides(
    colour = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +

  theme(
    legend.title = element_blank()
  )



## add end of line numbers to indicate additional capacity
# End-of-line labels
end_labels <- df_plot %>%
  group_by(type) %>%
  slice_max(dates, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    label_value = case_when(
      type == "Capacity: DND" ~ capacity - capacity,
      type == "Capacity: Increase by 50" ~ scen2_cap - capacity,
      TRUE ~ NA_real_
    )
  )

ggplot(
  df_plot,
  aes(
    x = dates,
    y = queue_size,
    colour = type,
    group = type
  )
) +

  geom_line(
    aes(
      linetype = type,
      linewidth = type
    )
  ) +

  # End-of-line additional capacity labels
  geom_text(
    data = end_labels %>% filter(!is.na(label_value)),
    aes(
      x = dates,
      y = queue_size,
      label = label_value,
      colour = type
    ),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +

  scale_linewidth_manual(
    values = linewidth_map
  ) +

  scale_linetype_manual(
    values = linetype_map
  ) +

  scale_colour_manual(
    values = c(
      "Historic WL" = "#000000",
      "Capacity: DND" = "#6A6A68",
      "Capacity: Increase by 50" = "#377EB8"
    )
  ) +

  # Target queue size
  geom_hline(
    yintercept = TQS,
    linetype = "dashed",
    colour = "black"
  ) +

  annotate(
    "text",
    x = max(df_plot$dates) - lubridate::days(14),
    y = TQS + 130,
    label = stringr::str_wrap(
      "Target Waiting List Size for 92% seen by 18 weeks",
      width = 30
    ),
    hjust = 1,
    colour = "black",
    size = 3
  ) +

  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +

  labs(
    title = "Example Simulated Waiting List Trajectory",
    subtitle = paste0(
      "Weekly Referrals = ", demand,
      "   Baseline Capacity = ", capacity
    ),
    x = "Date",
    y = "Queue Size"
  ) +

  theme_minimal() +

  guides(
    colour = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +

  theme(
    legend.title = element_blank()
  )




## Relief Capacity
# how much capacity would we need to reach our TQS in the target time frame?

relief_capacity <-
  calc_relief_capacity(
    demand = demand,
    queue_size = 3098, ## last known queue size
    target_queue_size = TQS, ## give the function our target queue size
    time_to_target = 39.29 ## number of weeks between end of the current waiting list and end march 27
  )

relief_capacity

# calc_relief_capacity:
# The core calculation is required capacity = demand + (current queue - target queue) / time to target
# This accounts for new demand coming in each week as well as adding enough extra capacity each week to reduce the existing waiting list down to the target


# how much additional capacity is required to meet the target?
add_cap <- relief_capacity - capacity
add_cap


## We can then simulate another projection line, using our releif capacity
referral_date <- as.Date("2026-06-29")

current_wl <-
  data.frame(
    Referral = rep(referral_date, 3098),
    Removal = rep(as.Date(NA), 3098)
  )

sim_RC <- wl_queue_size(
  wl_simulator(
    start_date = referral_date,
    end_date = end_march_27,
    demand = demand,
    capacity = relief_capacity,
    waiting_list = current_wl
  )
)

sim_RC$type <- "Capacity: Increased to Meet 92% Target by 31/03/27"


## We can plot this on our chart
df_plot <- bind_rows(
  original_wl,
  sim_DND,
  sim_increase_cap_250,
  sim_RC
)

linetype_map <- c(
  "Historic WL" = "solid",
  "Capacity: DND" = "dashed",
  "Capacity: Increase by 50" = "dashed",
  "Capacity: Increased to Meet 92% Target by 31/03/27" = "dashed"
)

linewidth_map <- c(
  "Historic WL" = 1.2,
  "Capacity: DND" = 1.6,
  "Capacity: Increase by 50" = 1.6,
  "Capacity: Increased to Meet 92% Target by 31/03/27" = 1.6
)


# End-of-line labels = additional capacity
end_labels <- df_plot %>%
  group_by(type) %>%
  slice_max(dates, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    label_value = case_when(
      type == "Capacity: DND" ~
        round(capacity - capacity, 1),

      type == "Capacity: Increase by 50" ~
        round(scen2_cap - capacity, 1),

      type == "Capacity: Increased to Meet 92% Target by 31/03/27" ~
        round(relief_capacity - capacity, 1),

      TRUE ~ NA_real_
    )
  )

ggplot(
  df_plot,
  aes(
    x = dates,
    y = queue_size,
    colour = type,
    group = type
  )
) +

  geom_line(
    aes(
      linetype = type,
      linewidth = type
    )
  ) +

  # End-of-line additional capacity labels
  geom_text(
    data = end_labels %>% filter(!is.na(label_value)),
    aes(
      x = dates,
      y = queue_size,
      label = label_value,
      colour = type
    ),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +

  scale_linewidth_manual(
    values = linewidth_map
  ) +

  scale_linetype_manual(
    values = linetype_map
  ) +

  scale_colour_manual(
    values = c(
      "Historic WL" = "#000000",
      "Capacity: DND" = "#6A6A68",
      "Capacity: Increase by 50" = "#377EB8",
      "Capacity: Increased to Meet 92% Target by 31/03/27" = "#E41A1C"
    )
  ) +

  # Target queue size
  geom_hline(
    yintercept = TQS,
    linetype = "dashed",
    colour = "black"
  ) +

  annotate(
    "text",
    x = max(df_plot$dates) - lubridate::days(75),
    y = TQS + 130,
    label = stringr::str_wrap(
      "Target Waiting List Size for 92% seen by 18 weeks",
      width = 30
    ),
    hjust = 1,
    colour = "black",
    size = 3
  ) +

  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.01, 0.10))
  ) +

  labs(
    title = "Example Simulated Waiting List Trajectory",
    subtitle = paste0(
      "Weekly Referrals = ", demand,
      "   Baseline Capacity = ", capacity
    ),
    x = "Date",
    y = "Queue Size"
  ) +

  theme_minimal() +

  guides(
    colour = guide_legend(
      override.aes = list(linewidth = 1)
    )
  ) +

  theme(
    legend.title = element_blank()
  )


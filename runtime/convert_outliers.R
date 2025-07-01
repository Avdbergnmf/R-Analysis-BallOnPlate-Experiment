# Convert old outliers.csv format to new dual-table format
# KILL entries -> heel strike outliers (removed heel strikes)
# KEEP entries -> step outliers (outlier steps)

# Load required libraries
library(dplyr)

# Read the current outliers data
outliers_data <- read.csv("./data_extra/outliers.csv", stringsAsFactors = FALSE)

# Check if data was loaded properly
if (nrow(outliers_data) == 0) {
    stop("No data found in outliers.csv")
}

print(paste("Total outliers found:", nrow(outliers_data)))
print(paste("KILL entries:", sum(outliers_data$action == "KILL")))
print(paste("KEEP entries:", sum(outliers_data$action == "KEEP")))

# Check column names
print("Column names:")
print(colnames(outliers_data))

# 1. Create heel strike outliers from KILL entries
heelstrike_outliers <- outliers_data %>%
    dplyr::filter(action == "KILL") %>%
    dplyr::select(participant, trialNum, time) %>%
    dplyr::arrange(participant, trialNum, time)

print(paste("Heel strike outliers created:", nrow(heelstrike_outliers)))

# 2. Create step outliers from KEEP entries
# For KEEP entries, we need to create time ranges (time_start, time_end) for plotting
# AND store the original heel strike time for integration with outlier removal code
# Strategy: For each KEEP entry, find the next heel strike to create a step range

keep_entries <- outliers_data %>%
    dplyr::filter(action == "KEEP") %>%
    dplyr::arrange(participant, trialNum, time)

step_outliers <- data.frame(
    participant = character(),
    trialNum = numeric(),
    time_start = numeric(),
    time_end = numeric(),
    heelstrike_time = numeric(), # Original heel strike time for outlier removal code
    stringsAsFactors = FALSE
)

if (nrow(keep_entries) > 0) {
    # For each KEEP entry, we need to estimate the step range
    # We'll use a simple approach: assume steps are roughly 1 second long
    # This is a rough approximation - users can adjust manually later

    for (i in 1:nrow(keep_entries)) {
        keep_entry <- keep_entries[i, ]

        # Look for the next heel strike in the same participant/trial
        # First check if there are other heel strikes in the data
        same_trial_data <- outliers_data %>%
            dplyr::filter(
                participant == keep_entry$participant,
                trialNum == keep_entry$trialNum,
                time > keep_entry$time
            ) %>%
            dplyr::arrange(time)

        if (nrow(same_trial_data) > 0) {
            # Use the next heel strike as the end time
            time_end <- same_trial_data$time[1]
        } else {
            # No next heel strike found, estimate based on typical step duration
            time_end <- keep_entry$time + 1.0 # Assume 1 second step
        }

        step_outlier <- data.frame(
            participant = keep_entry$participant,
            trialNum = keep_entry$trialNum,
            time_start = keep_entry$time,
            time_end = time_end,
            heelstrike_time = keep_entry$time, # Store original heel strike time
            stringsAsFactors = FALSE
        )

        step_outliers <- rbind(step_outliers, step_outlier)
    }
}

print(paste("Step outliers created:", nrow(step_outliers)))

# Save the new files
write.csv(heelstrike_outliers, "./data_extra/heelstrike_outliers.csv", row.names = FALSE)
write.csv(step_outliers, "./data_extra/step_outliers.csv", row.names = FALSE)

print("✅ Conversion complete!")
print("Files created:")
print("- ./data_extra/heelstrike_outliers.csv")
print("- ./data_extra/step_outliers.csv")

# Show preview of the results
print("\n--- Heel Strike Outliers Preview ---")
print("Columns: participant, trialNum, time")
print(head(heelstrike_outliers, 10))

if (nrow(step_outliers) > 0) {
    print("\n--- Step Outliers Preview ---")
    print("Columns: participant, trialNum, time_start, time_end, heelstrike_time")
    print(head(step_outliers, 10))
} else {
    print("\n--- No Step Outliers Found ---")
}

# Create a summary by participant
summary_data <- heelstrike_outliers %>%
    dplyr::group_by(participant) %>%
    dplyr::summarise(
        heel_strike_outliers = n(),
        .groups = "drop"
    )

if (nrow(step_outliers) > 0) {
    step_summary <- step_outliers %>%
        dplyr::group_by(participant) %>%
        dplyr::summarise(
            step_outliers = n(),
            .groups = "drop"
        )

    summary_data <- summary_data %>%
        dplyr::left_join(step_summary, by = "participant") %>%
        dplyr::mutate(step_outliers = ifelse(is.na(step_outliers), 0, step_outliers))
} else {
    summary_data$step_outliers <- 0
}

print("\n--- Summary by Participant ---")
print(summary_data)

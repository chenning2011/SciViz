# dataset needed for apa_bar
###### in the original apa_bar function, the rxyogen section contian three examples, please delete the first one : )
# Example 1: Sample data for depression scores by treatment groups
barData1 <- data.frame(
    Group = rep(c("Control", "Treatment"), each = 5),
    Time = rep(c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"), times = 2),
    Depression_Scores = c(20, 22, 19, 21, 23, 25, 27, 26, 28, 30),
    Error_Values = c(2, 1, 3, 1, 2, 1, 2, 1, 2, 2)  # Example error values
  )

# Example 2: Imbalanced Scenario
barData2 <- data.frame(
  Activity = rep(c("Running", "Swimming", "Cycling"), each = 4),
  Session_Type = rep(c("Weekday", "Weekend"), times = 6),
  Mean_Duration = c(30, 40, 25, 35, 45, 20, 50, 30, 55, 25, 60, 40),
  Error_Value = c(5, 7, 4, 6, 5, 3, 6, 4, 8, 5, 7, 3)
)

#____________________________________________________________________________________


# dataset needed for apa_line
# Example data for the line chart
lineData1 <- data.frame(
  Time = rep(1:5, 3),
  Value = c(10, 12, 15, 14, 18, 8, 9, 11, 13, 14, 20, 22, 21, 23, 24),
  Group = rep(c("Group 1", "Group 2", "Group 3"), each = 5),
 Error = c(1, 1, 2, 1, 2, 0.5, 1, 1.5, 1, 1, 1, 1, 1, 1, 1)  # Example error values
)


#____________________________________________________________________________________





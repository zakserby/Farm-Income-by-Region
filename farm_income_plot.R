# =============================================================
# AEM 6850: farm income by USDA climate hub region (multi-panel polygon plots and a US map, exported as png)
# Zachary Serby
# -------------------------------------------------------------
# =============================================================

# Load libraries
library("maps")

# -------------------------------------------------------------
# 1. Let's import some data
# -------------------------------------------------------------

# Importing Midwest Income data
data <- read.csv("ers_farm_inc_climate_hubs.csv", stringsAsFactors=F) # point to where the file is
data2 <- data

# Cleaning the data
head(data2)
data2$value <- data2$value / 1e9

# Clean data for United States by aggregating
United_States <- aggregate(value ~ year + variable, data = data2, FUN = sum, na.rm = TRUE)
# Add name_region column
United_States$name_region <- "United States"
# Combine with original dataset
data2 <- rbind(data2, United_States)

# Save PNG
png("farm_income_by_region.png", 
    width = 760*4, height = 528*4, res = 100*4)

# -------------------------------------------------------------
# 2. Create matrix layout for multi-panel plots
# -------------------------------------------------------------

# Multi panel plots:
# Create a "layout" matrix
par(mar=c(1.5,1.5,1.5,1.5), oma=c(6,6,6,6)) # add some outer margins
lmat <- matrix(c(1,1,2,3,1,1,4,5,6,7,8,9), nrow=3, byrow=T)
layout(lmat, widths=c(1,1,1,1), heights=c(1,1,1))

# -------------------------------------------------------------
# 3. Create plots for United States and Regions
# -------------------------------------------------------------

# Creating character string to list regions
regions <- c("United States", "Midwest", "Northeast", "Northern Plains", "Northwest", "Southeast", "Southern Plains", "Southwest")

# Creating loop to reiterate the process in Step (1)
for (x in regions) {
  
  # Retrieve x values, "year"
  years <- data2$year[data2$name_region == x & data2$variable == "Cash Receipts"]
  
  # Retrieve y values for each variable
  Net_Cash <- data2$value[data2$name_region == x & data2$variable == "Net Cash Income"]
  Cash_Receipts <- data2$value[data2$name_region == x & data2$variable == "Cash Receipts"]
  Other_Farm <- data2$value[data2$name_region == x & data2$variable == "Other Farm-related Income"]
  Government_Payments <- data2$value[data2$name_region == x & data2$variable == "Government Payments"]
  Total_Cash <- data2$value[data2$name_region == x & data2$variable == "Total Cash Expense"]
  
  # Stack Government Payments and Other Farm-related Income
  Other_Farm_Stack <- Cash_Receipts + Other_Farm
  Government_Payments_Stack <- Cash_Receipts + Other_Farm + Government_Payments
  
  # Sort years
  idx <- order(years)
  
  # Set y-axis bounds depending on each regions min and max
  ymin <- min(c(Net_Cash, Cash_Receipts, Other_Farm_Stack, Government_Payments_Stack, Total_Cash), na.rm = TRUE)
  ymax <- max(c(Net_Cash, Cash_Receipts, Other_Farm_Stack, Government_Payments_Stack, Total_Cash), na.rm = TRUE)
  
  # Increasing the y-axis range for Midwest and Northeast plots for the code that inputs manual tick marks below
  if (x == "Midwest") {
    ymin <- min(ymin, -50); ymax <- max(ymax, 100)
  } else if (x == "Northeast") {
    ymin <- min(ymin, -15); ymax <- max(ymax, 20)
  } 
  
  # Basic Plot for Net Cash Income
  plot(years[idx], Net_Cash[idx], type="l", col="red", ylim=c(ymin, ymax), xlab="", ylab="", yaxt="n")
  # Use if, else if statements to customize y-axis labels for different plots
  if (x == "Midwest") {
    axis(2, at = c(100, 50, 0, -50), las = 2)
  } else if (x == "Northern Plains") {
    axis(2, at = c(40, 20, 0, -20), las = 2)
  } else if (x == "Northeast") {
    axis(2, at = c(20, 15, 10, 5, 0, -5, -10, -15), las = 2)
  } else {
    axis(2, las = 2)  # default automatic ticks for other regions
  }
  # Add points
  points(years[idx], Net_Cash[idx], col="red", pch=16, cex=0.5)
  
  # Add Cash Receipts line
  lines(years[idx], Cash_Receipts[idx], lty=2, col="grey70", type="n")
  # Add Other Farm-related Income stacked line
  lines(years[idx], Other_Farm_Stack[idx], lty=2, col="grey80", type="n")
  # Add Government Payments stacked line
  lines(years[idx], Government_Payments_Stack[idx], lty=2, col="grey90", type="n")
  # Add Total Cash Expense line
  lines(years[idx], Total_Cash[idx], lty=2, col="#f4d47a", type="n")
  
  # Add horizontal abline
  abline(h=0, lty=2)
  
  # Shaded areas (polygons)
  # Cash Receipts polygon
  polygon(x=c(years[idx],rev(years[idx])), y = c(Cash_Receipts[idx], rev(rep(0, length(idx)))), 
          col = adjustcolor("grey70", alpha.f = 0.25), border=NA)
  # Other Farm-related Income Polygon
  polygon(x=c(years[idx],rev(years[idx])), y = c(Other_Farm_Stack[idx], rev(Cash_Receipts[idx])), 
          col = adjustcolor("grey80", alpha.f = 0.25), border=NA)
  # Government Payments Polygon
  polygon(x=c(years[idx],rev(years[idx])), y = c(Government_Payments_Stack[idx], rev(Other_Farm_Stack[idx])), 
          col = adjustcolor("grey90", alpha.f = 0.25), border=NA)
  # Total Cash Expense Polygon
  polygon(x=c(years[idx],rev(years[idx])), y = c(rep(0, length(idx)), rev(Total_Cash[idx])), 
          col = adjustcolor("#f4d47a", alpha.f = 0.25), border=NA)
  
  # Add some text outside
  mtext(x, side=3, line=0, cex=.6)
  
  # Add condition to add a legend only for the United States
  if (x == "United States") {
    # Add legend with black border squares and transparent background
    legend("bottomleft", c("Net Cash Income", "Cash Receipts", "Other Farm-related Income", "Government Payments", "Total Cash Expense"),
           col = c("red", "black", "black", "black", "black"),   # Borders and border/line colors
           lty = c(1, NA, NA, NA, NA),                           # Create line for Net Cash Income and none for others
           pch = c(NA, 22, 22, 22, 22),                         # Don't fill line but fill all boxes
           pt.bg = c(NA, "grey70", "grey80", "grey90", "#fce8b3"),
           pt.cex = 1.8, # Increase size of boxes
           cex = 1, # Decrease text size
           y.intersp = 0.8,
           inset = c(0.05, 0), 
           bty = "n")     
  }
}

# -------------------------------------------------------------
# 3. Create USA map
# -------------------------------------------------------------

# Create data set with regions and states
states_data <- data.frame(region=c(rep("Midwest", 8), rep("Northeast", 12), rep("Northern Plains", 6), rep("Northwest", 3), rep("Southeast", 11), rep("Southern Plains", 3), rep("Southwest", 5)),
                          state = c("illinois","indiana","iowa","michigan","minnesota","missouri","ohio","wisconsin",
                                    "connecticut","delaware","maine","maryland","massachusetts","new hampshire","new jersey","new york","pennsylvania","rhode island","vermont","west virginia",
                                    "colorado","montana","nebraska","north dakota","south dakota","wyoming",
                                    "idaho","oregon","washington",
                                    "alabama","arkansas","florida","georgia","kentucky","louisiana","mississippi","north carolina","south carolina","tennessee","virginia",
                                    "kansas","oklahoma","texas",
                                    "arizona","california","nevada","new mexico","utah"),stringsAsFactors = FALSE)

# Assign colors to each region
region_colors <- c("Midwest"="#8DD3C7", "Northeast"="#FFFDB3", "Northern Plains"="#BEBADA", "Northwest"="#FB8072", "Southeast"="#80B1D3", "Southern Plains"="#FDB462", "Southwest"="#B3DE68")

# Get state names from the map
statenames <- map("state", plot = FALSE)$names
# Remove everything after the state's name
state <- sub(":.*", "", statenames)
# Match colors and regions
colorsmatched <- region_colors[states_data$region[match(state, states_data$state)]]

# Adjust the margins for the USA map plot
par(mar = c(.6, .2, 0, .2))

# Draw map
map("state", col=colorsmatched, fill=TRUE, border="#666666", lty=1, lwd=1, ylim = c(20, 200), projection="polyconic")
legend("bottom", legend = names(region_colors), fill = region_colors, border="#666666", ncol=2, pt.cex = .8, cex = .6, x.intersp = 0.5, y.intersp = 0.9, xpd = TRUE, inset = c(0, -0.1), bty = "n")

# Add y-axis title
mtext("Value in $US Billions", side = 2, outer = TRUE, line = 2, cex = 1)

# Finish saving png, shut off display
dev.off()

sessionInfo()
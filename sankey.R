rm(list=ls())

library(plotly)
library(dplyr)

######### define colors (there are as many needed as there are nodes)
colors <- list()

# colors for nodes (in general: 1.0 opacity)
colors$nodes <- c("rgba(155, 134, 180, 1.0)", "rgba(200, 104, 102, 1.0)", "rgba(91, 137, 193, 1.0)", "rgba(171, 198, 116, 1.0)", "rgba(203, 217, 235, 1.0)", "rgba(248, 164, 94, 1.0)", "rgba(153, 153, 153, 1.0)", "rgba(62, 133, 119, 1.0)", "rgba(209, 73, 151, 1.0)", "rgba(133, 173, 255, 1.0)")

# colors for links between nodes (in general: 0.5 opacity)
colors$links <- c("rgba(155, 134, 180, 0.5)", "rgba(200, 104, 102, 0.5)", "rgba(91, 137, 193, 0.5)", "rgba(171, 198, 116, 0.5)", "rgba(203, 217, 235, 0.5)", "rgba(248, 164, 94, 0.5)", "rgba(153, 153, 153, 0.5)", "rgba(62, 133, 119, 0.5)", "rgba(209, 73, 151, 0.5)", "rgba(133, 173, 255, 0.5)")

# matching regions with colors
colorMatch <- function(region, opacity = 1){
  if(opacity == 1){
    return(colors$nodes[match(region, regions)])
  }else{
    return(colors$links[match(region, regions)])
  }
}

######### read data

# land use -> industrial processing
lu_ip = read.csv("./input/lu-ip.csv", sep = ";", header = T, strip.white = T)
# industrial processing -> consumption
ip_con = read.csv("./input/ip-con.csv", sep = ";", header = T, strip.white = T)

regions <- c("EU-28", "USA", "Brazil", "China", "India", "Indonesia", "Rest of Europe", "Rest of Asia-Pacific", "Rest of America", "Africa")
regions <- factor(regions, levels = c("EU-28", "USA", "Brazil", "China", "India", "Indonesia", "Rest of Europe", "Rest of Asia-Pacific", "Rest of America", "Africa"))

# add colors to links based on the From.Region
lu_ip$color <- colorMatch(region = lu_ip$From.Region, opacity = 0.5)
ip_con$color <- colorMatch(region = ip_con$From.Region, opacity = 0.5)

# add suffixes to _every_ region so there are unique region names (as they will be different nodes)
lu_ip$From.Region <- paste0(lu_ip$From.Region, "_lu")
lu_ip$To.Region <- paste0(lu_ip$To.Region, "_IP")
ip_con$From.Region <- paste0(ip_con$From.Region, "_IP")
ip_con$To.Region <- paste0(ip_con$To.Region, "_con")

# create data that combines the two data files
data <- NULL
data <- rbind(lu_ip, ip_con)

# aggregate the data for the sums in the brackets
aggregate.From <- data %>% group_by(From.Region) %>% summarise(sum(Value))
aggregate.To <- data %>% group_by(To.Region) %>% summarise(sum(Value))
## change colnames to make them merge-able
colnames(aggregate.From) <- c("Region", "Sum")
colnames(aggregate.To) <- c("Region", "Sum")
## bind the aggregates together
aggregate <- rbind(aggregate.From, aggregate.To)

## remove all _IP rows (they are the middle-nodes and we don't provide a sum for them)
aggregate <- aggregate[!gregexpr(pattern = "_IP",aggregate$Region) > -1,]

#### create the nodes-list (those are the nodes needed for the sankey)
nodes <- list()
nodes$u_label <- unique(c(unique(data$From.Region), unique(data$To.Region)))
nodes$label <- substr(nodes$u_label, 0, as.numeric(gregexpr(pattern ="_",nodes$u_label))-1)
nodes$index <- factor(nodes$u_label, levels = unique(nodes$u_label))

nodes <- data.frame(nodes) # convert to data.frame so the left-join is possible
nodes <- nodes %>% left_join(aggregate, by = c("u_label" = "Region")) # adding the sum
## adding colors to the nodes
nodes$color <- colorMatch(nodes$label, opacity = 1)

## adding the label to the nodes that will be displayed, so far I don't use html but it's just called html_label
nodes$html_label <- ""
nodes$html_label <- ifelse(!is.na(nodes$Sum), # checking if there is a sum (this is true for the left and right nodes)
                            sprintf("%s (%.1f Mha)", nodes$label, nodes$Sum/1000, digits = 1), # providing the sum in million ha
                            "" #as.character(nodes$label)
)

# create node-indizes that are used for the links
data$From.RegionIndex <- data$From.Region
data$From.RegionIndex <- factor(data$From.RegionIndex, levels = levels(nodes$index))
data$To.RegionIndex <- data$To.Region
data$To.RegionIndex <- factor(data$To.RegionIndex, levels = levels(nodes$index))

# now, let's plot the sankey

## IMPORTANT NOTE ##
## plotly arranges the nodes in some weird way and I haven't found a way to rearrange them in the code
## this means in order to export the plot, you need to open it in the browser (ideally; the RStudio viewer works as well) 
## resize everything as you see fit and then make a screenshot

plot_ly(
  type = "sankey",
  orientation = "h", # alternative: v
  #valueformat = ".0f",
  valuesuffix = " (1000 ha)",
  # Full HD resolution for exporting purpose in case needed, make sure to adjust font size
  # width = 1920,
  # height = 1080,
  # iterations = 0,
  
  arrangement = "snap", # default: "snap"
  textfont = list(
    # family = ,
    size = 14,
    color = "black"
  ),
  
  node = list(
    label = nodes$html_label,
    color = nodes$color,
    pad = 5,
    thickness = 20,
    line = list(
      #color = colors$nodes[5],
      width = 0 # 0 width because it doesn't look good
    )
  ),
  
  link = list(
    source = as.numeric(data$From.RegionIndex)-1,
    target = as.numeric(data$To.RegionIndex)-1,
    value = data$Value,
    color = data$color
    #label = paste(aggregate$`sum(Value)`, "test")
  )
) %>%
  layout(
    # title = "Test",
    #paper_bgcolor = "green",
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
  )

library(tidyverse)

prompt_keys <- read.csv2("path/to/prompt_keys.csv")
responses <- read.csv2("path/to/responses.csv")
gbd_dw <- read.csv("path/to/gbd_dw.csv")

# Assign IDs to each LLM iteration
prompt_keys$id <- 1:402
responses$id <- 1:402

# Convert the response columns to character
responses <- responses |>
  mutate(across(starts_with("X"), as.character))

# Join the prompt_keys and responses datasets by ID
combined <- left_join(prompt_keys, responses, by = "id", suffix = c("_prompt", "_response"))

# Reshape the combined dataset to long format
long_data <- combined |>
  pivot_longer(
    cols = starts_with("X"), 
    names_to = c("question", "type"),
    names_sep = "_"
  )

# Separate the choices provided into two columns, choice_A and choice_B
long_data <- long_data |>
  pivot_wider(names_from = type, values_from = value) |>
  separate(prompt, into = c("choice_A", "choice_B"), sep = ";") |>
  rename(selected_choice = response)

# Arrange and clean up the data
final_data <- long_data |>
  select(id, question, choice_A, choice_B, selected_choice)

# Rename columns for clarity
final_data <- final_data |>
  rename(description_first_person = "choice_A",
         description_second_person = "choice_B",
         answer_matrix = "selected_choice")

# Remove leading/trailing spaces and create a unique ID for each description
unique_descriptions <- final_data |>
  mutate(description_first_person = str_trim(description_first_person),
         description_second_person = str_trim(description_second_person)) |>
  select(description_first_person, description_second_person) |>
  pivot_longer(cols = everything(), values_to = "description") |>
  distinct(description) |>
  mutate(description_id = row_number())

# Remove repeated question 14
final_data_no_rep <- final_data |> filter(question != "X14")

# Replace the descriptions in final_data_norep with their corresponding IDs
final_data_numeric <- final_data_no_rep |>
  mutate(description_first_person = str_trim(description_first_person),
         description_second_person = str_trim(description_second_person)) |>
  left_join(gbd_dw, by = c("description_first_person" = "description")) |>
  rename(description_first_person_id = description_id) |>
  left_join(gbd_dw, by = c("description_second_person" = "description")) |>
  rename(description_second_person_id = description_id) |>
  select(id, question, description_first_person_id, description_second_person_id, answer_matrix)

final_data_numeric <- final_data_numeric |>
  rename(description_first_person = description_first_person_id,
         description_second_person = description_second_person_id)


# Save dataset to dataset entitled "data"
data <- final_data_numeric

# create dummy when choosing first person
data$choosefirst <- as.numeric(data$answer_matrix=="A")

# function that maps probabilities to heat colors (i.e., returns matrix)
color.map <- function(heat.mat, rgb.color=color){
  # bin the heat.mat values into the subintervals
  heat.values = as.vector(heat.mat)
  binned = cut(x=heat.values, breaks=seq(from=0, to=1, by=1/length(rgb.color)),
               
               right=T, include.lowest=T, labels=rgb.color)
  bin.mat = matrix(binned, nrow=nrow(heat.mat), byrow = F,
                   
                   dimnames=list(rownames(heat.mat), colnames(heat.mat)))
  
  return(bin.mat)
}

# rgb colors for images
red <- c(rep(27, 28), seq(26, 0, -1), rep(0, 27), rep(0, 27))
green <- c(seq(0, 27, 1), rep(27, 27), rep(27, 27), seq(26, 0, -1))
blue <- c(rep(0, 28), rep(0, 27), seq(1, 27, 1), rep(27, 27))
color <- rgb(red = red, green = green, blue = blue, maxColorValue = 27)

# create probability of choosing first person
# for each combination of description_first_person and
# description_second_person in the data set
summarized.data <- data |>
  group_by(description_first_person, description_second_person) |>
  summarise(prob=mean(choosefirst))

# create matrix probabilities
nr.health.states <- 28
matrix.prob <- matrix(NA, nrow = 28, ncol = 28)

for (A in sort(unique(as.numeric(data$description_first_person)))){
  for (B in sort(unique(as.numeric(data$description_second_person)))){
    # Extract the probability value from summarized.data
    prob_value <- subset(summarized.data,
                         description_first_person == A &
                           description_second_person == B)[, "prob"]
    
    # Assign the probability value to the correct position in the matrix
    if (length(prob_value) > 0) {
      matrix.prob[A, B] <- as.numeric(prob_value)
    } else {
      matrix.prob[A, B] <- NA  # Or another default value
    }
  }
}

# order the probability matrix
matrix.prob <- matrix.prob[order(rowMeans(matrix.prob, na.rm=TRUE),
                                 
                                 decreasing=TRUE),
                           order(colMeans(matrix.prob, na.rm=TRUE))]

# parameter that controls the size of the color gradient legend (optional)
# note: it must be odd, decrease n to make legend larger/
# increase n to make it smaller
n <- 3

# converts data frame into a matrix
heatProbs <- data.matrix(frame=matrix.prob)
heatMatrix <- color.map(heat.mat=heatProbs, rgb.color=color)

# 4:1 ratio for heatmap and legend
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1)) 

# plot an empty initial graph
original_par <- par()
par(mar=rep(2, 4))
plot(x = c(), y = c(), xlim = c(0,1), ylim = c(0,1), xlab = "",
     ylab = "", axes = F, main="", cex.main=2)

# assign plot parameters
x.a <- par()$usr[1]; x.b <- par()$usr[2];
y.a <- par()$usr[3]; y.b <- par()$usr[4];
# calculate step sizes
h.x <- (x.b - x.a) / nrow(heatMatrix)
h.y <- (y.b - y.a) / ncol(heatMatrix)
# mesh coordinates of heat maps
x.s <- x.a + (0:nrow(heatMatrix)) * h.x
y.s <- y.a + (0:ncol(heatMatrix)) * h.y

for (sims2 in 1:nrow(heatMatrix)){
  rect(xleft = rep(x.s[sims2], ncol(heatMatrix)),
       ybottom = y.s[-length(y.s)],
       xright = rep(x.s[sims2+1], ncol(heatMatrix)),
       ytop = y.s[-1], col = heatMatrix[sims2,], border = NA)
}
# add box
box()

# Plot the gradient legend
par(mar = c(2, 1, 2, 4)) # Adjust margins for the legend
image(x = 1, y = seq(0, 1, length = length(color)), z = t(matrix(seq(0, 1, length = length(color)))),
      col = color, axes = FALSE, xlab = "", ylab = "", xlim = c(1.05, 1.1))

# Add axis to the gradient legend
axis(4, at = seq(0, 1, by = 0.25), labels = paste0(seq(0, 100, by = 25), "%"), las = 1)
mtext("Probability", side = 4, line = 3)

par(original_par)
par(mfrow=c(1,1))

# create empty dummies
nr.health.states <- 28
list.state.nrs <- sort(unique
                       
                       (as.numeric(as.character(data$description_first_person))))

data[, paste0('picked', list.state.nrs)] <- rep(0, nrow(data))
# create indicator variables that indicate
# 1 if that health state is chosen as the healthier
# -1 if the health state is not chosen as the healthier
# 0 if that health state was not considered
for (state in list.state.nrs){
  data[which(data$description_first_person==state), paste0('picked', state)] <- 1
  data[which(data$description_second_person==state), paste0('picked', state)] <- -1
}
data <- data.frame(data)
# probit regression
formula <- paste("choosefirst ~ -1 + ",
                 
                 paste(names(data)[7:34], collapse=" + "),
                 sep="")

print(formula)

output_All <- stats::glm(formula, family = binomial(link="probit"), data=data)

# picked28 is the reference category
coef_All <- summary(output_All)$coefficients[, 1]

# define logit and expit functions to calculate disability weights
logit <- function(x) log(x / (1 - x))
expit <- function(x) exp(x) / (1 + exp(x))
## GBD data
gbd_dw_reordered <- gbd_dw[match(unique_descriptions$description_id, gbd_dw$description_id), ]
GBD <- gbd_dw_reordered$dw

# select coefficients
coef_All <- c(coef(output_All)[-nr.health.states], 0) #repl. coef of ref. by 0
names(coef_All) <- paste0('picked', list.state.nrs)
# run a non-parametric regression model (loess) of the probit regression
# coefficients against the logit-transformed disability weights from GBD
fit_loess <- loess(logit(GBD) ~ coef_All,
                   
                   control=loess.control(surface="direct"))

# predicted logit transformed disability weights for each of the probit coef
pred_GBD <- predict(fit_loess, coef_All, se = TRUE)
# plot of LOESS regression
plot(logit(GBD) ~ coef_All, xlab=paste0("Coefficients of Pooled Paired Comparisons"), ylab=paste0("Logit-Transformed GBD Disability Weights"))
test <- data.frame(x=coef_All, y=pred_GBD$fit)
ordered <- test[order(test$x),]
lines(ordered$x, ordered$y, col="blue")

# calculate R-square
ss.resid <- sum(fit_loess$residuals^2)
ss.dist <- sum((logit(GBD)-mean(logit(GBD), na.rm=TRUE))^2, na.rm=TRUE)
Rsquared_GBD_All <- 1-ss.resid/ss.dist
# calculate disability weights using bootstrapping
coef_se <- summary(output_All)$coefficients[, c("Estimate", "Std. Error")]
n_samples <- 1000
# sample coefficients
coef_sim <- apply(coef_se, 1, function(x) rnorm(n_samples, x[1], x[2]))

dw_sim <-
  apply(coef_sim,
        1,
        function(x) {
          fit_loess <- loess(logit(GBD) ~ c(x, 0),
                             
                             control=loess.control(surface="direct"))
          
          pred <- predict(fit_loess, c(x, 0), se = TRUE)
          dw_sim <- mapply(rnorm,
                           
                           mean = pred$fit, sd = pred$se.fit,
                           MoreArgs = list(n = n_samples))
          
        })
dw_sim <- expit(dw_sim)

# calculate mean and CI of disability weights
summarize <-
  function(x, round = 3) {
    m <- mean(x, na.rm = TRUE)
    ci <- quantile(x, c(.025, .975), na.rm = TRUE)
    n_na <- sum(is.na(x))
    if (n_na > 0) warning("there were", n_na, "NA")
    return(round(c(mean = m, ci), round))
  }

# mean disbability weights using numerical integration
out <- matrix(ncol = 3, nrow = nr.health.states)
for (i in seq(nr.health.states)) {
  out[i, ] <- summarize(dw_sim[(((i - 1) * n_samples) + 1):(i * n_samples), ])
}
# store results
DW_table <- data.frame(names=paste0('picked', list.state.nrs))
DW_table[, "DW_GBD_All"] <- out[,1]
DW_table[, "CI_lower_GBD_All"] <- out[,2]
DW_table[, "CI_upper_GBD_All"] <- out[,3]

# applied an inverse logit transformation at the draw level to predicted dw
plot(expit(pred_GBD$fit), coef_All)

# check that bootstrapped DW are similar to DW from probit analysis
round(expit(pred_GBD$fit)-DW_table[, "DW_GBD_All"], 3)

# Extract the numeric part of the 'names' column to create an ID column
DW_table <- DW_table |>
  mutate(description_id = as.numeric(gsub("picked", "", names)))

# Join the DW_table with unique_descriptions based on 'description_id'
joined_data <- DW_table |>
  inner_join(unique_descriptions, by = "description_id")

# Join the DW_table with original GBD DW based on 'description_id'
joined_data <- joined_data |>
  left_join(gbd_dw, by = "description_id") |>
  rename(original_dw = dw,
         description = description.y) |>
  select(-description.x, -X)

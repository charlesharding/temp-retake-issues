## Plot formatting

# Custom ggplot2 theme
theme_pub <- theme_set(theme_bw()) #Base it on theme_bw()
theme_pub <- theme_pub + theme(
  plot.margin = unit(c(1/32, 1/16, 1/32, 1/32), "in"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank()
)
theme_pub <-  theme_pub + 
  theme(panel.border = element_rect(color="black", size = 0.25, fill = NA)) 
theme_pub <- theme_pub + theme(
  panel.background = element_rect(fill = "white"), 
  axis.ticks = element_line(colour = "black")
)

# Extended
theme_pub_ext <- theme_pub
theme_pub_ext <- theme_pub_ext + theme(
  axis.title.x = element_text(size = 9, vjust = 1.9), 
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 9, vjust = 0.15, angle = 90), 
  axis.text.y  = element_text(size = 8)
)
theme_pub_ext <- theme_pub_ext + theme(
  legend.title = element_text(face = "plain", size = 8),
  plot.title =  element_text(face = "bold", size = 9, vjust= 2, hjust = 0.5),
  plot.subtitle = element_text(face="bold", size= 9, hjust = 0.5),
  strip.text = element_text(face = "plain", size=8)
)
theme_pub_ext <- theme_pub_ext + theme(
  panel.grid.major.x = element_line(linetype = "solid", color = "gray92"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(linetype = "solid", color = "gray92"),
  panel.grid.minor.y = element_blank()
)
theme_pub_ext <- theme_pub_ext + theme(panel.border = element_blank())
theme_pub_ext <- theme_pub_ext + 
  theme(strip.background = element_rect(color = "gray92", fill = "gray92"))
theme_pub_ext <- theme_pub_ext + theme(line = element_line(size = 0.25))
theme_pub_ext <- theme_pub_ext + theme(
  axis.line = element_line(color = "black", linetype = "solid", size = 0.25),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)
theme_pub_ext <- theme_pub_ext + theme(text = element_text("Roboto Condensed")) 


# Functions for adding degree symbols to axes

degreeFormat <- function(x, ...) {
     parse(text = paste(x, "*degree", sep = ""))
    }

degreeF <- function(x, ...) {
     parse(text = paste(x, "*degree*F", sep = ""))
    }

degreeC <- function(x, ...) {
     parse(text = paste(x, "*degree*C", sep = ""))
    }
    
dF <- function(x) {
    # Degree formatter with 3 places before decimal and 1 after
    y <- as.character(round(x, 1))
    y[floor(x/100) == 0] <- paste0(" ", y[floor(x/100) == 0])
    y[x %% 1 == 0] <- paste0(y[x %% 1 == 0], ".0")
    paste0(y, "\n\u00B0F")
    }
 
dC <- function(x) {
    # Degree formatter with 2 places before decimal and 1 after
    y <- as.character(round(x, 1))
    y[floor(x/10) == 0] <- paste0(" ", y[floor(x/10) == 0])
    y[x %% 1 == 0] <- paste0(y[x %% 1 == 0], ".0")
     paste0(y, "\n\u00B0C")
    }
    
dFS <- function(x) {
    # Like dF, but without splitting the result across
    # two lines
    y <- as.character(round(x, 1))
    y[floor(x/100) == 0] <- paste0(" ", y[floor(x/100) == 0])
    y[x %% 1 == 0] <- paste0(y[x %% 1 == 0], ".0")
    paste0(y, "\u00B0F")
    }
    
dCS <- function(x) {
    # Like dC, but without splitting the result across
    # two lines
    y <- as.character(round(x, 1))
    y[floor(x/10) == 0] <- paste0(" ", y[floor(x/10) == 0])
    y[x %% 1 == 0] <- paste0(y[x %% 1 == 0], ".0")
     paste0(y, "\u00B0C")
    }

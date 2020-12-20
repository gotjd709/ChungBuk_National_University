### 머신러닝 3주차 과제 - 1번 문제 ###



## 1. 데이터 마이닝


# 데이터 불러들이기
bank <- read.csv(file = "C:/Users/ay190130/Desktop/데이터/bank-additional-full.csv", header = T)

# 결측치 제거하기
bank_nona <- na.omit(bank)

# y 변수의 "no"값을 "0" 으로, "yes"값을 "1"으로 변환 - 문자형 데이터
bank_nona$y <- sapply(bank_nona$y, function(x) gsub("no", 0, x))
bank_nona$y <- sapply(bank_nona$y, function(x) gsub("yes", 1, x))

# y 변수를 숫자형 데이터로 변환
bank_nona$y <- as.numeric(bank_nona$y)

# 데이터의 구조 확인
str(bank_nona)



## 2. 로지스틱 회귀분석


# 모형만들기
a <- glm(y ~ age + duration + campaign + pdays + previous + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data = bank_nona)

# 결과보기
summary(a)





#### 2번 문제 ####
prop.table(table(bank_nona$y))
barplot(prop.table(table(bank_nona$y)))
n_x <- length(bank_nona$y[bank_nona$y == 0])
n_n <- length(bank_nona$y)
prop.test(n = n_n, x = n_x, p=1/2, alt = 'two.sided')



#### 5번 문제 ####
install.packages("lubridate")
library(lubridate)
covid_19 <- read.csv("C:/Users/ay190130/Desktop/데이터/PatientInfo.csv")
str(covid_19)
covid_19$symptom_onset_date <- as.Date(covid_19$symptom_onset_date)
covid_19$confirmed_date <- as.Date(covid_19$confirmed_date)
covid_19$released_date <- as.Date(covid_19$released_date)
covid_19$deceased_date <- ymd(covid_19$deceased_date)
covid_19$deceased_date
str(covid_19)

install.packages("incidence")
library(incidence)
library(ggplot2)
i.7.group <- incidence(covid_19$confirmed_date, interval = 1) 

my_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"))
plot(i.7.group, border = "white") +
  my_theme +
  theme(legend.position = c(0.1, 0.6))

install.packages("magrittr")
library(gplots)
library(magrittr)

fit_optim_split <- function(x, window = x$timespan/4, plot = TRUE,
                            quiet = TRUE, separate_split = TRUE) {
  if (ncol(x$counts) > 1 && separate_split) {
    # Calculate split for each group separately --------------------------------
    res        <- vector(mode = "list", length = ncol(x$counts))
    names(res) <- colnames(x$counts)
    for (i in names(res)) {
      res[[i]] <- fit_optim_split(x[, i], separate_split = FALSE, plot = FALSE)
    }
    dates  <- get_dates(x)[[1]]
    dfrows <- vapply(res, function(i) nrow(i$df), integer(1))
    out <- list(
      df = data.frame(dates = seq(dates, by = 1, length.out = sum(dfrows)),
                      mean.R2 = vector(mode = "numeric", length = sum(dfrows)),
                      groups = factor(rep(names(res), dfrows), names(res)),
                      stringsAsFactors = TRUE
      ),
      plot = ggplot2::ggplot(),
      split = seq(dates, by = 1, length.out = length(res)),
      fit = vector(mode = "list", length = length(res))
    )
    names(out$fit)   <- names(res)
    names(out$plot)  <- names(res)
    names(out$split) <- names(res)
    for (i in names(res)) {
      n <- factor(i, names(res))
      out$fit[[i]]   <- res[[i]]$fit
      out$plot[[i]]  <- res[[i]]$plot
      out$split[[i]] <- res[[i]]$split
      out$fit[[i]]$after$info$pred$groups  <- n
      out$fit[[i]]$before$info$pred$groups <- n
      out$df[out$df$groups == i, ]$dates   <- res[[i]]$df$dates
      out$df[out$df$groups == i, ]$mean.R2 <- res[[i]]$df$mean.R2
    }
    if (plot) {
      out$plot <- ggplot2::ggplot(
        out$df,
        ggplot2::aes_string(x = "dates", y = "mean.R2", color = "groups")
      ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_text(ggplot2::aes_string(label="dates"),
                           hjust = -0.1, angle = 35
        ) +
        ggplot2::ylim(min = min(out$df$mean.R2) - 0.1, max = 1)
    } else {
      out$plot <- NULL
    }
    
    # Adding attributes for incidence_fit_list ---------------------------------
    attr(out$fit, "locations") <- c(
      lapply(names(res), c, "before"),
      lapply(names(res), c, "after")
    )
    class(out$fit) <- "incidence_fit_list"
    return(out)
  }
  date.peak <- x$dates[which.max(pool(x)$counts)]
  try.since <- date.peak - window / 2
  try.until <- date.peak + window / 2
  to.keep <- x$dates >= try.since & x$dates <= try.until
  if (sum(to.keep) < 1) {
    stop("No date left to try after defining splits to try.")
  }
  
  splits.to.try <- x$dates[to.keep]
  need.to.try   <- length(splits.to.try) > 1
  
  f <- function(split) {
    fits <- fit(x, split = split, quiet = quiet)
    mean(vapply(fits, function(e) summary(e$model)$`adj.r.squared`, double(1)), na.rm = TRUE)
  }
  
  results <- vapply(splits.to.try, f, double(1))
  
  ## shape output
  df <- data.frame(dates = splits.to.try, mean.R2 = results, stringsAsFactors = TRUE)
  split <- if (need.to.try) splits.to.try[which.max(results)] else splits.to.try
  fit <- suppressWarnings(fit(x, split = split))
  out <- list(df = df,
              split = split,
              fit = fit)
  
  if (plot) {
    out$plot <- ggplot2::ggplot(
      df, ggplot2::aes_string(x = "dates", y = "mean.R2")) +
      ggplot2::geom_point() + ggplot2::geom_line() +
      ggplot2::geom_text(ggplot2::aes_string(label="dates"),
                         hjust=-.1, angle=35) +
      ggplot2::ylim(min=min(results)-.1, max=1)
  }
  
  out
}

fos <- fit_optim_split(i.7.group)
fos$split
fos$fit

plot(i.7.group, border = "white") %>%
  add_incidence_fit(fos$fit) +
  my_theme

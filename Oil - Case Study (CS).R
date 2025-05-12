# Time Series Data
data <- read.csv("C:/Users/Admin/Desktop/Oil TTR/Oil(1983-2019).csv")
head(data)
tail(data)

install.packages("dplyr")
library(dplyr)
data[is.na(data)]<-0
df = data
names(df)

detect_separating_lines <- function(df) {
  conditions <- (
    lag(df$Close, 1) > lag(df$Open, 1) &  # Previous candle is bullish
      df$Close < df$Open &               # Current candle is bearish
      df$Open == lag(df$Open, 1)         # Open prices are the same
  ) | (
    lag(df$Close, 1) < lag(df$Open, 1) &  # Previous candle is bearish
      df$Close > df$Open &               # Current candle is bullish
      df$Open == lag(df$Open, 1)         # Open prices are the same
  )
  return(as.integer(conditions))
}

detect_bearish_on_neck <- function(df) {
  conditions <- (
    lag(df$Close, 1) < lag(df$Open, 1) &  # Previous candle is bearish (black)
      df$Close > df$Open &                # Current candle is bullish (white)
      df$Open < lag(df$Low, 1) &          # Current candle opens below previous day's low
      df$Close == lag(df$Low, 1)          # Closes at the low of the previous candle
  )
  return(as.integer(conditions))
}

detect_bullish_on_neck <- function(df) {
  conditions <- (
    lag(df$Close, 1) > lag(df$Open, 1) &  # Previous candle is bullish (white)
      df$Close < df$Open &                # Current candle is bearish (black)
      df$Open > lag(df$High, 1)           # Current candle opens above previous day's high
  )
  return(as.integer(conditions))
}


detect_bearish_in_neck <- function(df) {
  conditions <- (
    lag(df$Close, 1) < lag(df$Open, 1) &  # Previous candle is bearish (black)
      df$Close > df$Open &                # Current candle is bullish (white)
      df$Open < lag(df$Low, 1) &          # Opens below the first day's low
      abs(df$Close - lag(df$Close, 1)) < 0.002 * lag(df$Close, 1) # Closes barely into the previous body
  )
  return(as.integer(conditions))
}

detect_bullish_in_neck <- function(df) {
  conditions <- (
    lag(df$Close, 1) > lag(df$Open, 1) &  # Previous candle is bullish (white)
      df$Close < df$Open &                # Current candle is bearish (black)
      df$Open > lag(df$High, 1) &         # Opens above the first day's high
      abs(df$Close - lag(df$Close, 1)) < 0.002 * lag(df$Close, 1) # Closes barely into the previous body
  )
  return(as.integer(conditions))
}

detect_bearish_thrusting <- function(df) {
  conditions <- (
    lag(df$Close, 1) < lag(df$Open, 1) &  # Previous candle is bearish (black)
      df$Close > df$Open &                # Current candle is bullish (white)
      df$Open < lag(df$Low, 1) &          # Opens considerably lower than the previous day's low
      df$Close < (lag(df$Open, 1) + lag(df$Close, 1)) / 2 &  # Closes below the midpoint
      df$Close > lag(df$Close, 1)         # Closes within the previous body
  )
  return(as.integer(conditions))
}

detect_bullish_thrusting <- function(df) {
  conditions <- (
    lag(df$Close, 1) > lag(df$Open, 1) &  # Previous candle is bullish (white)
      df$Close < df$Open &                # Current candle is bearish (black)
      df$Open > lag(df$High, 1) &         # Opens above the first day's high
      df$Close > (lag(df$Open, 1) + lag(df$Close, 1)) / 2 &  # Closes above the midpoint
      df$Close < lag(df$Open, 1)         # Closes within the previous body
  )
  return(as.integer(conditions))
}


detect_upside_tasuki_gap <- function(df) {
  conditions <- (
    lag(df$Close, 2) > lag(df$Open, 2) &   # First candle is bullish (white)
      lag(df$Close, 1) > lag(df$Open, 1) & # Second candle is also bullish (white)
      lag(df$Open, 1) > lag(df$Close, 2) & # Gap up between first and second candle
      df$Close < df$Open &                 # Third candle is bearish (black)
      df$Open < lag(df$Close, 1) &         # Third candle opens within second candle's body
      df$Close > lag(df$Open, 1)           # Third candle closes into the gap but doesn't fully close it
  )
  return(as.integer(conditions))
}

detect_downside_tasuki_gap <- function(df) {
  conditions <- (
    lag(df$Close, 2) < lag(df$Open, 2) &   # First candle is bearish (black)
      lag(df$Close, 1) < lag(df$Open, 1) & # Second candle is also bearish (black)
      lag(df$Open, 1) < lag(df$Close, 2) & # Gap down between first and second candle
      df$Close > df$Open &                 # Third candle is bullish (white)
      df$Open > lag(df$Close, 1) &         # Third candle opens within second candle's body
      df$Close < lag(df$Open, 1)           # Third candle closes into the gap but doesn't fully close it
  )
  return(as.integer(conditions))
}


detect_side_by_side_white_lines <- function(df) {
  conditions <- (
    (lag(df$Close, 2) > lag(df$Open, 2) &  # First candle is bullish (white)
       lag(df$Close, 1) > lag(df$Open, 1) & # Second candle is bullish (white)
       df$Close > df$Open &                 # Third candle is also bullish (white)
       lag(df$Open, 1) > lag(df$Close, 2) & # Gap in the trend direction
       abs(df$Open - lag(df$Open, 1)) / df$Open < 0.01 &  # Third candle opens at about the same price as the second
       abs(df$Close - lag(df$Close, 1)) / df$Close < 0.01 # Third candle closes at about the same price as the second
    )
  )
  return(as.integer(conditions))
}


detect_bullish_side_by_side_black_lines <- function(df) {
  conditions <- (
    lag(df$Close, 3) > lag(df$Open, 3) &   # First candle is bullish (white) in an uptrend
      lag(df$Close, 2) < lag(df$Open, 2) & # Second candle is bearish (black)
      lag(df$Open, 2) > lag(df$Close, 3) & # Second candle opens above the first candle’s close (gap up)
      lag(df$Close, 2) > lag(df$Low, 3) &  # Second candle does not fill the gap
      lag(df$Close, 1) < lag(df$Open, 1) & # Third candle is bearish (black)
      lag(df$Open, 1) > (lag(df$Open, 2) + lag(df$Close, 2)) / 2 & # Third candle opens above the midpoint of the second
      lag(df$Close, 1) > lag(df$Low, 2)    # Third candle does not close low enough to fill the gap
  )
  return(as.integer(conditions))
}

detect_bearish_side_by_side_black_lines <- function(df) {
  conditions <- (
    lag(df$Close, 3) < lag(df$Open, 3) &   # First candle is bearish (black) in a downtrend
      lag(df$Close, 2) < lag(df$Open, 2) & # Second candle is also bearish (black)
      lag(df$Open, 2) < lag(df$Close, 3) & # Second candle opens below the first candle’s close (gap down)
      lag(df$Close, 2) < lag(df$Low, 3) &  # Second candle does not fill the gap
      lag(df$Close, 1) < lag(df$Open, 1) & # Third candle is bearish (black)
      lag(df$Open, 1) > lag(df$Close, 2) & # Third candle opens higher but does not fill the gap
      lag(df$Close, 1) < lag(df$Low, 2)    # Third candle closes down but does not fill the gap
  )
  return(as.integer(conditions))
}

detect_upside_gap_three_methods <- function(df) {
  conditions <- (
    lag(df$Close, 3) > lag(df$Open, 3) &   # First candle is bullish (white) in an uptrend
      lag(df$Close, 2) > lag(df$Open, 2) & # Second candle is also bullish (white)
      lag(df$Open, 2) > lag(df$Close, 3) & # Gap between first and second candle
      lag(df$Close, 1) < lag(df$Open, 1) & # Third candle is bearish (black)
      lag(df$Close, 1) < lag(df$Open, 2) & # Third candle closes within the gap
      lag(df$Close, 1) > lag(df$Close, 3)  # Third candle does not fully close the gap
  )
  return(as.integer(conditions))
}

detect_downside_gap_three_methods <- function(df) {
  conditions <- (
    lag(df$Close, 3) < lag(df$Open, 3) &   # First candle is bearish (black) in a downtrend
      lag(df$Close, 2) < lag(df$Open, 2) & # Second candle is also bearish (black)
      lag(df$Open, 2) < lag(df$Close, 3) & # Gap between first and second candle
      lag(df$Close, 1) > lag(df$Open, 1) & # Third candle is bullish (white)
      lag(df$Close, 1) > lag(df$Open, 2) & # Third candle closes within the gap
      lag(df$Close, 1) < lag(df$Close, 3)  # Third candle does not fully close the gap
  )
  return(as.integer(conditions))
}

detect_rest_after_battle <- function(df) {
  # Calculate 10-period moving average
  df$ma10 <- zoo::rollmean(df$Close, 10, fill = NA, align = "right")
  
  # Calculate 5-day average high-low range before the pattern
  df$avg_high_low_range_5 <- zoo::rollapply(df$High - df$Low, 5, mean, fill = NA, align = "right")
  
  # Calculate midpoint of the first day's range
  df$midpoint_first <- (lag(df$High, 3) + lag(df$Low, 3)) / 2
  
  # Condition for the first day (strong bullish day)
  first_day_conditions <- (
    lag(df$Close, 3) > lag(df$Open, 3) &  # First day is bullish
      df$midpoint_first > lag(df$ma10, 3) & # Midpoint above 10-period MA (uptrend confirmation)
      (lag(df$High, 3) - lag(df$Low, 3)) > lag(df$avg_high_low_range_5, 3) # First day has a large range
  )
  
  # Condition for the second and third days (resting period)
  second_day_conditions <- (
    (lag(df$High, 2) - lag(df$Low, 2)) < 0.75 * (lag(df$High, 3) - lag(df$Low, 3)) & # 2nd day short range
      abs(lag(df$Close, 2) - lag(df$Open, 2)) < 0.5 * (lag(df$High, 2) - lag(df$Low, 2)) & # 2nd day small real body
      pmax(lag(df$Close, 2), lag(df$Open, 2)) > lag(df$Close, 3) & # 2nd day top above 1st day close
      pmin(lag(df$Close, 2), lag(df$Open, 2)) < lag(df$High, 3) &  # 2nd day low below 1st day high
      lag(df$Close, 2) > df$midpoint_first # 2nd day closes above 1st day midpoint
  )
  
  third_day_conditions <- (
    (lag(df$High, 1) - lag(df$Low, 1)) < 0.75 * (lag(df$High, 3) - lag(df$Low, 3)) & # 3rd day short range
      abs(lag(df$Close, 1) - lag(df$Open, 1)) < 0.5 * (lag(df$High, 1) - lag(df$Low, 1)) & # 3rd day small real body
      lag(df$Low, 1) > df$midpoint_first & # 3rd day low > 1st day midpoint
      lag(df$Open, 1) < lag(df$High, 2) & lag(df$Close, 1) < lag(df$High, 2) & # 3rd day opens & closes below 2nd day's high
      lag(df$Open, 1) > lag(df$Low, 2) & lag(df$Close, 1) > lag(df$Low, 2) # 3rd day opens & closes above 2nd day's low
  )
  
  # Combine all conditions
  conditions <- first_day_conditions & second_day_conditions & third_day_conditions
  
  return(as.integer(conditions))
}


detect_rising_three_methods <- function(df) {
  df$long_candle_first <- abs(lag(df$Close, 4) - lag(df$Open, 4)) > 
    mean(abs(df$Close - df$Open), na.rm = TRUE)  # First day large candle
  
  df$small_candles_mid <- (
    abs(lag(df$Close, 3) - lag(df$Open, 3)) < mean(abs(df$Close - df$Open), na.rm = TRUE) &
      abs(lag(df$Close, 2) - lag(df$Open, 2)) < mean(abs(df$Close - df$Open), na.rm = TRUE) &
      abs(lag(df$Close, 1) - lag(df$Open, 1)) < mean(abs(df$Close - df$Open), na.rm = TRUE) 
  )  
  
  df$inside_first_candle <- (
    pmax(lag(df$High, 3), lag(df$High, 2), lag(df$High, 1)) <= lag(df$High, 4) &
      pmin(lag(df$Low, 3), lag(df$Low, 2), lag(df$Low, 1)) >= lag(df$Low, 4)
  )  
  
  df$final_breakout_candle <- (df$Close > lag(df$Close, 4) & df$Open > lag(df$Close, 4))  
  
  df$Rising_Three_Methods <- ifelse(
    lag(df$Close, 4) > lag(df$Open, 4) &  
      df$long_candle_first & 
      df$small_candles_mid & 
      df$inside_first_candle & 
      df$final_breakout_candle & 
      df$Close > lag(df$Close, 4), 1, 0
  )
  
  return(df$Rising_Three_Methods)
}

detect_falling_three_methods <- function(df) {
  df$long_candle_first <- abs(lag(df$Close, 4) - lag(df$Open, 4)) > 
    mean(abs(df$Close - df$Open), na.rm = TRUE)  
  
  df$small_candles_mid <- (
    abs(lag(df$Close, 3) - lag(df$Open, 3)) < mean(abs(df$Close - df$Open), na.rm = TRUE) &
      abs(lag(df$Close, 2) - lag(df$Open, 2)) < mean(abs(df$Close - df$Open), na.rm = TRUE) &
      abs(lag(df$Close, 1) - lag(df$Open, 1)) < mean(abs(df$Close - df$Open), na.rm = TRUE) 
  )  
  
  df$inside_first_candle <- (
    pmax(lag(df$High, 3), lag(df$High, 2), lag(df$High, 1)) <= lag(df$High, 4) &
      pmin(lag(df$Low, 3), lag(df$Low, 2), lag(df$Low, 1)) >= lag(df$Low, 4)
  )  
  
  df$final_breakout_candle <- (df$Close < lag(df$Close, 4) & df$Open < lag(df$Close, 4))  
  
  df$Falling_Three_Methods <- ifelse(
    lag(df$Close, 4) < lag(df$Open, 4) &  
      df$long_candle_first & 
      df$small_candles_mid & 
      df$inside_first_candle & 
      df$final_breakout_candle & 
      df$Close < lag(df$Close, 4), 1, 0
  )
  
  return(df$Falling_Three_Methods)
}

detect_bullish_mat_hold <- function(df) {
  df$long_white_first <- (lag(df$Close, 4) > lag(df$Open, 4)) &
    ((lag(df$Close, 4) - lag(df$Open, 4)) > 0.5 * (lag(df$High, 4) - lag(df$Low, 4)))
  
  df$gap_up_star_second <- (lag(df$Open, 3) > lag(df$Close, 4)) &  
    (lag(df$Close, 3) < lag(df$Open, 3))
  
  df$small_candles_mid <- (
    abs(lag(df$Close, 2) - lag(df$Open, 2)) < mean(abs(df$Close - df$Open), na.rm = TRUE) &
      abs(lag(df$Close, 1) - lag(df$Open, 1)) < mean(abs(df$Close - df$Open), na.rm = TRUE) 
  )
  
  df$consecutive_decline <- (lag(df$High, 2) < lag(df$High, 3)) & (lag(df$High, 1) < lag(df$High, 2))
  
  df$long_white_fifth <- (df$Close > df$Open) & 
    ((df$Close - df$Open) > 0.5 * (df$High - df$Low)) &
    (df$Close > lag(df$Close, 4))
  
  df$Bullish_Mat_Hold <- ifelse(
    df$long_white_first & 
      df$gap_up_star_second & 
      df$small_candles_mid & 
      df$consecutive_decline & 
      df$long_white_fifth, 1, 0
  )
  
  return(df$Bullish_Mat_Hold)
}


detect_bearish_mat_hold <- function(df) {
  df$long_black_first <- (lag(df$Close, 4) < lag(df$Open, 4)) &  
    ((lag(df$Open, 4) - lag(df$Close, 4)) > 0.5 * (lag(df$High, 4) - lag(df$Low, 4)))
  
  df$gap_down_white_second <- (lag(df$Open, 3) < lag(df$Close, 4)) &  
    (lag(df$Close, 3) > lag(df$Open, 3))
  
  df$small_candles_mid <- (
    abs(lag(df$Close, 2) - lag(df$Open, 2)) < mean(abs(df$Close - df$Open), na.rm = TRUE) &
      abs(lag(df$Close, 1) - lag(df$Open, 1)) < mean(abs(df$Close - df$Open), na.rm = TRUE) 
  )
  
  df$consecutive_increase <- (lag(df$High, 2) > lag(df$High, 3)) & (lag(df$High, 1) > lag(df$High, 2))
  
  df$long_black_fifth <- (df$Close < df$Open) & 
    ((df$Open - df$Close) > 0.5 * (df$High - df$Low)) &
    (df$Close < lag(df$Open, 3))
  
  df$Bearish_Mat_Hold <- ifelse(
    df$long_black_first & 
      df$gap_down_white_second & 
      df$small_candles_mid & 
      df$consecutive_increase & 
      df$long_black_fifth, 1, 0
  )
  
  return(df$Bearish_Mat_Hold)
}


detect_bullish_three_line_strike <- function(df) {
  df$three_white_soldiers <- (lag(df$Close, 3) > lag(df$Open, 3)) & 
    (lag(df$Close, 2) > lag(df$Open, 2)) & 
    (lag(df$Close, 1) > lag(df$Open, 1)) & 
    (lag(df$Close, 2) > lag(df$Close, 3)) & 
    (lag(df$Close, 1) > lag(df$Close, 2))
  
  df$fourth_day_bearish <- (df$Open > lag(df$Close, 1)) &  
    (df$Close < lag(df$Open, 3))
  
  df$Bullish_Three_Line_Strike <- ifelse(
    df$three_white_soldiers & df$fourth_day_bearish, 1, 0
  )
  
  return(df$Bullish_Three_Line_Strike)
}

detect_bearish_three_line_strike <- function(df) {
  df$three_black_crows <- (lag(df$Close, 3) < lag(df$Open, 3)) & 
    (lag(df$Close, 2) < lag(df$Open, 2)) & 
    (lag(df$Close, 1) < lag(df$Open, 1)) & 
    (lag(df$Close, 2) < lag(df$Close, 3)) & 
    (lag(df$Close, 1) < lag(df$Close, 2))
  
  df$fourth_day_bullish <- (df$Open < lag(df$Close, 1)) &  
    (df$Close > lag(df$Open, 3))
  
  df$Bearish_Three_Line_Strike <- ifelse(
    df$three_black_crows & df$fourth_day_bullish, 1, 0
  )
  
  return(df$Bearish_Three_Line_Strike)
}


#Reversal

detect_hammer <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Length of the real body
  df$lower_shadow <- abs(df$Low - pmin(df$Open, df$Close))  # Length of lower shadow
  df$upper_shadow <- abs(df$High - pmax(df$Open, df$Close))  # Length of upper shadow
  
  df$hammer <- ifelse(
    (df$lower_shadow >= 2 * df$real_body) &  # Long lower shadow (at least 2x the real body)
      (df$upper_shadow <= 0.1 * df$real_body) &  # Very small or no upper shadow
      (df$real_body <= 0.5 * (df$High - df$Low)),  # Small real body at upper end
    1, 0
  )
  
  return(df$hammer)
}

detect_belt_hold <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Length of real body
  df$upper_shadow <- abs(df$High - pmax(df$Open, df$Close))  # Upper shadow
  df$lower_shadow <- abs(df$Low - pmin(df$Open, df$Close))  # Lower shadow
  
  # Bullish Belt Hold: Opens at low, no lower shadow, long white candle
  df$bullish_belt_hold <- ifelse(
    (df$Open == df$Low) &  # Opens at the low
      (df$lower_shadow == 0) &  # No lower shadow
      (df$Close > df$Open) &  # White candle
      (df$real_body >= 0.6 * (df$High - df$Low)),  # Strong candle body
    1, 0
  )
  
  # Bearish Belt Hold: Opens at high, no upper shadow, long black candle
  df$bearish_belt_hold <- ifelse(
    (df$Open == df$High) &  # Opens at the high
      (df$upper_shadow == 0) &  # No upper shadow
      (df$Close < df$Open) &  # Black candle
      (df$real_body >= 0.6 * (df$High - df$Low)),  # Strong candle body
    1, 0
  )
  
  return(df[, c("bullish_belt_hold", "bearish_belt_hold")])
}


detect_engulfing <- function(df) {
  df$real_body_prev <- abs(df$Close - df$Open)  # Previous day's real body
  df$real_body_curr <- abs(dplyr::lag(df$Close) - dplyr::lag(df$Open))  # Current real body
  
  # Bullish Engulfing: Downtrend → Small black candle → Large white candle engulfing it
  df$bullish_engulfing <- ifelse(
    (df$Close > df$Open) &  # White candle today
      (dplyr::lag(df$Close) < dplyr::lag(df$Open)) &  # Black candle yesterday
      (df$Open < dplyr::lag(df$Close)) &  # Today's open is below yesterday's close
      (df$Close > dplyr::lag(df$Open)),  # Today's close is above yesterday's open
    1, 0
  )
  
  # Bearish Engulfing: Uptrend → Small white candle → Large black candle engulfing it
  df$bearish_engulfing <- ifelse(
    (df$Close < df$Open) &  # Black candle today
      (dplyr::lag(df$Close) > dplyr::lag(df$Open)) &  # White candle yesterday
      (df$Open > dplyr::lag(df$Close)) &  # Today's open is above yesterday's close
      (df$Close < dplyr::lag(df$Open)),  # Today's close is below yesterday's open
    1, 0
  )
  
  return(df[, c("bullish_engulfing", "bearish_engulfing")])
}


detect_harami <- function(df) {
  df$real_body_prev <- abs(df$Close - df$Open)  # Previous day's real body
  df$real_body_curr <- abs(dplyr::lag(df$Close) - dplyr::lag(df$Open))  # Current real body
  
  # Bullish Harami: Downtrend → Large black candle → Small white candle inside it
  df$bullish_harami <- ifelse(
    (df$Close > df$Open) &  # White candle today
      (dplyr::lag(df$Close) < dplyr::lag(df$Open)) &  # Black candle yesterday
      (df$Open > dplyr::lag(df$Close)) &  # Today's open is inside previous body
      (df$Close < dplyr::lag(df$Open)),  # Today's close is inside previous body
    1, 0
  )
  
  # Bearish Harami: Uptrend → Large white candle → Small black candle inside it
  df$bearish_harami <- ifelse(
    (df$Close < df$Open) &  # Black candle today
      (dplyr::lag(df$Close) > dplyr::lag(df$Open)) &  # White candle yesterday
      (df$Open < dplyr::lag(df$Close)) &  # Today's open is inside previous body
      (df$Close > dplyr::lag(df$Open)),  # Today's close is inside previous body
    1, 0
  )
  
  return(df[, c("bullish_harami", "bearish_harami")])
}


detect_harami_cross <- function(df) {
  df$real_body_prev <- abs(df$Close - df$Open)  # Previous day's real body
  df$real_body_curr <- abs(dplyr::lag(df$Close) - dplyr::lag(df$Open))  # Current real body
  
  # Identify Doji (open ≈ close)
  df$is_doji <- ifelse(abs(df$Close - df$Open) < (0.1 * (df$High - df$Low)), 1, 0)  # Small threshold for Doji
  
  # Harami Cross: Large candle followed by a Doji inside it
  df$harami_cross <- ifelse(
    (df$is_doji == 1) &  # Current candle is a Doji
      (df$Open > dplyr::lag(df$Close)) &  # Doji's open inside previous real body
      (df$Close < dplyr::lag(df$Open)) &  # Doji's close inside previous real body
      (dplyr::lag(df$real_body_curr) > (0.6 * (dplyr::lag(df$High) - dplyr::lag(df$Low)))),  # Previous long candle
    1, 0
  )
  
  return(df[, "harami_cross"])
}

detect_inverted_hammer_shooting_star <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$upper_shadow <- df$High - pmax(df$Open, df$Close)  # Upper shadow size
  df$lower_shadow <- pmin(df$Open, df$Close) - df$Low  # Lower shadow size
  
  # Identify Inverted Hammer (after downtrend)
  df$inverted_hammer <- ifelse(
    (df$upper_shadow > (2 * df$real_body)) &  # Upper shadow > 2x real body
      (df$lower_shadow < (0.1 * df$real_body)) &  # Little to no lower shadow
      (dplyr::lag(df$Close) < dplyr::lag(df$Open)),  # Previous candle in downtrend
    1, 0
  )
  
  # Identify Shooting Star (after uptrend)
  df$shooting_star <- ifelse(
    (df$upper_shadow > (3 * df$real_body)) &  # Upper shadow > 3x real body
      (df$lower_shadow < (0.1 * df$real_body)) &  # Little to no lower shadow
      (dplyr::lag(df$Close) > dplyr::lag(df$Open)),  # Previous candle in uptrend
    1, 0
  )
  
  return(df[, c("inverted_hammer", "shooting_star")])
}

detect_piercing_line <- function(df) {
  df$piercing_line <- ifelse(
    (dplyr::lag(df$Close) < dplyr::lag(df$Open)) &  # Previous day is a long black (bearish) candle
      ((dplyr::lag(df$Open) - dplyr::lag(df$Close)) > (0.7 * (dplyr::lag(df$High) - dplyr::lag(df$Low)))) &  # Long body (not a doji)
      (df$Open < dplyr::lag(df$Low)) &  # Second day opens below the previous day's low
      (df$Close > dplyr::lag(df$Close) + (abs(dplyr::lag(df$Close) - dplyr::lag(df$Open)) / 2)) &  # Closes above midpoint of the previous body
      (df$Close < dplyr::lag(df$Open)),  # Closes within the previous body but below the open
    1, 0
  )
  
  return(df[, "piercing_line"])
}


detect_dark_cloud_cover <- function(df) {
  df$dark_cloud_cover <- ifelse(
    (dplyr::lag(df$Close) > dplyr::lag(df$Open)) &  # Previous day is a long white (bullish) candle
      ((dplyr::lag(df$Close) - dplyr::lag(df$Open)) > (0.7 * (dplyr::lag(df$High) - dplyr::lag(df$Low)))) &  # Long body (not a doji)
      (df$Open > dplyr::lag(df$High)) &  # Second day opens above the previous day's high (gap up)
      (df$Close < (dplyr::lag(df$Open) + dplyr::lag(df$Close)) / 2) &  # Closes below the midpoint of the previous candle
      (df$Close > dplyr::lag(df$Open)),  # Closes within the previous candle's range
    1, 0
  )
  
  return(df[, "dark_cloud_cover"])
}


detect_doji_star <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Calculate real body size
  df$doji <- ifelse(df$real_body < (0.1 * (df$High - df$Low)), 1, 0)  # Doji condition
  
  df$doji_star <- ifelse(
    (abs(dplyr::lag(df$Close) - dplyr::lag(df$Open)) > (0.7 * (dplyr::lag(df$High) - dplyr::lag(df$Low)))) &  # First day is a long candle
      ((df$Open > dplyr::lag(df$High)) | (df$Open < dplyr::lag(df$Low))) &  # Gaps in the direction of trend
      (df$doji == 1),  # Second day is a Doji
    1, 0
  )
  
  return(df[, "doji_star"])
}


detect_meeting_lines <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$prev_real_body <- dplyr::lag(df$real_body)  # Previous day's real body
  
  # Define a long day as one where the body is at least 70% of the day's range
  df$long_day <- ifelse(df$real_body > (0.7 * (df$High - df$Low)), 1, 0)
  df$prev_long_day <- dplyr::lag(df$long_day)
  
  # Identify Meeting Lines Pattern
  df$meeting_lines <- ifelse(
    (df$prev_long_day == 1 & df$long_day == 1) &  # Both are long days
      (((dplyr::lag(df$Close) < dplyr::lag(df$Open)) & (df$Close > df$Open)) |  # Downtrend: First day black, second day white
         ((dplyr::lag(df$Close) > dplyr::lag(df$Open)) & (df$Close < df$Open))) &  # Uptrend: First day white, second day black
      (df$Close == dplyr::lag(df$Close)),  # Both days close at the same price
    1, 0
  )
  
  return(df[, "meeting_lines"])
}

detect_homing_pigeon <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$prev_real_body <- dplyr::lag(df$real_body)  # Previous day's real body
  
  # Identify long black body in a downtrend
  df$long_black_body <- ifelse(
    (df$Close < df$Open) &  # Black candle (Close < Open)
      (df$real_body > (0.7 * (df$High - df$Low))),  # Long body (at least 70% of range)
    1, 0
  )
  
  # Identify short black body completely inside the previous day's body
  df$short_black_body <- ifelse(
    (df$Close < df$Open) &  # Black candle (Close < Open)
      (df$real_body < dplyr::lag(df$real_body)) &  # Smaller than the previous body
      (df$Open > dplyr::lag(df$Close)) &  # Inside the previous body (open below prior open)
      (df$Close < dplyr::lag(df$Open)),  # Inside the previous body (close above prior close)
    1, 0
  )
  
  # Identify Homing Pigeon pattern
  df$homing_pigeon <- ifelse(
    (dplyr::lag(df$long_black_body) == 1) &  # Previous day is a long black body
      (df$short_black_body == 1),  # Current day is a short black body inside previous
    1, 0
  )
  
  return(df[, "homing_pigeon"])
}

detect_descending_hawk <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$prev_real_body <- dplyr::lag(df$real_body)  # Previous day's real body
  
  # Identify first long white body in an uptrend
  df$long_white_body <- ifelse(
    (df$Close > df$Open) &  # White candle (Close > Open)
      (df$real_body > (0.7 * (df$High - df$Low))),  # Long body (at least 70% of range)
    1, 0
  )
  
  # Identify second white body completely inside the first
  df$small_white_body_inside <- ifelse(
    (df$Close > df$Open) &  # White candle (Close > Open)
      (df$real_body < dplyr::lag(df$real_body)) &  # Shorter than the previous body
      (df$Open > dplyr::lag(df$Open)) &  # Inside the first day's body
      (df$Close < dplyr::lag(df$Close)),  # Inside the first day's body
    1, 0
  )
  
  # Identify Descending Hawk pattern
  df$descending_hawk <- ifelse(
    (dplyr::lag(df$long_white_body) == 1) &  # First day is a long white candle
      (df$small_white_body_inside == 1),  # Second day is a white body inside first
    1, 0
  )
  
  return(df[, "descending_hawk"])
}


detect_matching_low <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  
  # Identify first long black candle
  df$long_black_body <- ifelse(
    (df$Close < df$Open) &  # Black candle (Close < Open)
      (df$real_body > (0.7 * (df$High - df$Low))),  # Long body (at least 70% of range)
    1, 0
  )
  
  # Identify second black candle with the same close as the first
  df$matching_black_body <- ifelse(
    (df$Close < df$Open) &  # Black candle (Close < Open)
      (df$Close == dplyr::lag(df$Close)) &  # Close is the same as the previous day
      (dplyr::lag(df$long_black_body) == 1),  # Previous day was a long black body
    1, 0
  )
  
  return(df[, "matching_black_body"])
}


detect_matching_high <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$upper_shadow <- df$High - pmax(df$Open, df$Close)  # Upper shadow size
  
  # Identify first long white candle in an uptrend
  df$long_white_body <- ifelse(
    (df$Close > df$Open) &  # White candle (Close > Open)
      (df$real_body > (0.7 * (df$High - df$Low))) &  # Long body (at least 70% of range)
      (df$upper_shadow < (0.1 * df$real_body)),  # Little or no upper shadow
    1, 0
  )
  
  # Identify second white candle with the same close as the first
  df$matching_high <- ifelse(
    (df$Close > df$Open) &  # White candle (Close > Open)
      (df$Close == dplyr::lag(df$Close)) &  # Close is the same as the previous day
      (df$upper_shadow < (0.1 * df$real_body)) &  # Little or no upper shadow
      (dplyr::lag(df$long_white_body) == 1),  # Previous day was a long white body
    1, 0
  )
  
  return(df[, "matching_high"])
}


detect_kicking <- function(df) {
  # Identify Marubozu candles (full body, no shadows)
  df$marubozu_white <- ifelse(
    (df$Open == df$Low) & (df$Close == df$High),  # No shadows (Marubozu)
    1, 0
  )
  
  df$marubozu_black <- ifelse(
    (df$Open == df$High) & (df$Close == df$Low),  # No shadows (Marubozu)
    1, 0
  )
  
  # Identify Kicking pattern
  df$kicking <- ifelse(
    ((df$marubozu_white == 1 & dplyr::lag(df$marubozu_black) == 1) | 
       (df$marubozu_black == 1 & dplyr::lag(df$marubozu_white) == 1)) &  # One black, one white
      (df$Open > dplyr::lag(df$High) | df$Open < dplyr::lag(df$Low)),  # Gap between candles
    1, 0
  )
  
  return(df[, "kicking"])
}

detect_one_white_soldier <- function(df) {
  df$one_white_soldier <- ifelse(
    (dplyr::lag(df$Close) < dplyr::lag(df$Open)) &  # Previous day is black (bearish)
      (df$Open >= dplyr::lag(df$Close)) &  # Second day opens at or above previous close
      (df$Close > df$Open) &  # Second day is white (bullish)
      (df$Close > dplyr::lag(df$High)),  # Closes above previous high
    1, 0
  )
  
  return(df[, "one_white_soldier"])
}

detect_one_black_crow <- function(df) {
  df$one_black_crow <- ifelse(
    (dplyr::lag(df$Close) > dplyr::lag(df$Open)) &  # Previous day is white (bullish)
      (df$Open <= dplyr::lag(df$Close)) &  # Second day opens at or below previous close
      (df$Close < df$Open) &  # Second day is black (bearish)
      (df$Close < dplyr::lag(df$Low)),  # Closes below previous low
    1, 0
  )
  
  return(df[, "one_black_crow"])
}

detect_morning_evening_star <- function(df) {
  df$morning_star <- ifelse(
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &  # 1st day is bearish (black)
      (df$Open > dplyr::lag(df$Close)) &  # 2nd day gaps up
      (df$Close > df$Open) &  # 3rd day is bullish (white)
      (df$Close > (dplyr::lag(df$Open, 2) + dplyr::lag(df$Close, 2)) / 2),  # Closes above midpoint of 1st day
    1, 0
  )
  
  df$evening_star <- ifelse(
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  # 1st day is bullish (white)
      (df$Open < dplyr::lag(df$Close)) &  # 2nd day gaps down
      (df$Close < df$Open) &  # 3rd day is bearish (black)
      (df$Close < (dplyr::lag(df$Open, 2) + dplyr::lag(df$Close, 2)) / 2),  # Closes below midpoint of 1st day
    1, 0
  )
  
  return(df[, c("morning_star", "evening_star")])
}


detect_morning_evening_doji_star <- function(df) {
  df$morning_doji_star <- ifelse(
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &  # 1st day is bearish (black)
      (df$Open > dplyr::lag(df$Close)) &  # 2nd day gaps up
      (abs(df$Open - df$Close) < (df$High - df$Low) * 0.1) &  # 2nd day is a Doji (small body)
      (df$Close > df$Open) &  # 3rd day is bullish (white)
      (df$Close > (dplyr::lag(df$Open, 2) + dplyr::lag(df$Close, 2)) / 2),  # Closes above midpoint of 1st day
    1, 0
  )
  
  df$evening_doji_star <- ifelse(
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  # 1st day is bullish (white)
      (df$Open < dplyr::lag(df$Close)) &  # 2nd day gaps down
      (abs(df$Open - df$Close) < (df$High - df$Low) * 0.1) &  # 2nd day is a Doji (small body)
      (df$Close < df$Open) &  # 3rd day is bearish (black)
      (df$Close < (dplyr::lag(df$Open, 2) + dplyr::lag(df$Close, 2)) / 2),  # Closes below midpoint of 1st day
    1, 0
  )
  
  return(df[, c("morning_doji_star", "evening_doji_star")])
}

detect_abandoned_baby <- function(df) {
  df$abandoned_baby <- ifelse(
    # First day follows the prior trend
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2) | dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &
      
      # Second day is a Doji that gaps above or below the first day’s shadows
      (abs(dplyr::lag(df$Open, 1) - dplyr::lag(df$Close, 1)) < (dplyr::lag(df$High, 1) - dplyr::lag(df$Low, 1)) * 0.1) & 
      ((dplyr::lag(df$Low, 1) > dplyr::lag(df$High, 2)) | (dplyr::lag(df$High, 1) < dplyr::lag(df$Low, 2))) &
      
      # Third day is opposite color of the first day
      ((df$Close > df$Open & dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) | 
         (df$Close < df$Open & dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2))) &
      
      # Third day gaps in the opposite direction with no overlapping shadows
      ((df$Low > dplyr::lag(df$High, 1)) | (df$High < dplyr::lag(df$Low, 1))),
    
    1, 0
  )
  
  return(df[, c("abandoned_baby")])
}


detect_tri_star <- function(df) {
  df$tri_star <- ifelse(
    # All three days are Doji (small body)
    (abs(dplyr::lag(df$Open, 2) - dplyr::lag(df$Close, 2)) < (dplyr::lag(df$High, 2) - dplyr::lag(df$Low, 2)) * 0.1) &
      (abs(dplyr::lag(df$Open, 1) - dplyr::lag(df$Close, 1)) < (dplyr::lag(df$High, 1) - dplyr::lag(df$Low, 1)) * 0.1) &
      (abs(df$Open - df$Close) < (df$High - df$Low) * 0.1) &
      
      # The second Doji gaps above or below the first and third Doji
      ((dplyr::lag(df$Low, 1) > dplyr::lag(df$High, 2) & dplyr::lag(df$Low, 1) > df$High) | 
         (dplyr::lag(df$High, 1) < dplyr::lag(df$Low, 2) & dplyr::lag(df$High, 1) < df$Low)),
    
    1, 0
  )
  
  return(df[, c("tri_star")])
}


detect_upside_gap_two_crows <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$prev_real_body <- dplyr::lag(df$real_body)  # Previous day's real body size
  df$prev2_real_body <- dplyr::lag(df$real_body, 2)  # Two days before
  
  df$upside_gap_two_crows <- ifelse(
    # First day is a long white day (close > open) in an uptrend
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$real_body, 2) > median(df$real_body, na.rm = TRUE)) &
      
      # Second day is a black day (close < open) with an upward gap
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) & 
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$High, 2)) & 
      
      # Third day is a black day (close < open) opening above first black day
      # but closing below its body (engulfing it)
      (df$Close < df$Open) & 
      (df$Open > dplyr::lag(df$Open, 1)) & 
      (df$Close < dplyr::lag(df$Close, 1)) & 
      (df$Close > dplyr::lag(df$Close, 2)), 
    
    1, 0
  )
  
  return(df[, c("upside_gap_two_crows")])
}

detect_upside_gap_two_crows <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$prev_real_body <- dplyr::lag(df$real_body)  # Previous day's real body size
  df$prev2_real_body <- dplyr::lag(df$real_body, 2)  # Two days before
  
  df$upside_gap_two_crows <- ifelse(
    # First day is a long white day (close > open) in an uptrend
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$real_body, 2) > median(df$real_body, na.rm = TRUE)) &
      
      # Second day is a black day (close < open) with an upward gap
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) & 
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$High, 2)) & 
      
      # Third day is a black day (close < open) opening above first black day
      # but closing below its body (engulfing it)
      (df$Close < df$Open) & 
      (df$Open > dplyr::lag(df$Open, 1)) & 
      (df$Close < dplyr::lag(df$Close, 1)) & 
      (df$Close > dplyr::lag(df$Close, 2)), 
    
    1, 0
  )
  
  return(df[, c("upside_gap_two_crows")])
}


detect_unique_three_river_bottom <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$lower_shadow <- df$Low - pmin(df$Open, df$Close)  # Lower shadow size
  
  df$unique_three_river_bottom <- ifelse(
    # First day is a long black day (close < open)
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$real_body, 2) > median(df$real_body, na.rm = TRUE)) & 
      
      # Second day is a Harami (small black body inside first day's body)
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) & 
      (dplyr::lag(df$real_body, 1) < dplyr::lag(df$real_body, 2)) & 
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2)) & 
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 2)) & 
      
      # Second day sets a new low
      (dplyr::lag(df$Low, 1) < dplyr::lag(df$Low, 2)) & 
      
      # Third day is a short white day below the second day's close
      (df$Close > df$Open) & 
      (df$real_body < median(df$real_body, na.rm = TRUE)) & 
      (df$Close < dplyr::lag(df$Close, 1)), 
    
    1, 0
  )
  
  return(df[, c("unique_three_river_bottom")])
}

detect_unique_three_mountain_top <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$upper_shadow <- df$High - pmax(df$Open, df$Close)  # Upper shadow size
  
  df$unique_three_mountain_top <- ifelse(
    # First day: Long white candle in an uptrend
    (dplyr::lag(df$Close, 3) > dplyr::lag(df$Open, 3)) & 
      (dplyr::lag(df$real_body, 3) > median(df$real_body, na.rm = TRUE)) & 
      
      # Second day: Small white candle with a long upper shadow
      (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$real_body, 2) < dplyr::lag(df$real_body, 3)) & 
      (dplyr::lag(df$upper_shadow, 2) > 2 * dplyr::lag(df$real_body, 2)) & 
      (dplyr::lag(df$Open, 2) < dplyr::lag(df$Close, 3)) & 
      
      # Third day: Opens higher but not above the second day’s high
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2)) & 
      (dplyr::lag(df$High, 1) < dplyr::lag(df$High, 2)) & 
      
      # Third day: Small black candle, closing above the second day’s close
      (df$Close < df$Open) & 
      (df$real_body < median(df$real_body, na.rm = TRUE)) & 
      (df$Close > dplyr::lag(df$Close, 2)), 
    
    1, 0
  )
  
  return(df[, c("unique_three_mountain_top")])
}


detect_three_white_soldiers <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  df$prev_real_body <- dplyr::lag(df$real_body, 1)
  
  df$three_white_soldiers <- ifelse(
    # Three consecutive long white candles
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &
      (df$Close > df$Open) &
      
      # Each close is higher than the previous close
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Close, 2)) &
      (df$Close > dplyr::lag(df$Close, 1)) &
      
      # Each opens within the previous body
      (dplyr::lag(df$Open, 1) >= dplyr::lag(df$Open, 2)) &
      (dplyr::lag(df$Open, 1) <= dplyr::lag(df$Close, 2)) &
      (df$Open >= dplyr::lag(df$Open, 1)) &
      (df$Open <= dplyr::lag(df$Close, 1)) &
      
      # Each closes at or near the high of the day
      (dplyr::lag(df$Close, 2) >= dplyr::lag(df$High, 2) * 0.9) &
      (dplyr::lag(df$Close, 1) >= dplyr::lag(df$High, 1) * 0.9) &
      (df$Close >= df$High * 0.9), 
    
    1, 0
  )
  
  return(df[, c("three_white_soldiers")])
}


detect_three_black_crows <- function(df) {
  df$real_body <- abs(df$Close - df$Open)  # Real body size
  
  df$three_black_crows <- ifelse(
    # Three consecutive long black candles
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &
      (df$Close < df$Open) &
      
      # Each close is lower than the previous close (new lows)
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Close, 2)) &
      (df$Close < dplyr::lag(df$Close, 1)) &
      
      # Each opens within the previous body
      (dplyr::lag(df$Open, 1) >= dplyr::lag(df$Close, 2)) &
      (dplyr::lag(df$Open, 1) <= dplyr::lag(df$Open, 2)) &
      (df$Open >= dplyr::lag(df$Close, 1)) &
      (df$Open <= dplyr::lag(df$Open, 1)) &
      
      # Each closes near the low (90% of the daily low)
      (dplyr::lag(df$Close, 2) <= dplyr::lag(df$Low, 2) * 1.1) &
      (dplyr::lag(df$Close, 1) <= dplyr::lag(df$Low, 1) * 1.1) &
      (df$Close <= df$Low * 1.1), 
    
    1, 0
  )
  
  return(df[, c("three_black_crows")])
}


detect_identical_three_crows <- function(df) {
  df$identical_three_crows <- ifelse(
    # Three consecutive long black candles
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &
      (df$Close < df$Open) &
      
      # Each day closes lower than the previous day (stair-step down)
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Close, 2)) &
      (df$Close < dplyr::lag(df$Close, 1)) &
      
      # Each day starts at the previous day’s close
      (dplyr::lag(df$Open, 1) == dplyr::lag(df$Close, 2)) &
      (df$Open == dplyr::lag(df$Close, 1)), 
    
    1, 0
  )
  
  return(df[, c("identical_three_crows")])
}


detect_advance_block <- function(df) {
  df$advance_block <- ifelse(
    # Three consecutive white (bullish) candles with higher closes
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &
      (df$Close > df$Open) &
      
      # Each day closes higher than the previous day
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Close, 2)) &
      (df$Close > dplyr::lag(df$Close, 1)) &
      
      # Each day opens within the previous day's body
      (dplyr::lag(df$Open, 1) >= dplyr::lag(df$Low, 2)) & (dplyr::lag(df$Open, 1) <= dplyr::lag(df$High, 2)) &
      (df$Open >= dplyr::lag(df$Low, 1)) & (df$Open <= dplyr::lag(df$High, 1)) &
      
      # Long upper shadows on second and third candles (showing weakening strength)
      ((dplyr::lag(df$High, 1) - dplyr::lag(df$Close, 1)) > (dplyr::lag(df$Close, 1) - dplyr::lag(df$Open, 1))) &
      ((df$High - df$Close) > (df$Close - df$Open)),
    
    1, 0
  )
  
  return(df[, c("advance_block")])
}


detect_descent_block <- function(df) {
  df$descent_block <- ifelse(
    # First candle: Long black candle in a downtrend
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$Close, 2) < dplyr::lag(df$Close, 3)) &  # Continuation of downtrend
      
      # Second and third candles: Both black with lower closes
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) & 
      (df$Close < df$Open) & 
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Close, 2)) & 
      (df$Close < dplyr::lag(df$Close, 1)) & 
      
      # Last two days have long lower shadows (showing uncertainty)
      ((dplyr::lag(df$Low, 1) < dplyr::lag(df$Close, 1) - (dplyr::lag(df$Open, 1) - dplyr::lag(df$Close, 1)))) & 
      ((df$Low < df$Close - (df$Open - df$Close))), 
    
    1, 0
  )
  
  return(df[, c("descent_block")])
}

detect_deliberation <- function(df) {
  df$bearish_deliberation <- ifelse(
    # First two candles: Long white (bullish) candles
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &
      
      # Third candle: A Spinning Top or Star, opening near previous close
      (df$Open >= dplyr::lag(df$Close, 1) * 0.98) & (df$Open <= dplyr::lag(df$Close, 1) * 1.02) & 
      (abs(df$Open - df$Close) < (df$High - df$Low) * 0.3),  # Spinning Top condition
    
    1, 0
  )
  
  df$bullish_deliberation <- ifelse(
    # First two candles: Long black (bearish) candles
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) & 
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &
      
      # Third candle: A small black candle or Star that may gap
      (df$Close < df$Open) & 
      ((df$Open > dplyr::lag(df$Close, 1)) |  # Gaps away
         (abs(df$Open - df$Close) < (df$High - df$Low) * 0.3)),  # Spinning Top condition
    
    1, 0
  )
  
  return(df[, c("bearish_deliberation", "bullish_deliberation")])
}


detect_two_crows <- function(df) {
  df$two_crows <- ifelse(
    # First candle: Long white (bullish) candle
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) & 
      ((dplyr::lag(df$Close, 2) - dplyr::lag(df$Open, 2)) > (dplyr::lag(df$High, 2) - dplyr::lag(df$Low, 2)) * 0.6) &
      
      # Second candle: Gaps up and is a black (bearish) candle
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2)) & 
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &
      
      # Third candle: Black candle that opens inside the second candle's body and closes inside the first candle's body
      (df$Open < dplyr::lag(df$Open, 1)) & (df$Open > dplyr::lag(df$Close, 1)) &
      (df$Close < dplyr::lag(df$Close, 2)) & (df$Close > dplyr::lag(df$Open, 2)),
    
    1, 0
  )
  
  return(df[, c("two_crows")])
}


detect_two_rabbits <- function(df) {
  df$two_rabbits <- ifelse(
    # First candle: Black (bearish) candle in a downtrend
    (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &
      
      # Second candle: Downward gapping white (bullish) candle
      (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2)) &
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &
      
      # Third candle: Another white candle
      (df$Open > dplyr::lag(df$Open, 1)) & (df$Open < dplyr::lag(df$Close, 1)) &
      (df$Close > dplyr::lag(df$Open, 2)) & (df$Close < dplyr::lag(df$Close, 2)),
    
    1, 0
  )
  
  return(df[, c("two_rabbits")])
}

detect_three_inside <- function(df) {
  df$three_inside_up <- ifelse(
    # Identify a Harami pattern first
    (dplyr::lag(df$Open, 2) > dplyr::lag(df$Close, 2)) &  # First candle: Black (bearish)
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &  # Second candle: White (bullish)
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2)) &  # Harami: 2nd candle inside 1st
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 2)) &
      
      # Third candle is bullish with a higher close
      (df$Close > dplyr::lag(df$Close, 1)), 
    
    1, 0
  )
  
  df$three_inside_down <- ifelse(
    # Identify a Harami pattern first
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  # First candle: White (bullish)
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 1)) &  # Second candle: Black (bearish)
      (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2)) &  # Harami: 2nd candle inside 1st
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 2)) &
      
      # Third candle is bearish with a lower close
      (df$Close < dplyr::lag(df$Close, 1)), 
    
    1, 0
  )
  
  return(df[, c("three_inside_up", "three_inside_down")])
}


detect_three_outside <- function(df) {
  df$three_outside_up <- ifelse(
    # Identify a Bullish Engulfing pattern first
    (dplyr::lag(df$Open, 2) > dplyr::lag(df$Close, 2)) &  # First candle: Black (bearish)
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &  # Second candle: White (bullish)
      (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 2)) &  # Engulfs the first candle
      (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2)) &
      
      # Third candle confirms the reversal with a higher close
      (df$Close > dplyr::lag(df$Close, 1)), 
    
    1, 0
  )
  
  df$three_outside_down <- ifelse(
    # Identify a Bearish Engulfing pattern first
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  # First candle: White (bullish)
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 1)) &  # Second candle: Black (bearish)
      (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2)) &  # Engulfs the first candle
      (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 2)) &
      
      # Third candle confirms the reversal with a lower close
      (df$Close < dplyr::lag(df$Close, 1)), 
    
    1, 0
  )
  
  return(df[, c("three_outside_up", "three_outside_down")])
}

detect_three_stars_south <- function(df) {
  df$three_stars_south <- ifelse(
    # First Candle: Long Black with a long lower shadow (Hammer-like)
    (dplyr::lag(df$Open, 2) > dplyr::lag(df$Close, 2)) &  # First candle is black
      ((dplyr::lag(df$Open, 2) - dplyr::lag(df$Close, 2)) < 
         (dplyr::lag(df$Low, 2) - dplyr::lag(df$Close, 2)) * 2),  # Long lower shadow
    
    # Second Candle: Smaller Black Candle with a higher low
    (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 1)) &  # Black body
      (dplyr::lag(df$Low, 1) > dplyr::lag(df$Low, 2)) &  # Higher low
      ((dplyr::lag(df$High, 1) - dplyr::lag(df$Low, 1)) < 
         (dplyr::lag(df$High, 2) - dplyr::lag(df$Low, 2))),  # Smaller range than first
    
    # Third Candle: Small Black Marubozu inside previous range
    (df$Open > df$Close) &  # Black body
      (df$Open < dplyr::lag(df$High, 1)) &  # Opens inside second day's range
      (df$Close > dplyr::lag(df$Low, 1)),  # Closes inside second day's range
    
    1, 0
  )
  
  return(df[, "three_stars_south"])
}


detect_three_stars_north <- function(df) {
  df$three_stars_north <- ifelse(
    # First Candle: Long White Day
    (dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  # White body
      ((dplyr::lag(df$Close, 2) - dplyr::lag(df$Open, 2)) > (dplyr::lag(df$High, 2) - dplyr::lag(df$Low, 2)) * 0.5) &  # Large body
      ((dplyr::lag(df$High, 2) - dplyr::lag(df$Close, 2)) > (dplyr::lag(df$High, 2) - dplyr::lag(df$Low, 2)) * 0.4) &  # Upper shadow > 40%
      ((dplyr::lag(df$Open, 2) - dplyr::lag(df$Low, 2)) < (dplyr::lag(df$High, 2) - dplyr::lag(df$Low, 2)) * 0.075),  # Lower shadow < 7.5%
    
    # Second Candle: White day with a lower high & higher low
    (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &  # White body
      (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2)) &  # Opens below 1st day's close
      (dplyr::lag(df$High, 1) < dplyr::lag(df$High, 2)) &  # Lower high
      (dplyr::lag(df$Low, 1) > dplyr::lag(df$Low, 2)) &  # Higher low
      ((dplyr::lag(df$High, 1) - dplyr::lag(df$Close, 1)) > (dplyr::lag(df$High, 1) - dplyr::lag(df$Low, 1)) * 0.4) &  # Upper shadow > 40%
      ((dplyr::lag(df$Open, 1) - dplyr::lag(df$Low, 1)) < (dplyr::lag(df$High, 1) - dplyr::lag(df$Low, 1)) * 0.075),  # Lower shadow < 7.5%
    
    # Third Candle: Small White Marubozu inside 2nd candle’s range
    (df$Close > df$Open) &  # White body
      (df$Open < dplyr::lag(df$High, 1)) & (df$Close > dplyr::lag(df$Low, 1)) &  # Inside 2nd day's range
      ((df$High - df$Low) < (dplyr::lag(df$High, 1) - dplyr::lag(df$Low, 1)) * 0.6) &  # Small range
      ((df$High - df$Close) < (df$High - df$Low) * 0.1) &  # Very small upper shadow
      ((df$Open - df$Low) < (df$High - df$Low) * 0.1),  # Very small lower shadow
    
    1, 0
  )
  
  return(df[, "three_stars_north"])
}

detect_stick_sandwich <- function(df) {
  df$stick_sandwich <- ifelse(
    # Bullish Stick Sandwich
    ((dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &  # First day: Black candle (downtrend)
       (dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &  # Second day: White candle
       (dplyr::lag(df$Close, 1) > dplyr::lag(df$Close, 2)) &  # Second day closes above first day
       (df$Close < df$Open) &  # Third day: Black candle
       (df$Close == dplyr::lag(df$Close, 2))) |  # Third day closes at first day's close
      
      # Bearish Stick Sandwich
      ((dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  # First day: White candle (uptrend)
         (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &  # Second day: Black candle
         (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2)) &  # Second day opens below first day's close
         (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 2)) &  # Second day closes below first day's open
         (df$Close > df$Open) &  # Third day: White candle
         (df$Open < dplyr::lag(df$Close, 1)) & (df$Close > dplyr::lag(df$Open, 1))),  # Third day engulfs second day's real body
    
    1, 0
  )
  
  return(df[, "stick_sandwich"])
}

detect_squeeze_alert <- function(df) {
  df$squeeze_alert <- ifelse(
    
    # Bullish Squeeze Alert (Downtrend Reversal)
    ((dplyr::lag(df$Close, 3) < dplyr::lag(df$Open, 3)) &  # First day: Long candle in a downtrend
       (dplyr::lag(df$High, 2) < dplyr::lag(df$High, 3)) & (dplyr::lag(df$Low, 2) > dplyr::lag(df$Low, 3)) &  # Second day: Lower high, higher low
       (dplyr::lag(df$High, 1) < dplyr::lag(df$High, 2)) & (dplyr::lag(df$Low, 1) > dplyr::lag(df$Low, 2))) |  # Third day: Lower high, higher low
      
      # Bearish Squeeze Alert (Uptrend Reversal)
      ((dplyr::lag(df$Close, 3) > dplyr::lag(df$Open, 3)) &  # First day: Long candle in an uptrend
         (dplyr::lag(df$High, 2) < dplyr::lag(df$High, 3)) & (dplyr::lag(df$Low, 2) > dplyr::lag(df$Low, 3)) &  # Second day: Lower high, higher low
         (dplyr::lag(df$High, 1) < dplyr::lag(df$High, 2)) & (dplyr::lag(df$Low, 1) > dplyr::lag(df$Low, 2))),  # Third day: Lower high, higher low
    
    1, 0
  )
  
  return(df[, "squeeze_alert"])
}

#4 or more
detect_breakaway <- function(df) {
  # Create result vector filled with zeros
  result <- rep(0, nrow(df))
  
  # Skip first 5 rows since we need at least 5 previous days
  for (i in 6:nrow(df)) {
    # Bullish Breakaway
    if ((df$Close[i-5] < df$Open[i-5]) &  # Day 1: Long black candle (downtrend)
        (df$Close[i-4] < df$Open[i-4]) & (df$Open[i-4] < df$Low[i-5]) &  # Day 2: Black, gaps down
        (df$Close[i-3] > df$Close[i-4]) &  # Day 3: Close higher
        (df$Close[i-2] > df$Close[i-3]) &  # Day 4: Close higher
        (df$Close[i-1] > df$Open[i-1]) & (df$Close[i-1] > df$Open[i-4])) {  # Day 5: White, closes in the gap
      result[i] <- 1
    }
    # Bearish Breakaway
    else if ((df$Close[i-5] > df$Open[i-5]) &  # Day 1: Long white candle (uptrend)
             (df$Close[i-4] > df$Open[i-4]) & (df$Open[i-4] > df$High[i-5]) &  # Day 2: White, gaps up
             (df$Close[i-3] < df$Close[i-4]) &  # Day 3: Close lower
             (df$Close[i-2] < df$Close[i-3]) &  # Day 4: Close lower
             (df$Close[i-1] < df$Open[i-1]) & (df$Close[i-1] < df$Open[i-4])) {  # Day 5: Black, closes in the gap
      result[i] <- 1
    }
  }
  
  return(result)
}


detect_concealing_baby_swallow <- function(df) {
  df$concealing_baby_swallow <- ifelse(
    
    # 1st & 2nd Day: Black Marubozu (Strong Bearish Candles)
    ((dplyr::lag(df$Close, 4) < dplyr::lag(df$Open, 4)) & 
       (dplyr::lag(df$High, 4) == dplyr::lag(df$Open, 4)) & 
       (dplyr::lag(df$Low, 4) == dplyr::lag(df$Close, 4)) &  # 1st Day: Black Marubozu
       
       (dplyr::lag(df$Close, 3) < dplyr::lag(df$Open, 3)) & 
       (dplyr::lag(df$High, 3) == dplyr::lag(df$Open, 3)) & 
       (dplyr::lag(df$Low, 3) == dplyr::lag(df$Close, 3)) &  # 2nd Day: Black Marubozu
       
       # 3rd Day: Black candle, gaps down, trades into Day 2 with long upper shadow
       (dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) & 
       (dplyr::lag(df$Open, 2) < dplyr::lag(df$Low, 3)) & 
       (dplyr::lag(df$High, 2) > dplyr::lag(df$Close, 3)) &  # Trades into Day 2
       
       # 4th Day: Black candle, engulfs 3rd day entirely (body & shadow)
       (dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) & 
       (dplyr::lag(df$Open, 1) > dplyr::lag(df$High, 2)) & 
       (dplyr::lag(df$Close, 1) < dplyr::lag(df$Low, 2))),  # Engulfs Day 3 entirely
    
    1, 0
  )
  
  return(df[, "concealing_baby_swallow"])
}

detect_ladder_bottom <- function(df) {
  # Create result vector filled with zeros
  result <- rep(0, nrow(df))
  
  # Skip first 5 rows since we need at least 5 previous days
  for (i in 6:nrow(df)) {
    # First 3 Days: Long Black Candles (Three Black Crows-like)
    if ((df$Close[i-5] < df$Open[i-5]) &  # 1st Black Candle
        (df$Close[i-4] < df$Open[i-4]) &  # 2nd Black Candle
        (df$Close[i-3] < df$Open[i-3]) &  # 3rd Black Candle
        
        (df$Open[i-4] < df$Open[i-5]) &  # Consecutive Lower Opens
        (df$Close[i-4] < df$Close[i-5]) & 
        (df$Open[i-3] < df$Open[i-4]) &  
        (df$Close[i-3] < df$Close[i-4]) &  # Lower Closes
        
        # Fourth Day: Black Candle with Upper Shadow
        (df$Close[i-2] < df$Open[i-2]) &  
        (df$High[i-2] > df$Open[i-2]) &  # Upper Shadow Present
        
        # Fifth Day: White Candle with Open Above Previous Black Body
        (df$Close[i-1] > df$Open[i-1]) &  # White Candle
        (df$Open[i-1] > df$Close[i-2])) {  # Opens Above Day 4 Body
      
      result[i] <- 1
    }
  }
  
  return(result)
}

detect_ladder_top <- function(df) {
  # Create result vector filled with zeros
  result <- rep(0, nrow(df))
  
  # Skip first 5 rows since we need at least 5 previous days
  for (i in 6:nrow(df)) {
    # First 3 Days: Long White Candles (Uptrend)
    if ((df$Close[i-5] > df$Open[i-5]) &  # 1st White Candle
        (df$Close[i-4] > df$Open[i-4]) &  # 2nd White Candle
        (df$Close[i-3] > df$Open[i-3]) &  # 3rd White Candle
        
        (df$Open[i-4] > df$Open[i-5]) &  # Consecutive Higher Opens
        (df$Close[i-4] > df$Close[i-5]) & 
        (df$Open[i-3] > df$Open[i-4]) &  
        (df$Close[i-3] > df$Close[i-4]) &  # Higher Closes
        
        # Fourth Day: White Candle with a Long Lower Shadow
        (df$Close[i-2] > df$Open[i-2]) &  
        (df$Low[i-2] < df$Low[i-3]) &  # Lower Shadow Extends Down
        
        # Fifth Day: Black Candle with Open Below Previous White Body and Closes Below Low
        (df$Close[i-1] < df$Open[i-1]) &  # Black Candle
        (df$Open[i-1] < df$Close[i-2]) &  # Opens Below Day 4 Body
        (df$Close[i-1] < df$Low[i-2])) {  # Closes Below Day 4 Low
      
      result[i] <- 1
    }
  }
  
  return(result)
}


detect_after_bottom_gap_up <- function(df) {
  df$after_bottom_gap_up <- ifelse(
    
    # Day 1: Long Black Candle in a Downtrend
    ((dplyr::lag(df$Close, 5) < dplyr::lag(df$Open, 5))) &  
      
      # Days 2 & 3: Black Candles Closing Lower than Previous Days
      ((dplyr::lag(df$Close, 4) < dplyr::lag(df$Close, 5)) &  
         (dplyr::lag(df$Close, 3) < dplyr::lag(df$Close, 4)) &  
         (dplyr::lag(df$Close, 3) < dplyr::lag(df$Open, 3))) & 
      
      # Day 3: Gaps Down (Opens Below Day 2's Close)
      (dplyr::lag(df$Open, 3) < dplyr::lag(df$Close, 4)) &  
      
      # Day 4: Long White Candle
      ((dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2))) &  
      
      # Day 5: Long White Candle with a Gap Up
      ((dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &  
         (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2))),  
    
    1, 0  # If pattern matches, mark as 1; else, 0
  )
  
  return(df[, "after_bottom_gap_up"])
}


detect_after_top_gap_down <- function(df) {
  df$after_top_gap_down <- ifelse(
    
    # Day 1: Long White Candle in an Uptrend
    ((dplyr::lag(df$Close, 5) > dplyr::lag(df$Open, 5))) &  
      
      # Days 2 & 3: White Candles Closing Higher than Previous Days
      ((dplyr::lag(df$Close, 4) > dplyr::lag(df$Close, 5)) &  
         (dplyr::lag(df$Close, 3) > dplyr::lag(df$Close, 4)) &  
         (dplyr::lag(df$Close, 3) > dplyr::lag(df$Open, 3))) & 
      
      # Day 3: Gaps Up (Opens Above Day 2's Close)
      (dplyr::lag(df$Open, 3) > dplyr::lag(df$Close, 4)) &  
      
      # Day 4: Long Black Candle
      ((dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2))) &  
      
      # Day 5: Long Black Candle with a Gap Down
      ((dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &  
         (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2))),  
    
    1, 0  # If pattern matches, mark as 1; else, 0
  )
  
  return(df[, "after_top_gap_down"])
}


detect_three_gap_downs <- function(df) {
  df$three_gap_downs <- ifelse(
    
    # Day 1: Any color (acts as the reference)
    TRUE &  
      
      # Day 2: Any color but must gap down
      (dplyr::lag(df$Open, 3) < dplyr::lag(df$Close, 4)) &  
      (dplyr::lag(df$Close, 3) < dplyr::lag(df$Open, 4)) &  
      
      # Day 3: Long Black Candle with a Gap Down
      ((dplyr::lag(df$Close, 2) < dplyr::lag(df$Open, 2)) &  
         (dplyr::lag(df$Open, 2) < dplyr::lag(df$Close, 3))) &  
      
      # Day 4: Another Long Black Candle with a Gap Down
      ((dplyr::lag(df$Close, 1) < dplyr::lag(df$Open, 1)) &  
         (dplyr::lag(df$Open, 1) < dplyr::lag(df$Close, 2))),  
    
    1, 0  # If pattern matches, mark as 1; else, 0
  )
  
  return(df[, "three_gap_downs"])
}


detect_three_gap_ups <- function(df) {
  df$three_gap_ups <- ifelse(
    
    # Day 1: Any color (acts as the reference)
    TRUE &  
      
      # Day 2: Any color but must gap up
      (dplyr::lag(df$Open, 3) > dplyr::lag(df$Close, 4)) &  
      (dplyr::lag(df$Close, 3) > dplyr::lag(df$Open, 4)) &  
      
      # Day 3: Long White Candle with a Gap Up
      ((dplyr::lag(df$Close, 2) > dplyr::lag(df$Open, 2)) &  
         (dplyr::lag(df$Open, 2) > dplyr::lag(df$Close, 3))) &  
      
      # Day 4: Another Long White Candle with a Gap Up
      ((dplyr::lag(df$Close, 1) > dplyr::lag(df$Open, 1)) &  
         (dplyr::lag(df$Open, 1) > dplyr::lag(df$Close, 2))),  
    
    1, 0  # If pattern matches, mark as 1; else, 0
  )
  
  return(df[, "three_gap_ups"])
}



################

#Reversal Pattern

# One-Day Patterns
df$Hammer <- detect_hammer(df)
df$Belt_Hold <- detect_belt_hold(df)

# Two-Day Patterns
df$Engulfing <- detect_engulfing(df)
df$Harami <- detect_harami(df)
df$Harami_Cross <- detect_harami_cross(df)
patterns <- detect_inverted_hammer_shooting_star(df)
df$Inverted_Hammer <- patterns$inverted_hammer
df$Shooting_Star <- patterns$shooting_star
df$Piercing_Line <- detect_piercing_line(df)
df$Dark_Cloud_Cover <- detect_dark_cloud_cover(df)
df$Doji_Star <- detect_doji_star(df)
df$Meeting_Lines <- detect_meeting_lines(df)
df$Homing_Pigeon <- detect_homing_pigeon(df)
df$Descending_Hawk <- detect_descending_hawk(df)
df$Matching_Low <- detect_matching_low(df)
df$Matching_High <- detect_matching_high(df)
df$Kicking <- detect_kicking(df)
df$One_White_Soldier <- detect_one_white_soldier(df)
df$One_Black_Crow <- detect_one_black_crow(df)

# Three-Day Patterns
patterns <- detect_morning_evening_star(df)
df$Morning_Star <- patterns$morning_star
df$Evening_Star <- patterns$evening_star
patterns <- detect_morning_evening_doji_star(df)
df$Morning_Doji_Star <- patterns$morning_doji_star
df$Evening_Doji_Star <- patterns$evening_doji_star
df$Abandoned_Baby <- detect_abandoned_baby(df)
df$Tri_Star <- detect_tri_star(df)
df$Upside_Gap_Two_Crows <- detect_upside_gap_two_crows(df)
df$Unique_Three_River_Bottom <- detect_unique_three_river_bottom(df)
df$Unique_Three_Mountain_Top <- detect_unique_three_mountain_top(df)
df$Three_White_Soldiers <- detect_three_white_soldiers(df)
df$Three_Black_Crows <- detect_three_black_crows(df)
df$Identical_Three_Crows <- detect_identical_three_crows(df)
df$Advance_Block <- detect_advance_block(df)
df$Descent_Block <- detect_descent_block(df)
df$Deliberation <- detect_deliberation(df)
df$Two_Crows <- detect_two_crows(df)
df$Two_Rabbits <- detect_two_rabbits(df)
patterns <- detect_three_inside(df)
df$Three_Inside_Up <- patterns$three_inside_up
df$Three_Inside_Down <- patterns$three_inside_down
patterns <- detect_three_outside(df)
df$Three_Outside_Up <- patterns$three_outside_up
df$Three_Outside_Down <- patterns$three_outside_down
df$Three_Stars_North <- detect_three_stars_north(df)
df$Stick_Sandwich <- detect_stick_sandwich(df)
df$Squeeze_Alert <- detect_squeeze_alert(df)

# Four-or-More-Day Patterns
df$Breakaway <- detect_breakaway(df)
df$Concealing_Baby_Swallow <- detect_concealing_baby_swallow(df)
df$Ladder_Bottom <- detect_ladder_bottom(df)
df$Ladder_Top <- detect_ladder_top(df)
df$After_Bottom_Gap_Up <- detect_after_bottom_gap_up(df)
df$After_Top_Gap_Down <- detect_after_top_gap_down(df)
df$Three_Gap_Downs <- detect_three_gap_downs(df)
df$Three_Gap_Ups <- detect_three_gap_ups(df)

#CONTINUATION PATTERN

# Two-Day Patterns
df$Separating_Lines <- detect_separating_lines(df)
df$Bearish_On_Neck <- detect_bearish_on_neck(df)
df$Bullish_On_Neck <- detect_bullish_on_neck(df)
df$Bearish_In_Neck <- detect_bearish_in_neck(df)
df$Bullish_In_Neck <- detect_bullish_in_neck(df)
df$Bearish_Thrusting <- detect_bearish_thrusting(df)
df$Bullish_Thrusting <- detect_bullish_thrusting(df)

# Three-Day Patterns
df$Upside_Tasuki_Gap <- detect_upside_tasuki_gap(df)
df$Downside_Tasuki_Gap <- detect_downside_tasuki_gap(df)
df$Side_by_Side_White_Lines <- detect_side_by_side_white_lines(df)
df$Bullish_Side_By_Side_Black_Lines <- detect_bullish_side_by_side_black_lines(df)
df$Bearish_Side_By_Side_Black_Lines <- detect_bearish_side_by_side_black_lines(df)
df$Upside_Gap_Three_Methods <- detect_upside_gap_three_methods(df)
df$Downside_Gap_Three_Methods <- detect_downside_gap_three_methods(df)
df$Rest_after_Battle <- detect_rest_after_battle(df)

# Four-or-More-Day Patterns
df$Rising_Three_Methods <- detect_rising_three_methods(df)  
df$Falling_Three_Methods <- detect_falling_three_methods(df)  
df$Bullish_Mat_Hold <- detect_bullish_mat_hold(df)  
df$Bearish_Mat_Hold <- detect_bearish_mat_hold(df)  
df$Bullish_Three_Line_Strike <- detect_bullish_three_line_strike(df)  
df$Bearish_Three_Line_Strike <- detect_bearish_three_line_strike(df)  

install.packages("writexl")
library(writexl)
df[is.na(df)]<-0
write_xlsx(df, path="C:/Users/Admin/Desktop/Oil TTR/Oil (CS).xlsx")

names(df)
length(df)

selected_data =df[df$Downside_Tasuki_Gap==1,"Date"]
print(selected_data)

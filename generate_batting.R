# 同梱CSVを作ったときの条件。gelbach.R の --generate 指定時に読み込む。先頭の乱数設定で同じ36人を再現する。
set.seed(20261119, kind = "Mersenne-Twister", sample.kind = "Rejection")
n_players <- 36L
at_bats <- 200L

# 練習期間の開始前に測定する共変量。打率は安打数を打数で割った値。
experience_years <- sample(1:8, n_players, replace = TRUE)
experience_centered <- experience_years - 4.5
player_variation <- runif(n_players, -0.030, 0.030)
hit_probability_before_training <- 0.260 + 0.014 * experience_centered + player_variation
hits_before_training <- rbinom(n_players, at_bats, hit_probability_before_training)
previous_season_avg <- hits_before_training / at_bats

# 練習前の打率が低い選手や経験が短い選手ほど、特別練習に入りやすい。
centered_previous_season_avg <- previous_season_avg - 0.260
treatment_log_odds <- -0.60 * centered_previous_season_avg / 0.050 - 0.23 * experience_centered
treatment_probability <- plogis(treatment_log_odds)
treatment <- rbinom(n_players, size = 1L, prob = treatment_probability)

# 練習期間終了後は両群を同じ条件で測定する。
# 特別練習は期待打率を 0.025（2.5 パーセントポイント）上げる。
expected_avg_without_treatment <- 0.270 + 0.60 * centered_previous_season_avg +
  0.012 * experience_centered
hit_probability_after_training <- expected_avg_without_treatment + 0.025 * treatment
hits_after_training <- rbinom(n_players, at_bats, hit_probability_after_training)

simulated <- data.frame(
  player_id = sprintf("P%02d", seq_len(n_players)),
  special_training = treatment,
  experience_years = experience_years,
  previous_season_avg = previous_season_avg,
  batting_avg_after_training = hits_after_training / at_bats
)
write.csv(simulated, "batting.csv", row.names = FALSE, quote = FALSE)

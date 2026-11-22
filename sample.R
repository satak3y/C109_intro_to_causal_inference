# install.packages(c("broom", "dplyr"))
library(broom)
library(dplyr)

# 1. 読み込み（batting.csv を置いたフォルダで実行）
# setwd("C:/自分のフォルダ")
batting <- read.csv("batting.csv")

# 2. 加工：特別練習（1）と通常練習（0）に分け、練習前の共変量の平均差を求める。
covariates <- c("previous_season_avg", "experience_years")
special <- filter(batting, special_training == 1)
regular <- filter(batting, special_training == 0)
mean_gap <- colMeans(special[covariates]) - colMeans(regular[covariates])

# 3. 回帰：Y は練習後打率、T は特別練習の有無、B は前シーズン打率、K は経験年数。
# 基本モデル：Y = b0 + b1*T + e
base <- lm(batting_avg_after_training ~ special_training, data = batting)

# 共変量を一つ追加：Y = c0 + c1*T + c2*B + e または Y = d0 + d1*T + d2*K + e
with_season <- update(base, . ~ . + previous_season_avg)
with_experience <- update(base, . ~ . + experience_years)

# 完全モデル：Y = a0 + a1*T + a2*B + a3*K + e
full <- update(with_season, . ~ . + experience_years)

# tidy() の term は変数名、estimate は推定係数。model で4つの回帰を見分ける。
coefficients_df <- bind_rows(base = tidy(base), with_season = tidy(with_season),
                             with_experience = tidy(with_experience), full = tidy(full),
                             .id = "model")
print(select(coefficients_df, model, term, estimate), n = Inf)

# 4. Gelbach分解：「調整後 − 調整前」への寄与 = −完全モデルの係数 × 群間平均差。
# mean_gap[term] で各変数の平均差を取り出す。100倍してパーセントポイントに直す。
gelbach_df <- tidy(full) |>
  filter(term %in% covariates) |>
  mutate(group_gap = mean_gap[term], contribution_pp = -100 * estimate * group_gap) |>
  select(term, estimate, group_gap, contribution_pp)
print(gelbach_df)

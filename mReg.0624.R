getwd()  #ディレクトリの確認

library(tidyverse)  #データ読み込みのためにtidyverseを呼び出す
df <- read_csv("R_book_data.csv",  
               locale = locale(encoding = 'UTF-8'))  #UTF-8で読み込む（文字化け防止）
glimpse(df)  #データの確認

#今回使うパッケージを呼び出す：追加でインストールor呼び出すものもあります
library(rms)
library(ggplot2)


####1：データの準備：回帰分析に使う変数を抽出する####
#従属変数（Y）＝ LOS
#独立変数（X）＝ Age, Sex, DM, Severity, New_Treatment
df_Reg <- 
  df %>% 
  select(c(LOS, Age, Sex, DM, Severity, New_Treatment))  #df_Regに用いる変数を入れる
glimpse(df_Reg)

#一応...データの前処理：(1)欠測値の処理
#(1)欠測値の処理
#欠測値の確認：各変数の欠測値を数える
na.count_r1 <- sapply(df_Reg, function(y) sum(is.na(y)))
na.count_r1

#欠測値の処理（ざっくり：専門書で対処して下さい）
#リストワイズ除去：欠損値を一つでも含むケースをすべての分析から除去
#[Ref.: https://uribo.github.io/practical-ds/03/handling-missing-data.html]
df_Reg %>% 
  tidyr::drop_na() %>% 
  nrow()

#ペアワイズ除去：相関を利用する分析の場合，相関の計算に関係する欠損のみを除去
#[Ref.:https://bit.ly/3zFN5JI]
#任意の二変数が共にNAの場合のレコードを削除する：今回は完全データなので何も起こらない
df_Reg %>% 
  dplyr::mutate(flag = Age | LOS) %>%   #任意の二変数：今回はLOSとAge
  tidyr::drop_na(flag) %>% 
  dplyr::select(-flag) %>% 
  head()

#平均値代入法（単一代入法）：機械学習（予測）だとよくみる
#LOSの平均値を計算して整数に丸める
LOS.imp <- df_Reg %>% 
  dplyr::summarise(LOS_mean = round(mean(LOS, na.rm = TRUE), 0))

#平均値を代入：#is.na：欠測値かの確認，#ifelse：欠測値に平均値を代入
df_Reg %>% 
  dplyr::mutate(LOS.imp = ifelse(is.na(LOS), LOS.imp$LOS_mean, LOS)) %>%  
  dplyr::select(LOS, LOS.imp, Age, Sex, DM, Severity, New_Treatment) %>%  
  head()

##おまけ：MICE(連鎖方程式に基づく多重代入法)
#必要なパッケージのインストールと呼び出し
install.packages("mice")
library(mice)

#疑似完全データの数(m)は20：デフォルトは m = 5
#乱数発生をseed値=1に固定（再現性のため）
df_mice <- 
  df_Reg %>% 
  mice::mice(m = 20, printFlag = FALSE, seed = 1) %>% 
  mice::complete()
head(df_mice)  #先頭6行だけ表示


####2：データの確認（外れ値・相関・正規性）####
#必要なパッケージをインストールと呼び出し
install.packages("GGally")
library(GGally)

#全部変数の散布図行列を描く
ggpairs(df_Reg)  #LOSの分布が気になる...

#LOS(在院日数)の分布をヒストグラムで確認 → 右に裾が長い分布=尖度(Skewness)が正
hist(df_Reg$LOS, freq = FALSE)
lines(density(df_Reg$LOS), col = "orange", lwd = 2)  #推測した密度を描画(オレンジ・線幅2)

#LOS：右に裾が長い分布 → 対数をとると正規分布に近づく
#LOSを対数変換した値をデータに追加する
df_log <- 
  df %>% 
  select(c(LOS, Age, Sex, DM, Severity, New_Treatment))
df_log %>% 
  mutate(log_LOS = log(df_Reg$LOS)) -> df_log

glimpse(df_log)

hist(df_log$log_LOS, freq = FALSE)
lines(density(df_log$log_LOS), col = "orange", lwd = 2)

#df_logの散布図行列を描いてみる
ggpairs(df_log)

#おまけ：もう少し良い感じにしてみる
#[Ref1.: https://sawad0613.github.io/.github.io-ggpairs/]
#[Ref2.: https://assabu.exblog.jp/28933942/]

#まずは，データセットの値をクラス変換する(例：性別0/1を実数値から文字列にする)
df_plot <- df_log %>% select(c(LOS, log_LOS, Age))

df_plot %>% mutate(Sex_cat = as.character(df_log$Sex)) -> df_plot
df_plot %>% mutate(DM_cat  = as.character(df_log$DM)) -> df_plot
df_plot %>% mutate(Sv_ord  = as.ordered(df_log$Severity)) -> df_plot  #重症度は順序値へ
df_plot %>% mutate(NT_cat  = as.character(df_log$New_Treatment)) -> df_plot

glimpse(df_plot)

df_plot %>% 
  ggpairs(
    upper = list(continuous = "cor", combo = "box_no_facet", discrete = "count"),
    lower = list(continuous = "points", combo = "facethist", discrete = "facetbar"),
    diag  = list(continuous = "densityDiag", discrete = "barDiag"), 
    mapping = aes(color = Sex_cat)
  )
                      
#外れ値の確認と除去
#LOS(平均在院日数)と独立変数をプロットして外れ値をみる
plot(df_Reg$Age, df_Reg$LOS)
abline(h = 50, col = "blue")  #仮に，LOS>50日を外れ値とする
abline(v = 60, col = "red")   #仮に，Age<60才を外れ値とする
abline(v = 90, col = "red")   #仮に，Age>90才を外れ値とする

#外れ値の除去 == 外れ値じゃない値「だけ」データセットに入れる操作をする
df_out <- df_Reg[(df_Reg$LOS <= 50) & (df_Reg$Age >= 60) & (df_Reg$Age <= 90), ]
glimpse(df_out)

plot(df_out$Age, df_out$LOS)
abline(h = 50, col = "blue")
abline(v = 60, col = "red")
abline(v = 90, col = "red")

#各変数の正規性を確認する：プロット→検定
#分布をプロット
par(oma=c(0, 0, 5, 0))
par(mfrow=c(2, 3))

plot(density(df_log$Age))
plot(density(df_log$Sex))
plot(density(df_log$DM))
plot(density(df_log$Severity))
plot(density(df_log$New_Treatment))
dev.off()  #プロットツールをオフにする

#Q-Qプロット
par(oma=c(0, 0, 5, 0))
par(mfrow=c(2, 3))

qqnorm(df_log$Age)
qqline(df_log$Age, col = "red")
qqnorm(df_log$Sex)
qqline(df_log$Sex, col = "red")
qqnorm(df_log$DM)
qqline(df_log$DM, col = "red")
qqnorm(df_log$Severity)
qqline(df_log$Severity, col = "red")
qqnorm(df_log$New_Treatment)
qqline(df_log$New_Treatment, col = "red")
dev.off()  #プロットツールをオフにする

#正規性の検定：(1) Kolmogorov-Smirnov検定, (2) Shapiro-Wilk検定
#[Ref.: https://qiita.com/uri/items/e656f90e9dda342c54bb]

#(1) Kolmogorov-Smirnov検定
#Ho：二つの分布が等しい
ks.test(df_log, "pnorm", alternative = "two.sided")

#(2) Shapiro-Wilk検定
#H0：標本は正規母集団からサンプリングされたものである
shapiro.test(df_log$Age)
shapiro.test(df_log$Sex)
shapiro.test(df_log$DM)
shapiro.test(df_log$Severity)
shapiro.test(df_log$New_Treatment)

##おまけ：散布図を「動かして」確認する
#[Ref.: https://www.karada-good.net/analyticsr/r-223]
#必要なパッケージをインストールと呼び出し
install.packages("zoom")
library(zoom)

#例えば...LOSとAgeの散布図を確認したい
plot(df_Reg$LOS, df_Reg$Age)
session.zoom()  #動かしながら確認できる
#キーボードのescキーを押すとメニューが表示されます


####3：単回帰分析####
#[Ref.: https://kirikuroda.github.io/datareporting/association.html]
#回帰式の作成・確認 → 回帰式のプロット

##LOSが実測値(生データ)
uni_Age <- lm(LOS ~ Age, data = df_Reg)  #X = Age
summary(uni_Age)

attach(df_Reg)  #呼び出すデータセットを指定
plot(Age, LOS)  #x軸にAge，y軸にLOSとして散布図をプロット
abline(uni_Age, col = "red")  #散布図に回帰直線を赤色でプロット

##LOSが対数変換値
uni_log <- lm(log_LOS ~ Age, data = df_log)
summary(uni_log)

attach(df_log)
plot(Age, log_LOS)
abline(uni_log, col = "red")

##おまけ1：〇〇別解析のモデリングと結果のプロット
#男女別の回帰分析を行う：(X, Y) = (Severity, LOS)
m.df_Reg <- df_Reg[df_Reg$Sex == 1, ]  #男性(1)だけ抽出したデータを作成
f.df_Reg <- df_Reg[df_Reg$Sex == 2, ]  #女性(2)だけ抽出したデータを作成

m.df_Reg  #データの確認：男性(1)
f.df_Reg  #データの確認：女性(2)

m.uni_Sev <- lm(LOS ~ Severity, data = m.df_Reg)  #男性(1)の回帰分析
f.uni_Sev <- lm(LOS ~ Severity, data = f.df_Reg)  #女性(2)の回帰分析

summary(m.uni_Sev)  #男性(1)の回帰分析の結果
summary(f.uni_Sev)　#女性(2)の回帰分析の結果

#回帰分析のプロット
plot(df_Reg$Severity, df_Reg$LOS, col = Sex)  #男性(1)「黒」，女性(2)「赤」
abline(m.uni_Sev)               #男性(1)の回帰直線を「黒」でプロット
abline(f.uni_Sev, col = "red")  #女性(2)の回帰直線を「赤」でプロット

##おまけ2：両対数を取って回帰式を当てはめる
y <- log(df_Reg$LOS)
x <- df_Reg$Age
plot(x, y)

uni_logAge <- lm(log(LOS) ~ log(Age), data = df_Reg)
summary(uni_logAge)

attach(df_Reg)
plot(log(Age), log(LOS))
abline(uni_logAge, col = "red")  #回帰直線を赤色でプロット


####4：重回帰分析〜ここからテキスト〜####

##回帰式の作成：LOSが実測値
model_r1 <- glm(LOS ~ Age + Sex + DM + Severity + New_Treatment,
                family = gaussian(link = "identity"),
                data = df_Reg)
result_r1 <- summary(model_r1)
result_r1  #回帰式の概要の表示

##回帰式の作成：LOSが対数変換値
model_log <- glm(log_LOS ~ Age + Sex + DM + Severity + New_Treatment, 
                 family = gaussian(link = "identity"), 
                 data = df_log)
result_log <- summary(model_log)
result_log


####5：結果の解釈(モデル)：残差の分析####

#単回帰分析の残差分析(LOSが実測値)
par(mfrow=c(2, 2))
plot(uni_Age)

#単回帰分析の残差分析(LOSが対数変換値)
par(mfrow=c(2, 2))
plot(uni_log)

#重回帰分析の残差分析(LOSが実測値)
par(mfrow=c(2, 2))
plot(model_r1)

#重回帰分析の残差分析(LOSが対数変換値)
par(mfrow=c(2, 2))
plot(model_log)


####6：結果の解釈(モデル)：VIF・決定係数・モデルの検定####

#VIFの計算
#VIF(分散拡大要因)：独立変数間の多重共線性を検出するための指標の1つ
#VIF≦2.0：Good, VIF>10：too Bad = 多重共線性あり
vif(model_r1)   #独立変数間のVIF

##決定係数（R^2値）の算出
#R^2値とは：モデルによって説明された従属変数の変動の合計の割合
#R^2 = (TSS-SSR)/TSS = 1 - SSR/TSS

#TSS（総平方和）：従属変数と平均との距離の2乗に基づく従属変数の合計
#SSR（残差平方和）：Xによって説明されていないYの残差の変動

#TSSとSSRを計算する
TSS_r1 <- sum((df_Reg$LOS - mean(df_Reg$LOS))^2)
SSR_r1 <- sum(resid(model_r1)^2)

#R^2 value : R^2 = (TSS-SSR)/TSS
Rsqrt_r1 <- (TSS_r1 - SSR_r1)/TSS_r1
Rsqrt_r1

##決定係数を計算する関数##
#[Ref.: 今井耕介，社会科学のためのデータ分析入門（上）]
R2 <- function(model) {
  resid <- resid(model)        #残差
  y <- fitted(model) + resid   #従属変数
  TSS <- sum((y - mean(y))^2)  #TSS：総平方和
  RSS <- sum(resid^2)          #RSS：残差平方和
  R2 <- (TSS - RSS) / TSS      #R^2：決定係数
  return(R2)
}

R2(model_r1)   #model_r1の決定係数を算出する
R2(model_log)  #model_logの決定係数を算出する

#調整済み決定係数（adjusted R^2値）の算出
#調整済みR^2 = 1 - (SSR/(n-p-1))/(TSS/(n-1))
#adj R^2：複数の独立変数がある場合に，自由度を補正した決定係数のこと
#n：サンプルサイズ，p+1：p個の独立変数と切片の数：：自由度は(n-p-1)

#決定係数と同様に関数を組んでみる
adjR2 <- function(model) {
  resid <- resid(model)　　　　　　　　　　　　　　　　#残差
  y <- fitted(model) + resid　　　　　　　　　　　　　 #結果
  n <- length(y)　　　　　　　　　　　　　　　　　　　 #サンプルサイズ
  TSS.adj <- sum((y - mean(y))^2) / (n - 1)            #調整TSS
  SSR.adj <- sum(resid^2) / (n - length(coef(model)))  #調整RSS
  R2.adj <- 1 - SSR.adj / TSS.adj                      #調整R^2
  return(R2.adj)
}

adjR2(model_r1)   #model_r1の調整済み決定係数を算出する
adjR2(model_log)  #model_logの調整済み決定係数を算出する

#モデルの検定：係数制約の検定

#共変量の各指標が効果を持たない or 回帰式から除外できるかの検定
#b0 = 誤差項(ε), b1 = Age, b2 = Sex, b3 = DM, b4 = Severity, b5 = New_Treatment
#Age, Sexは必ず投入する変数なので検定の対象外とする
#H0：b3 = 0, b4 = 0, b5 = 0 : DM, Severity, New_Treatmentが
#H1：H0 is NOT ture

#必要なパッケージのインストールと呼び出し
install.packages("lmtest")
library(lmtest)

#非制約モデル：LOS ~ Age + Sex + DM + Severity + New_Treatment
no.rest <- glm(LOS ~ Age + Sex + DM + Severity + New_Treatment,
               family = gaussian(link = "identity"),
               data = df_Reg)
#制約モデル：LOS ~ Age + Sex
with.rest <- glm(LOS ~ Age + Sex,
                 family = gaussian(link = "identity"),
                 data = df_Reg)

#Wald検定の実施：H0(制約モデル) vs H1(非制約モデル)
waldtest(no.rest, with.rest)
#[2] 49.232：各係数=0という仮説(H0)が正しい時のF統計量
#[2] < 2.2e-16 ***：p < 0.001 なのでH0は棄却されH1が採択される


####7：結果の解釈(偏回帰係数)####

#R^2値・モデルのF検定：分散分析・偏回帰係数のｔ検定
##偏回帰係数（bまたはB）の表示
result_b <- result_r1$coefficients
result_b
##偏回帰係数（bまたはB）の95%CIの表示
result_CI <- confint(model_r1)
result_CI

##標準化偏回帰係数（β）の算出・表示
ssd <- sapply(df_Reg, sd)  #不偏標準偏差を求める
ssd
#独立変数の偏回帰係数に不偏標準偏差を掛けて標準化偏回帰係数を求める
beta_r1 <- result_b[2:6, 1]*ssd[2:6]/s[1]
beta_r1
#result_b[2:6, 1] #独立変数の偏回帰係数を抽出したもの

                      
#実測LOS vs 対数LOS
options(scipen=100)  #指数表記をやめさせる(?)
#実測LOS
result_r1
##偏回帰係数（bまたはB）を取り出す
result_b <- result_r1$coefficients[1:6, 1:2]
##偏回帰係数（bまたはB）の95%CIの算出
result_CI <- confint(model_r1)
#t値とそのp値を取り出す
result_tp <- result_r1$coefficients[1:6, 3:4]
#独立変数間のVIF
vif_r1 <- vif(model_r1)
VIF_r1 <- c(0, vif_r1[1:5] )  #切片は空白なのでとりあえず0を入れる
#テーブル化
table_r1 <- cbind(result_b, result_CI, result_tp, VIF_r1)

#対数LOS
result_log
##偏回帰係数（bまたはB）を取り出す
result_logb <- result_log$coefficients[1:6, 1:2]
##偏回帰係数（bまたはB）の95%CIの算出
result_logCI <- confint(model_log)
#t値とそのp値を取り出す
result_logtp <- result_log$coefficients[1:6, 3:4]
#独立変数間のVIF
vif_log <- vif(model_log)
VIF_log <- c(0, vif_log[1:5] )  #切片は空白なのでとりあえず0を入れる
#テーブル化
table_log <- cbind(result_logb, result_logCI, result_logtp, VIF_log)

#中身の確認：実測LOS vs 対数LOS
#LOS実測値
round(table_r1, digits = 3)   #表が見にくいので便宜上小数点3桁までに丸める
#LOS対数変換値
round(table_log, digits = 3)  #表が見にくいので便宜上小数点3桁までに丸める

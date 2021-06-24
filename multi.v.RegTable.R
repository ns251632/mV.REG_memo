#データの読み込みなどは省略しています

##回帰式の作成：LOSが実測値
model_r1 <- glm(LOS ~ Age + Sex + DM + Severity + New_Treatment,
                family = gaussian(link = "identity"),
                data = df_Reg)
result_r1 <- summary(model_r1)
result_r1  #回帰式の概要の表示

##偏回帰係数（bまたはB）を取り出す
result_b <- result_r1$coefficients[1:6, 1:2]
##偏回帰係数（bまたはB）の95%CIの算出
result_CI <- confint(model_r1)
##標準化偏回帰係数（β）の算出・表示
beta_r1 <- result_b[2:6, 1]*s[2:6]/s[1]
#t値とそのp値を取り出す
result_tp <- result_r1$coefficients[1:6, 3:4]
#独立変数間のVIF
vif_r1 <- vif(model_r1)
VIF_r1 <- c(0, vif_r1[1:5] )  #切片は空白なのでとりあえず0を入れる

##結果の出力：回帰モデル
table_r1 <- cbind(result_b, result_CI, beta_r1, result_tp, VIF_r1)
table_r1  #中身の確認
write.csv(table_r1, 
          "重回帰分析_LOS実測値_結果.csv")  #ここで "/ダイレクトパス/ファイル名.csv" とすると任意のフォルダに保存できる

#結果の出力：モデルの評価(R2, adjR2, AIC)
table_rM <- cbind(R2_r1 = R2(model_r1), adjR2_r1 = adjR2(model_r1), AIC_r1 = result_r1$aic)
table_rM  #中身の確認
write.csv(table_rM, 
          "重回帰分析_LOS実測値_モデル評価.csv")  #ここで "/ダイレクトパス/ファイル名.csv" とすると任意のフォルダに保存できる

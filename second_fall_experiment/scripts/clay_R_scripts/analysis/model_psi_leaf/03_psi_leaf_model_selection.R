# Add leaftemp_mean =-= BEST MODEL ? (All coefficients significant, and highest R2, lowest MSE)
lm_1 <- lm(mean_psi_MPa ~ bmp_box_temp + minutes + irrig + leaftemp_mean, df2); summary(lm_1)
mean(lm_1$residuals^2); mean(abs(lm_1$residuals))


# drop leaf temp...Keeping it in model is advised. 
anova(update(lm_1, .~.-leaftemp_mean, df2), lm_1, test="LRT")
summary(update(lm_1, .~.-leaftemp_mean, df2))

# drop box_temp
anova(update(lm_1, .~.-bmp_box_temp, df2), lm_1, test="LRT")


### Physical-based models (no leaf temperature)
# Same model as above but using all 60 observations (and no leaftemp).... NO IMPROVEMENT.
lm_p1 <- lm(mean_psi_MPa ~  bmp_box_temp + minutes + irrig + block, comb); summary(lm_p1)
mean(lm_p1$residuals^2); mean(abs(lm_p1$residuals))
# use air temp (low) instead of box temp
lm_p2 <- lm(mean_psi_MPa ~  sht2_low_temp + minutes + irrig + block, comb); summary(lm_p2)
mean(lm_p2$residuals^2); mean(abs(lm_p2$residuals))
# use air temp (high) ... not as good.
lm_p3 <- lm(mean_psi_MPa ~  sht1_high_temp + minutes + irrig + block, comb); summary(lm_p3)
mean(lm_p3$residuals^2); mean(abs(lm_p3$residuals))


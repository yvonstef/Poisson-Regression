require(tidyverse)
require(sandwich)
require(msm)
require(zoo)
require(ggmap)

crashes_data = Traffic_Crashes_._Crashes

crashes_data$ADDRESS = paste(crashes_data$STREET_NO,crashes_data$STREET_DIRECTION,crashes_data$STREET_NAME)
crashes_data$CRASH_DATE =  as.Date(crashes_data$CRASH_DATE, format = "%m/%d/%Y")

#Addresses without traffic controls
#missing records correction

table(is.na(crashes_data))
crashes_data = na.omit(crashes_data)
table(is.na(crashes_data))


traffic_controls = crashes_data[ crashes_data$TRAFFIC_CONTROL_DEVICE != "NO CONTROLS"
                                  & crashes_data$DEVICE_CONDITION != "NO CONTROLS"  
                                   & crashes_data$TRAFFIC_CONTROL_DEVICE != "UNKNOWN"
                                   & crashes_data$DEVICE_CONDITION != "UNKNOWN"
                                   & crashes_data$TRAFFIC_CONTROL_DEVICE != "OTHER"
                                   & crashes_data$WEATHER_CONDITION != "OTHER",]
#mention why choose weather over surface condition

accidents_count = traffic_controls %>% group_by(ADDRESS,LIGHTING_CONDITION,TRAFFIC_CONTROL_DEVICE,DEVICE_CONDITION,POSTED_SPEED_LIMIT,ROAD_DEFECT,ALIGNMENT,ROADWAY_SURFACE_COND) %>% summarise(count=n())

accidents_count = accidents_count[accidents_count$ROADWAY_SURFACE_COND != "UNKNOWN" & accidents_count$LIGHTING_CONDITION != "UNKNOWN" & accidents_count$ROAD_DEFECT != "UNKNOWN",]

summary(accidents_count)

with(accidents_count, tapply(count,DEVICE_CONDITION, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

with(accidents_count, tapply(count,TRAFFIC_CONTROL_DEVICE, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

with(accidents_count, tapply(count,LIGHTING_CONDITION, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

with(accidents_count, tapply(count,POSTED_SPEED_LIMIT, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))

with(accidents_count, tapply(count,ROADWAY_SURFACE_COND, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

with(accidents_count, tapply(count,ALIGNMENT, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))

with(accidents_count, tapply(count,ROAD_DEFECT, function(x) {
  sprintf("Mean (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))





ggplot(accidents_count, aes(count, fill = DEVICE_CONDITION)) +
  geom_histogram(binwidth=1, position="dodge") + xlab("Number of Accidents")


ggplot(accidents_count, aes(count, fill = TRAFFIC_CONTROL_DEVICE)) +
  geom_histogram(binwidth=1, position="dodge")+ xlab("Number of Accidents")

ggplot(accidents_count, aes(count, fill = LIGHTING_CONDITION)) +
  geom_histogram(binwidth=1, position="dodge") + xlab("Number of Accidents")

ggplot(accidents_count, aes(count, fill = ROADWAY_SURFACE_COND)) +
  geom_histogram(binwidth=1, position="dodge") + xlab("Number of Accidents")

ggplot(accidents_count, aes(count, fill = ALIGNMENT)) +
  geom_histogram(binwidth=1, position="dodge") + xlab("Number of Accidents")

ggplot(accidents_count, aes(count, fill = ROAD_DEFECT)) +
  geom_histogram(binwidth=1, position="dodge") + xlab("Number of Accidents")


summary(model <- glm(count ~ ROADWAY_SURFACE_COND+LIGHTING_CONDITION+TRAFFIC_CONTROL_DEVICE+DEVICE_CONDITION + POSTED_SPEED_LIMIT+ROAD_DEFECT+ALIGNMENT, family="poisson", data=accidents_count))



with(model, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))

accidents_count$pred = predict(model, type="response")

accidents_count %>%
  group_by(ADDRESS)%>%
  arrange(desc(pred))%>%
  head(5)%>%
  ggplot(aes(reorder(ADDRESS,pred),pred))+
  geom_col()+
  coord_flip()+
  geom_label(aes(label=pred),size=2)+
  labs(title="Accidents  Estimation", subtitle="Top 5",y="Counts",x="")





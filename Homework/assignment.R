


install.packages("readr")  # 安装readr包
library(readr)  # 加载readr包
dataset <- read_csv("C:/Users/13263/Desktop/ds_salaries.xlsx")
num_observations <- nrow(dataset)
print(num_observations)
head(df)
ggplot(df, aes(x = salary_currency)) +
  geom_histogram()

library(readxl)
df <- read_excel("C:/Users/13263/Desktop/ds_salaries.xlsx")

usd_data <- df[df$salary_currency == "USD", ]
mean_usd_value <- mean(usd_data$salary, na.rm = TRUE)

install.packages("dplyr")
library(dplyr)

mean_usd_value <- df %>%
  filter(salary_currency == "USD") %>%
  summarize(mean_value = mean(salary, na.rm = TRUE)) %>%
  pull(mean_value)



usd_data <- df[df$salary_currency == "USD", ]
median_usd_value <- median(usd_data$salary, na.rm = TRUE) 
library(dplyr)

median_usd_value <- df %>%
  filter(salary_currency == "USD") %>%
  summarize(median_value = median(salary, na.rm = TRUE)) %>%
  pull(median_value)
mean_local_currency_value <- mean(df$`local currency`, na.rm = TRUE)
mean_local_currency_value <- df %>%
  summarize(mean_value = mean(`local currency`, na.rm = TRUE)) %>%
  pull(mean_value)

library(ggplot2)

# Filter the data
usd_salaries <- df[df$salary_currency == "USD", ]

# Plot the histogram
ggplot(data = usd_salaries, aes(x = salary)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) + # You can adjust the binwidth as needed
  labs(title = "Distribution of Salaries in USD",
       x = "Salary in USD",
       y = "Frequency") +
  theme_minimal()
options(scipen = 100)

install.packages("scales")
library(scales)  # 用于数字格式化

# 筛选出USD薪水数据
usd_salaries <- df[df$salary_currency == "USD", ]

# 绘制直方图并调整x轴标签
ggplot(data = usd_salaries, aes(x = salary)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Salaries in USD",
       x = "Salary in USD",
       y = "Frequency") +
  scale_x_continuous(labels = comma) +  # 使用comma函数调整x轴的标签格式
  theme_minimal()


library(ggplot2)

# Filter the data for salaries in USD
usd_salaries <- df[df$salary_currency == "USD", ]

# Construct the boxplots
ggplot(data = usd_salaries, aes(x = company_size, y = salary)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Distribution of Salaries in USD by Company Size",
       x = "Company Size",
       y = "Salary in USD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # This rotates x-axis labels for better visibility


ggplot(data = df[df$salary_currency == "USD", ], aes(x = salary)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 200000, 20000)) + # Adjust these values as needed
  theme_minimal()

ggplot(data = df[df$salary_currency == "USD", ], aes(x = salary)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(labels = comma) +  # Display numbers with comma separators
  theme_minimal()


number_of_rows <- nrow(df)
print(number_of_rows)

dimensions <- dim(df)
number_of_rows <- dimensions[1]
print(number_of_rows)

number_of_columns <- ncol(df)
print(number_of_columns)


dimensions <- dim(df)
number_of_columns <- dimensions[2]
print(number_of_columns)










str(BEEPS)
nominal_variables <- sum(sapply(BEEPS, is.factor))
print(nominal_variables)

View(BEEPS)
# 找到包含缺失值的数据点
missing_rows <- which(apply(is.na(BEEPS), 1, any))

# 查看包含缺失值的行数
print(missing_rows)

install.packages("psych")  # 安装psych包
library(psych)             # 加载psych包
# 假设你有一个变量测量级别的向量（"continuous", "nominal", "ordinal"）
measurement_levels <- c("id", "", "ordinal", "continuous")

# 使用describe()函数生成描述性统计信息报告
describe(BEEPS, measures = measurement_levels)

BEEPS <- BEEPS %>% mutate(b4a = ifelse(is.na(column_name), mean(column_name, na.rm = TRUE), b4a))

BEEPS <- BEEPS %>% mutate(column_name = ifelse(is.na(column_name), mean(column_name, na.rm = TRUE), column_name))

# 使用mice包进行缺失值插补
install.packages("mice")
library(mice)
imputed_data <- mice(beeps, method = "pmm", m = 5) 

boxplot(beeps$I1, main="Boxplot of I1", ylab="I1 Values")



beeps_clean <- na.omit(beeps)
View(beeps_clean)


boxplot(beeps$I4, main="Boxplot of I4", ylab="I4 Values")

boxplot(beeps$I4, main="Boxplot of I4", ylab="I4 Values")

beeps <- beeps[complete.cases(beeps), ]

# 检查数据中是否包含无限值
any(is.infinite(beeps$I1))

# 绘制 I4 列的箱线图
boxplot(beeps_clean$l1, main="Boxplot of l1", ylab="l1 Values")


summary(beeps$l1)
hist(beeps$l1, main="Histogram of l1", xlab="l1 Values", ylab="Frequency")
# Create a Q-Q plot for l1
qqnorm(beeps$l1)
qqline(beeps$l1)
shapiro.test(beeps$l1)


sample_mean <- mean(your_data$permanent_employees)
sample_mean <- mean(beeps_clean$permanent_employees)

# 安装zoo包（如果尚未安装）
install.packages("zoo")

# 加载zoo包
library(zoo)
# 将数据框中的所有缺失值填充为0
dataset <- na.fill(dataset, fill = 0)


dataset <- na.fill(dataset, fill = 0)
library(utils)

# 将beeps数据框中的所有缺失值填充为0
# 假设beeps是你的数据框
beeps_filled <- beeps  # 创建一个新的数据框以保存结果
beeps_filled[is.na(beeps_filled)] <- 0
# 计算Z-Score
z_scores <- scale(beeps$l1)
# Save outlier values as a separate object
outliers <- boxplot(beeps$l1, plot = FALSE)$out

# Create a copy of the dataset without outliers
beeps_clean <- beeps[-which(beeps$l1 %in% outliers), ]

# Find the smallest outlier
min(boxplot(beeps$l1, plot = FALSE)$out)

# Save filtered dataset 
beeps_clean <- beeps[beeps$l1< 146, ]

# Build a simple boxplot
boxplot(beeps_clean$l1,  main = "Number of permanent full time employees at end of last Fiscal year")

# Descriptive statistics for l1
describe(beeps$l1)

# Create a histogram of l1
hist(beeps$l1, main="Histogram of l1", xlab="l1 Values", ylab="Frequency")
# Create a Q-Q plot for l1
qqnorm(beeps$l1)
qqline(beeps$l1)

# Shapiro-Wilk normality test
shapiro.test(beeps$l1)

# Create a boxplot for variable l1
boxplot(beeps$l1, main="Boxplot of l1", ylab="l1 Values")

# Calculate the mean for the entire sample
sample_mean <- mean(beeps$l2)
colnames(beeps)


# Create two separate R objects corresponding to each subsample
beeps_clean1 <- beeps_clean[beeps_clean$country == "Slovak Rep.", ]
beeps_clean2 <- beeps_clean[beeps_clean$country == "Slovenia", ]

# Describe two subsamples

## Country 1
describe(beeps_clean1$l1)
## Country 2
describe(beeps_clean2$l1)

# (Parametric) t-test for country 1
t.test(beeps_clean1$l1, mu = 25)

# (Parametric) T-test for comparing means of both countries
t.test(beeps_clean1$l1,  beeps_clean2$l1, var.equal = TRUE)

# 进行两样本t检验
result <- t.test(beeps_clean1$l1, beeps_clean2$l2, var.equal = TRUE)

res.aov1 <- aov(l1 ~ b1 , data = beeps_clean1)
res.aov2 <- aov(l1 ~ b1 , data = beeps_clean2)
View(res.aov1)
View(res.aov2)
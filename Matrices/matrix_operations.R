
Games
rownames(Games)
colnames(Games)

# How many plays did LeBron James played in 2008
Games["LeBronJames","2008"] # 81

# Calculating the field goals per game
FieldGoals / Games
# Rounding with 1 decimal
round(FieldGoals / Games,1)

# Minutes played per game
round(MinutesPlayed / Games)


# Finding max and min salary of all the players across all the years along with names
apply(Salary, 1, range)

# 1 indicates rows

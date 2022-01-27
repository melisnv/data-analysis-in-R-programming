
FieldGoals

# transposing the table 
t(FieldGoals)

# Normalizing the data first
matplot(t(FieldGoals/Games), type = 'b', pch = 15:18, col=c(1:4,6))

# adding legend
legend("bottomleft", inset = 0.01, legend = Players, 
       col=c(1:4,6), pch = 15:18, horiz = F)


# Normalizing the data first
matplot(t(FieldGoals/FieldGoalAttempts), type = 'b',
        pch = 15:18, col=c(1:4,6))

# accuracy
legend("bottomleft", inset = 0.01, legend = Players, 
       col=c(1:4,6), pch = 15:18, horiz = F, cex = 0.3)
library("mRMRe")

load("FS.Rdata")
df <- cbind(X, Y)
print(ncol(df))

f_data <- mRMR.data(data = data.frame(df))
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 21, feature_count = 4)

print(results)

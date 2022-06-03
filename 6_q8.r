x=1

# a priori probabilities (Uniforme donc 0.5 & 0.5)
p.m=1/2
p.p=1-p.m
# inverse proababilities
px.p=dunif(x,-1,1)

# p(x=0| y=1)
px.m=dunif(x,1,2)

## conditional probabilities
# p (y=1|x)
py.p=px.p*p.p/(px.p*p.p+px.m*p.m)
# p (y=-1|x)
py.m=px.m*p.m/(px.p*p.p+px.m*p.m)

print(paste("If x=",x, ":"))
print(paste("   P(y=1|x)=",py.p))
print(paste("   P(y=-1|x)=",py.m))

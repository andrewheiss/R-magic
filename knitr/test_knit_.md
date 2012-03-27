<!--roptions dev='png', fig.width=5, fig.height=5 -->
Now we write some code chunks in this markdown file:

<!--begin.rcode
x <- 1:100
y <- rnorm(100)
model <- lm(y~x)
summary(model)
end.rcode-->

We can also produce plots:

<!--begin.rcode md-cars-scatter, message=FALSE
library(ggplot2)
qplot(hp, mpg, data=mtcars)+geom_smooth()
end.rcode-->

So no more hesitation on using GitHub and **knitr**! You just write a minimal amount of code to get beautiful output on the web.
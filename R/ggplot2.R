library(ggplot2)

## iris

plot(iris$Sepal.Width, iris$Sepal.Length)
qplot(Sepal.Width, Sepal.Length, 
      data = iris)
q0 <- ggplot(data = iris, 
             mapping = aes(x = Sepal.Width,
                           y = Sepal.Length,
                           colour = Species))
q1 <- qplot(Sepal.Width, Sepal.Length, 
            colour = Species, 
            data = iris)
q1
q2 <- q1 + 
  theme_bw()
q2
q3 <- q2 +
  geom_smooth(method = loess, se = FALSE)
q3
q1.1 <- q0 +
  geom_point()
q1.1
q4 <- q0 +
  stat_density_2d(aes(alpha = ..density..), 
                 contour = FALSE)
q4
q5 <- q4 +
  facet_wrap(~ Species)
q5

## mpg

qplot(displ, hwy, data = mpg) +
  facet_grid(. ~ cyl)
qplot(displ, hwy, data = mpg) +
  facet_grid(drv ~ . )
qplot(displ, hwy, data = mpg) +
  facet_grid(drv ~ cyl)
qplot(displ, hwy, data = mpg) +
  facet_wrap(~ class)
qplot(displ, hwy, 
      data = mpg, 
      colour = class)
qplot(displ, hwy, 
      data = mpg, 
      geom = "smooth")
qplot(displ, hwy, 
      data = mpg, 
      geom = c("point", "smooth"))   
qplot(class, hwy, 
      data = mpg)   
qplot(class, hwy, 
      data = mpg, 
      geom = "boxplot")   
qplot(reorder(class, hwy), hwy, 
      data = mpg, 
      geom = "boxplot")
qplot(reorder(class, hwy, 
              FUN = median), hwy, 
      data = mpg, 
      geom = "boxplot")
qplot(cty, hwy, data = mpg)
m0 <- ggplot(data = mpg, 
       mapping = aes(x = cty, y = hwy))
m1 <- m0 + geom_point()
m1
m2 <- m0 + geom_point(position = position_jitter())
m2
m3 <- m1 + geom_jitter()
m3

## Diamonds

rm(diamonds) ## R 내장 오브젝트로 전환 
qplot(x, z, data = diamonds) ## `x = 0`은 불가능한 값. 
min(diamonds$x)  ## 최소값으로 검색  
x.zero <- which(diamonds$x == 0) ## 모두 8개로 파악. 
diamonds[x.zero, ] ## 데이터 파악. `y > 0` 한 경우만 있음. 
diamonds[x.zero, c("x", "y", "z")] <- NA ## NA로 수정   
max(diamonds$z, na.rm = TRUE) ## NA를 제거하지 않으면 최대값이 NA로 잡힘.
z.max <- which(diamonds$z == 31.8) ## 최대값의 인덱스 저장
diamonds[z.max, ] ## 최대값 케이스 파악 
head(diamonds, n = 20) ## z는 정의에 따라 x, y보다 작아야 하므로 31.8은 3.18을 잘못 쓴 것으로 파악.
diamonds[z.max, "z"] <- 3.18 ## z 최대값을 수정.
qplot(x, z, 
      data = diamonds, 
      na.rm = TRUE)
qplot(x, 
      data = diamonds, 
      na.rm = TRUE)
qplot(cut, 
      data = diamonds)
qplot(cut, 
      data = diamonds, 
      fill = cut)
qplot(cut, 
      data = diamonds, 
      colour = cut)
qplot(color, 
      data = diamonds, 
      fill = cut)
qplot(color, 
      data = diamonds, 
      fill = cut, 
      position = "dodge") ## Deprecated
g0 <- ggplot(data = diamonds, 
             mapping = aes(x = color,
                           fill = cut)) 
g1 <- g0 + geom_bar()
g1
g2 <- g0 + geom_bar(position = position_dodge())
g2
g3 <- g0 + geom_bar(position = position_fill())
g3
g4 <- g0 + geom_bar(position = position_stack())
g4

qplot(carat, 
      data = diamonds)
qplot(carat, 
      data = diamonds, 
      binwidth = 0.1)
qplot(carat, 
      data = diamonds, 
      binwidth = 0.01)

summary(diamonds$depth)
sd(diamonds$depth)
zoom <- coord_cartesian(xlim = c(55, 70))
qplot(depth, 
      data = diamonds, 
      binwidth = 0.2) 
qplot(depth, 
      data = diamonds, 
      binwidth = 0.2) +
  zoom
qplot(depth, 
      data = diamonds, 
      binwidth = 0.2, 
      fill = cut) +
  zoom
qplot(depth, 
      data = diamonds, 
      binwidth = 0.2) +
  zoom +
  facet_wrap(~ cut)
qplot(depth, 
      data = diamonds, 
      binwidth = 0.2,
      geom = "freqpoly", 
      colour = cut) +
  zoom +
  facet_wrap(~ cut) 
qplot(depth, 
      data = diamonds, 
      binwidth = 0.2,
      geom = "freqpoly", 
      colour = cut) +
  zoom
qplot(depth, 
      data = diamonds, 
      geom = "density", 
      colour = cut) +
  zoom
qplot(price, 
      data = diamonds, 
      binwidth = 500) +
  facet_wrap(~ cut) 
qplot(price, 
      data = diamonds, 
      binwidth = 500,
      fill = cut)
qplot(price, 
      data = diamonds, 
      binwidth = 500,
      geom = "freqpoly",
      colour = cut)
qplot(price, 
      data = diamonds, 
      binwidth = 500,
      geom = "density",
      colour = cut)
qplot(carat, price,
      data = diamonds, 
      colour = cut)
qplot(carat, price,
      data = diamonds,
      geom = "bin2d")
# install.packages("hexbin", 
                 repos = "https://cran.rstudio.com")
library(hexbin)
qplot(carat, price,
      data = diamonds,
      geom = "hex")
qplot(carat, price,
      data = diamonds,
      geom = "density2d")
qplot(carat, price,
      data = diamonds,
      geom = c("point", "density2d"))
qplot(carat, price,
      data = diamonds,
      geom = "smooth")
qplot(carat, price,
      data = diamonds,
      geom = "smooth",
      colour = cut)
qplot(carat, price,
      data = diamonds,
      geom = "smooth",
      group = cut)
qplot(carat, price,
      data = diamonds,
      geom = "smooth",
      colour = cut,
      se = FALSE)
qplot(carat, price,
      data = diamonds,
      geom = "smooth",
      colour = cut,
      method = lm)
qplot(carat, price,  ## "blue" 는 1로 인식
      data = diamonds,
      colour = "blue")
qplot(carat, price,  ## `I()` 함수 사용
      data = diamonds,
      colour = I("blue"))
qplot(carat, price,  
      data = diamonds)
qplot(carat, price,  
      data = diamonds,
      size = I(0.5))
qplot(carat, price,  
      data = diamonds,
      alpha = I(0.1))
qplot(carat, price,  
      data = diamonds,
      size = I(0.5),
      alpha = I(0.1))

## Save 

ggsave("../pics/my-plot.pdf")
ggsave("../pics/my-plot.png")
ggsave("../pics/my-plot2.pdf", width = 6, height = 6)

## texas

library(readr)
texas <- read_csv("https://raw.githubusercontent.com/rstudio/Intro/master/data/texas.csv")
View(texas)
qplot(long, lat, 
      data = texas)
head(texas)
qplot(long, lat, 
      data = texas,
      geom = "polygon",
      color = I("white"),
      fill = I("black"), 
      group = group)
texas2 <- texas[sample(nrow(texas)), ] ## 강의 오타 수정 
qplot(long, lat,
      data = texas2, 
      geom = "polygon",
      group = group)

## maps

library(maps)
counties <- map_data("county")
qplot(long, lat,
      data = counties, 
      geom = "polygon",
      group = group,
      fill = group)
help(package = "maps")
tx <- qplot(long, lat, 
      data = texas,
      geom = "polygon",
      fill = bin, 
      group = group)
tx
tx + scale_fill_brewer(palette = "Blues")

class(tx)
str(tx)

cp <- qplot(carat, price,
            data = diamonds)
cp  
class(cp)
str(cp)
cp1 <- cp + 
  labs(caption = "cp") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cp1 
cp2 <- cp + 
  coord_polar() + labs(caption = "cp + coord_polar()") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cp2
cp3 <- cp + 
  coord_flip() + labs(caption = "cp + coord_flip()") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cp3
cp4 <- cp + 
  coord_fixed(ratio = 1 / 10000) +
  labs(caption = "cp + coord_fixed(ratio = 1 / 10000)") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cp4
cp5 <- cp +
  coord_trans(y = "log10", x = "log10") + ## ytrans, xtrans deprecated
  labs(caption = 'cp + coord_trans(y = "log10", x = "log10")') +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cp5
cp6 <- cp + 
  coord_cartesian(ylim = c(0, 5000),
                  xlim = c(0, 1)) +
  labs(caption = 'cp + coord_cartesian(ylim = c(0, 5000), xlim = c(0, 1)') +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cp6
library(grid)
library(gridExtra) ## 두 그림을 한 장에 
grid.arrange(cp1, cp2, 
             ncol = 2,
             top = textGrob("Polar", gp = gpar(fontsize = 24)))
grid.arrange(cp1, cp3, 
             ncol = 2,
             top = textGrob("flip", gp = gpar(fontsize = 24)))
grid.arrange(cp1, cp4, 
             ncol = 2,
             top = textGrob("fixed", gp = gpar(fontsize = 24)))
grid.arrange(cp1, cp5, 
             ncol = 2,
             top = textGrob("trans", gp = gpar(fontsize = 24)))
grid.arrange(cp1, cp6, 
             ncol = 2,
             top = textGrob("cartesian (to zoom)", gp = gpar(fontsize = 24)))
# install.packages("mapproj", repos = "https://cran.rstudio.com")
library(mapproj) ## coord_map needs mapproj
tx1 <- tx +
  labs(caption = "tx") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
tx1
tx2 <- tx + coord_map() +
  labs(caption = "tx + coord_map()") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
tx2
grid.arrange(tx1, tx2, 
             ncol = 2,
             top = textGrob("coord_map", gp = gpar(fontsize = 24)))

## Pie Charts

d2 <- subset(diamonds, color == "D")
cc <- qplot(color, data = d2, fill = cut)
cc

cc1 <- cc +
  labs(caption = "cc") +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cc1
  
cc2 <- cc + coord_polar(theta = "y") +
  labs(caption = 'cc + coord_polar(theta = "y")') +
  theme(plot.caption = element_text(size = 16, hjust = 0.5))
cc2

grid.arrange(cc1, cc2, 
             ncol = 2,
             top = textGrob("Pie Charts", gp = gpar(fontsize = 24)))


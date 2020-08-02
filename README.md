# low-budget-COVID19-tracking

Welcome!

Are you being bombarded by such an overwhelming amount of COVID19 information that your smol-brain-energy cannot process?

Are the web based COVID19 trackers just too mainstream and sleek for your style?

Are you itching for a gritty and laughably simple visualization tool to track the real-ish-time number of cases/deaths of the COVID19 pandemic both globally and domestically (US only)? 

Well then, you've come to the right place.

In a sleep deprived and caffine fueled mania I have managed to write a script to do just that! 

Introducing "covidfun.R".

The "covidfun.R" script comes with 4 simple data visualization commands:
```
covid.total()
covid.new()
covid.domestic.total()
covid.domestic.new()
```

The ```covid.total()``` and ```covid.new()``` commands will plot the number of total/new cases based on your country query so you can see how your wonderful country compares to the rest of the world. 

The ```covid.domestic.total()``` and ```covid.domestic.new()``` commands will plot the number of total/new cases based on your US state query so you can observe how piss-poor of a job your state is doing to control the spread. 

So, clone the repo, set your working directory, source the script, sit back and relax (and I do mean relax, this shit is not super-efficient) and watch the off-brand Disney magic unfold.

BUT FIRST, please make sure you install the following packages or this thing will refuse listen (much like the people refusing to wear a goddamn mask in public):
```
install.packages("ggplot2")
install.packages("gganimate")
install.packages("dplyr")
install.packages("gifski")
```

Thanks to https://covid.ourworldindata.org and https://data.humdata.org for making their data publicly available and frequently updated so my kludgy mess is at least accurate.   

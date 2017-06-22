#Visuals

ggplot(master.comb, aes(x=department)) + geom_bar() + facet_wrap(~reordered, ncol=1)

ggplot(master.comb, aes(x=department)) + geom_bar() + facet_wrap(~order_dow, ncol=2)

ggplot(orders, aes(x=order_dow)) + geom_bar() 

ggplot(orders, aes(x=order_hour_of_day)) + geom_bar() + facet_wrap(~order_dow, ncol=2)

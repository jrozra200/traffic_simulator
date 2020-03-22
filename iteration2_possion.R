## ITERATION 2: CAN I MAKE CARS INITIALIZE ON A MORE REASONABLE CADENCE

### ITERATION 1: INITIALIZING A CAR USING A UNIFORM DISTRIBUTION

library(ggplot2)
library(ggpmisc)

sim_time <- 60 * 60 # ONE HOUR, IN SECONDS
sec_since_last_car <- 0 # INITIALIZE VARIABLE @ 0 - WILL TRACK SECONDS SINCE 
                        # LAST CAR "LEFT"
prob_for_car <- 10 # WHAT IS THE CUTOFF FOR HAVING A CAR?
total_cars <- 0 # INITIALIZE VARIABLE @ 0 - HOW MANY CARS DID WE HAVE
all_cars <- data.frame() # LETS TRACK THE CARS AS THEY GO

## DO THIS WITHOUT A LOOP, I THINK... 

# DOES A CAR INITIALIZE THIS SECOND?
for(sec in 1:sim_time){
    # NO PROBABILITY ASSIGNED IF IT ISN'T AT LEAST 2 SECONDS
    prob <- rpois(1, 10)
    
    sec_since_last_car <- sec_since_last_car + 1
    
    if(prob >= prob_for_car) { # THIS IS NOT HOW THIS WILL WORK
        total_cars <- total_cars + 1
        
        car <- data.frame(car = total_cars,
                          time_launched = sec,
                          sec_since_last_car = sec_since_last_car)
        all_cars <- rbind(all_cars, car)
        
        sec_since_last_car <- 0
    }
}

mean(all_cars$sec_since_last_car)

my.formula <- y ~ x

ggplot(data = all_cars, aes(x = time_launched, y = car)) +
    geom_point() + 
    geom_smooth() + 
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE)

ggplot(data = all_cars, aes(x = time_launched, y = sec_since_last_car)) +
    geom_point() + 
    geom_smooth() + 
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE)

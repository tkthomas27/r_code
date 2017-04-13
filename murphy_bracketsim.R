# Murphy's NCAA Bracket Simulator

# http://www.murphyspunch.com/projects/ncaa-bracket-simulator/

##########################################################################

# create the log5 function which determines game winner
log5 <- function(power1, power2){
    (power1 - power1*power2) / (power1 + power2 - 2*power1*power2)
}

# create a 3D array to store the data
myarray <- array(0, dim = c(64,7,4))
# slots the 64 teams with unique identifiers
myarray[,1,1] = 1:64
# read pythagoreans from text on the web
myarray[,1,3] <- data.matrix(read.table(url("http://www.murphyspunch.com/pythag_2013.txt")))

# home court adjustments
myarray[1,2:3,4] <- .01
myarray[11,2:3,4] <- .01
myarray[23,2:3,4] <- .01
myarray[31,2:3,4] <- .01
myarray[33,2:3,4] <- .01
myarray[39,2:3,4] <- .01
myarray[49,4:5,4] <- .005
myarray[54,2:3,4] <- .005
# matrix to convert unique IDs to seed numbers
convert_seed <- matrix(c(1,1,2,16,3,8,4,9,5,5,6,12,7,4,8,13,9,6,10,11,11,3,12,14,13,7,14,10,15,2,0,15), nrow=16, ncol=2, byrow=T)
# sets total simulations
max_reps = 10000

# Module 1 holders: survival analysis

# array to store wins
cumwins <- array(0, dim = c(64,6))

# Module 2 holders: seeds advancing

total_seeds <- matrix(0, max_reps, 6)
one_seeds <- matrix(0, max_reps, 6)
two_seeds <- matrix(0, max_reps, 6)
three_seeds <- matrix(0, max_reps, 6)
four_seeds <- matrix(0, max_reps, 6)
five_seeds <- matrix(0, max_reps, 6)

# Module 3 holders: Conferences advancing

acc <- matrix(0, max_reps, 6)
atlantic_ten <- matrix(0, max_reps, 6)
big_ten <- matrix(0, max_reps, 6)
big_east <- matrix(0, max_reps, 6)
big_twelve <- matrix(0, max_reps, 6)
mvc <- matrix(0, max_reps, 6)
mountain_west <- matrix(0, max_reps, 6)
pac_twelve <- matrix(0, max_reps, 6)
sec <- matrix(0, max_reps, 6)
wcc <- matrix(0, max_reps, 6)

# Module 4 holders: total wins for prop teams
louisville <- matrix(0, max_reps, 6)
indiana <- matrix(0, max_reps, 6)
duke <- matrix(0, max_reps, 6)
florida <- matrix(0, max_reps, 6)
# start the master loop
reps <- 1
while (reps <= max_reps) {

    # Play a tournament
    j <- 1
    while (j <= 6) {
        i <-1
        while (i <= 2^(6-j)) {
            play_game = runif(1)
            # pass along winning team
            myarray[i, j+1, 1] <- if(log5(myarray[2*i-1,j,3] + myarray[2*i-1,j+1,4], myarray[2*i,j,3] + myarray[2*i,j+1,4]) > play_game) { myarray[2*i-1,j,1] } else { myarray[2*i,j,1] }
            # pass along that team's seed
            myarray[i, j+1, 2] <- convert_seed[,2][match(myarray[i,j+1,1]%%16,convert_seed[,1])]
            # pass along that team's home court adjusted pythag
            myarray[i, j+1, 3] <- if(log5(myarray[2*i-1,j,3]+myarray[2*i-1,j+1,4],myarray[2*i,j,3] + myarray[2*i,j+1,4]) > play_game)  { myarray[2*i-1,j,3] + myarray[2*i-1,j+1,4] - myarray[2*i-1,j,4] } else { myarray[2*i,j,3] + myarray[2*i,j+1,4] - myarray[2*i,j,4] }
            i <- i+1
        }

        j <- j+1
    }

    # Module 1: Count the wins
    j <- 1
    while (j <= 6) {
        i <-1
        while (i <= 64) {
            cumwins[i,j] <- sum(myarray[,j+1,1]==i) + cumwins[i,j]
            i <- i+1
        }

        j <- j+1
    }

    # Module 2: Store seed info

    # Count the sum of seeds
    total_seeds[reps,] <- apply(myarray[,2:7,2], 2, sum)
    # Count number of seeds advancing by seed
    one_seeds[reps,] <-  apply(myarray[,2:7,2], 2, function(x) sum(x==1))
    two_seeds[reps,] <-  apply(myarray[,2:7,2], 2, function(x) sum(x==2))
    three_seeds[reps,] <-  apply(myarray[,2:7,2], 2, function(x) sum(x==3))
    four_seeds[reps,] <-  apply(myarray[,2:7,2], 2, function(x) sum(x==4))
    five_seeds[reps,] <-  apply(myarray[,2:7,2], 2, function(x) sum(x==5))

    # Module 3: Store conference info
    acc[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==15) + sum (x==35)+ sum (x==51) + sum (x==63))
    atlantic_ten[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==7) + sum (x==24) + sum (x==37) + sum (x==52) + sum (x==57))
    big_ten[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==11) + sum (x==21) + sum (x==31) + sum (x==39) + sum (x==42)+ sum (x==49) + sum (x==61))
    big_east[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==1) + sum (x==14) + sum (x==19) + sum (x==29)+ sum (x==36) + sum (x==47) + sum (x==55) + sum (x==59))
    big_twelve[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==5) + sum (x==23) + sum (x==30) + sum (x==33) + sum (x==46))
    mvc[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==13) + sum (x==20))
    mountain_west[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==3) + sum (x==27) + sum (x==45) + sum (x==53))
    pac_twelve[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==6) + sum (x==25) + sum (x==41) + sum (x==54) + sum (x==62))
    sec[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==4) + sum (x==22) + sum (x==43))
    wcc[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==10) + sum (x==17))

    # Module 4: Store individual team info
    louisville[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==1))
    indiana[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==49))
    duke[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==15))
    florida[reps,] <- apply(myarray[,2:7,1], 2, function(x) sum(x==43))

    # next tourney

    reps <- reps + 1
}

# Some data outputs

# Sum of seeds in Final 4
hist(total_seeds[,4], breaks=36, density=50, col="darkorange3", xlim = range(4:40), main="Sum of Seeds in the Final Four (10,000 simulations)")
# Odds of x one seeds advancing
sum(one_seeds[,4]==0)/ max_reps
sum(one_seeds[,4]==1)/ max_reps
sum(one_seeds[,4]==2)/ max_reps
sum(one_seeds[,4]==3)/ max_reps
sum(one_seeds[,4]==4)/ max_reps

# Find total wins by 1 seeds compared to over/under
one_seeds_condensed <- apply(one_seeds,1,sum)
one_seeds_ou <- 12
sum(one_seeds_condensed < one_seeds_ou)/ max_reps
sum(one_seeds_condensed > one_seeds_ou)/ max_reps
sum(one_seeds_condensed == one_seeds_ou)/ max_reps

# Find total wins by 2 seeds compared to over/under
two_seeds_condensed <- apply(two_seeds,1,sum)
two_seeds_ou <- 9
sum(two_seeds_condensed < two_seeds_ou)/ max_reps
sum(two_seeds_condensed > two_seeds_ou)/ max_reps
sum(two_seeds_condensed == two_seeds_ou)/ max_reps

# 15 Seed advances
sum(two_seeds[,1] < 4)/max_reps

# 13 or 14 Seed advances
sum(three_seeds[,1] + four_seeds[,1] < 8)/max_reps

# 14 or 15 Seed advances
sum(three_seeds[,1] + two_seeds[,1] < 8)/max_reps

# 12 Seeds to advance (1 minus 5 Seeds)
sum(five_seeds[,1]==0)/ max_reps
sum(five_seeds[,1]==1)/ max_reps
sum(five_seeds[,1]==2)/ max_reps
sum(five_seeds[,1]==3)/ max_reps
sum(five_seeds[,1]==4)/ max_reps

# conference template
condensed <- apply(acc,1,sum)
ou <- 6.5
under <- sum(condensed < ou)/max_reps
over <- sum(condensed > ou)/max_reps
push <- sum(condensed == ou)/max_reps
under
over
push
summary(condensed)

# team template
team_temp <- apply(indiana,1,sum)
ou <- 3.5
under <- sum(team_temp < ou)/max_reps
over <- sum(team_temp > ou)/max_reps
push <- sum(team_temp == ou)/max_reps
under
over
push
summary(team_temp)
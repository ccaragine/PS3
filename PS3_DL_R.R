############################
# 1-3
door<-list(1)#define the structure of the door function (the players curtain choice)
class(door)<-"door" # list of value 1 or 2 or 3, depends on user input

PlayGame<-function(x,...){#We set up preliminary code for a function
  UseMethod("PlayGame")
}
PlayGame.door <-function(x,y){#We define the job of the function and assign it to our class "door"
  x<-sample(1:3,1)#We've defined x as a random number 1 through 3
  if(x==door[1]){# and if it's equal to the player's choice you win!!
    print("Congratulations!")
  } else { print("Better Luck Next Time!")}#Otherwise you lose
}

#Because we define these value inside the function we only have to run the function
  #but we could also leave them blank and input arguments into the PlayGame.door(x)
#Because the function reruns the whole code we play a unique game every time
  #(**Have issue with unique games in four***)
PlayGame.door()
#####################################################
#4
setClass(Class="door", #Here we define our class and slots
         representation=representation(
           x="numeric",#Stating the type of object which can exist in the class(a number)
           y="numeric"
         ),
         prototype=prototype(
           x=c(), #Proclaiming the form of the object (how many; list or matrix, etc)
           y=c()
         )
)

setValidity("door", function(object){#Here we perform a test to verify the logic of our class
  x<-object@x
  y<-object@y
  test1<-all(object@x==object@x)#If these two variables exist outside the defined 
  test2<-all(object@y==object@y)  #values (numeric) then return "wrong"
  if(!test1 & !test2){return("wrong")}
})
#Let's test if our logic is valid
test<-new("door",x="cat", y=1)
#[returns error BUT not the DEFINED "wrong", WHY?]

setMethod("initialize", "door", function(.Object,...){
  value=callNextMethod()#Preliminary method calling a metho
  validObject(value)#We tell it to cycle through the next method for command
  return(value)
})

setGeneric("PlayGame", #Here we do a preliminary set up of the function for "Let's Make a Deal"
           function(object="door"){
             standardGeneric("PlayGame")
           })

setMethod("PlayGame", "door",#Here we give the generic an actual argument to run
          
          function(object){#same as function in S3
            if(object@x==object@y){#except now we call on values defined by our created object
              return("Congratulations!")
            } else { return("Better Luck Next Time!")}   
          })
#I don't know how to overcome this problem so I do it manually
#Ideally I want to run the code game and input it into PlayGame()
  #so that everytime I run game I get a unique output
#HOWEVER, because I haven't figured that out the manual override would be
  #to set up multiple games
#[IF RESULTS LOOK SUSPICIOUS RERUN GAME1/2/3 AND PlayGame1/2/3 until you get unique values]

game1<-new("door", x=c(1), y=c(sample(1:3,1)))#Set up three games made up of 
game2<-new("door", x=c(1), y=c(sample(1:3,1)))  #x(players choice) & y(random curtain) 
game3<-new("door", x=c(1), y=c(sample(1:3,1)))



PlayGame(game1)#Here we play different iterations of the game
PlayGame(game2)
PlayGame(game3)


###############################
#> PlayGame(game1)            #
#[1] "Better Luck Next Time!" #
#> PlayGame(game2)            #
#[1] "Congratulations!"       #
#> PlayGame(game3)            #
#[1] "Better Luck Next Time!" #
###############################


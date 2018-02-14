####################################################################
#PS3
###################################################################
# 1-3
###################################################################
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
#####################################################

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
 if(is.numeric(object@x)==T){
    return()
  }else{
    return("Wrong")
  }

 })


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


#The only way we can test multiple iterations is to make seperate games 
game1<-new("door", x=1, y=c(sample(1:3,1)))#Set up three games made up of 
game2<-new("door", x=1, y=c(sample(1:3,1)))  #x(players choice) & y(random curtain) 
game3<-new("door", x=1, y=c(sample(1:3,1)))

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


####################################################################
#Class Assignment
####################################################################
#1-3

statsstudent<-function(name){#Here is our initial function
  courage<-sample(c(1:100), 1)#It randomly distributes characteristics to an unspecified name
  ambition<-sample(c(1:100), 1)
  intelligence<-sample(c(1:100), 1)
  effort<-sample(c(1:100), 1) 
  student <- list(name=name, courage=courage, ambition=ambition, intelligence=intelligence, effort=effort)
  class(student) <- "student"#As requested we assign this to class "student"
  return(student)#We are returned a list which have all the characteristics of the students and their name and class
}

dom<-statsstudent("dom")#two test functions
carter<-statsstudent("carter")

Sorthat<-function(x,...){#We set up preliminary code for a function to sort based upon the randomly dist. char.
  UseMethod("Sorthat", x)
}


Sorthat.student<-function(x){#Guts of the function
  charac <- c( x[[2]], x[[3]], x[[4]], x[[5]])#We can't call on a list in a function so we make a vector that contains the characteristics of interest
  if(charac[1]==max(charac)){ #Then we say if courage is highest assign Gryffindor
    class(x)<- c(class(x), "Gryffindor")#Then we give the student his class of student and concatenate that with his/her new assignment
    print("GRYFFINDOR!")# An exciting proclamation
    return(x)
  } 
  if(charac[2]==max(charac)){ #Then we say if ambition is highest assign Gryffindor
    class(x)<- c(class(x), "Slytherin")
    print("SLYTHERIN!")# A shitty proclamation
    return(x)
  } 
  if(charac[3]==max(charac)){#Then we say if intelligence is highest assign Gryffindor
    class(x)<- c(class(x), "Ravenclaw")
    print("RAVENCLAW!")# I used to think I was smart
    return(x)
  } 
  if(charac[4]==max(charac)){#Then we say if effort highest assign Gryffindor
    class(x)<- c(class(x), "Hufflepuff")
    print("HUFFLEPUFF!")#Every grad students assignment :/
    return(x)
  } 
}

dom_sorted<-Sorthat(dom)#Let's make sure this works
cart_sorted <- Sorthat(carter)#And is producing unique outputs

dom_sorted
cart_sorted




###########################################
#4
Gryffindor_Tower<-new.env()#Create the four environments
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()


curfew<-function(x,...){#We set up preliminary code for a function
  UseMethod("curfew", x)
}
#The problem below uses class(dom_sorted)[2] to define the second class which should be associated 
  #with where I have been sorted. Relying on that, I use if statements which say if your second
    #assigned class is "" then assign that object to the specified environment
class(dom_sorted)[2]
curfew.student<-function(x){  #assign evironment by schools
  name<-x$name
  if(class(x)[2]=="Gryffindor"){
    assign(name, "", envir = Gryffindor_Tower)}
  if(class(x)[2]=="Slytherin"){
    assign(name, "", envir = Black_Lake)}
  if(class(x)[2]=="Ravenclaw"){
    assign(name, "", envir = Ravenclaw_Tower)}
  if(class(x)[2]=="Hufflepuff"){
    assign(name, "", envir = Basement)}
}
#Use the function
curfew(dom_sorted) 
curfew(cart_sorted)
#Check location
ls(Gryffindor_Tower)  
ls(Black_Lake)
ls(Ravenclaw_Tower)
ls(Basement)

#But this crud doesn't work yet. Will Update


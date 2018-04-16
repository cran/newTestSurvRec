is.Survrecu <-
function(x) 
{xs<-x
 class(xs)
 if (class(xs)=="formula") {yy<-"TRUE"} else yy<-"FALSE"
 return(yy)}

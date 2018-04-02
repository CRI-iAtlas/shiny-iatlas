
concordanceIndex = function(predictions, observations){
	out = .Call("equalciR2C", predictions, observations)
	return (out); 
}

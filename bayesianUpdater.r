# we have an urn with k red balls and n-k green balls
# sample from the urn and use bayes theorem to find P(y_i) for i = 0..n where y_i is the event that there are i red balls

# here, k is chosen randomly and printed at the end of the function

#########

bup = function(num_samples,n) {

	k = sample(0:n,1)

	li0 = double(n+1)
	li1 = double(n+1)
	for(i in 1:(n+1)) {
		li0[i] = (n-(i-1))/n
		li1[i] = (i-1)/n
	}

	prior = rep(1/(n+1),n+1)
	par(mfrow = c(1,2))
	barplot(prior)
	
	# print prior
	#print(prior)
	
	for(i in 1:num_samples){
		# draw sample
		s = runif(1,0,1)
			
		# get correct likelihood function
		p = k/n
		if(s < p) l = li1
		if(s >= p) l = li0
		
		# do the updating
		pr.l = prior*l
		prior = pr.l/sum(pr.l)

		# print posterior
		#print(prior)
	}

	# print final results
	print(k)
	barplot(prior)
	return(prior)
}

bup(5,10)

factors :: Int->[Int]
factors n = [x|x<-[1..n-1], mod n x == 0] 

isprime :: Int->Bool
isprime n = length(factors n) == 1

primes :: Int->[Int]
primes n = [x|x<-[1..n-1], isprime x] 

occurance :: Char->[Char]->Int
occurance n f = length([x|x<-f, n == x]) 
pythia8toHEPEVT: 
		 g++ -O2 -ansi -pedantic -W -Wall -Wshadow -I$pyeight$/include pythia8toHEPEVT.cc -o pythia8toHEPEVT -L$pyeight$/lib/archive -lpythia8 -llhapdfdummy 


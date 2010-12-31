name = program

hc = ghc -Wall -O2
main = Main.hs
source = *.hs

$(name) : $(source)
	$(hc) --make -o $@ $(main)

clean :
	rm $(name) *.hi *.o


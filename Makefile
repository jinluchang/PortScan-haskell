name = port-scan
target = /home/frank/bin/port-scan

hc = ghc -Wall -O2
main = Main.hs
source = Main.hs CheckOpen.hs HostPortStatus.hs

all : $(name)

run : $(name)
	./$(name)

install : $(target)

$(name) : $(source)
	$(hc) --make -o $@ $(main)

$(target) : $(name)
	cp $(name) $@

clean :
	rm $(name) *.hi *.o

dist-clean :
	rm $(target) $(name) *.hi *.o


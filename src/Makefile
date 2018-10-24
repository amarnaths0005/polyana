#
# Copyright (c) 2015-2018 Vasilios E. Raptis <polyana.software@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
#-----------------------------------------------------------------------
#
#
# some variables
program    = ../bin/polyana
compiler   = gfortran
#options    =-cpp
options    =-cpp -funroll-loops
#options    =-cpp -fopenmp -fbounds-check -std=f2003 -Wall -Wextra
#options    =-cpp -fopenmp -fbounds-check -DSTREAMS
# use this if your compiler is not 2003-compliant: 
# options   =-cpp -DFORTRAN95
objects    = atoms.o groups.o molecules.o parameters.o polyana.o \
             process.o strings.o system.o utilities.o
modules    = atoms.mod groups.mod molecules.mod parameters.mod \
             process.mod strings.mod system.mod          
          
# the target for the executable
polyana: $(objects)
#	$(compiler) -o $(program) $(objects)  
	$(compiler) -o $(program) -fopenmp $(objects)  

# compilation of sources
$(objects): %.o: %.f90
	$(compiler) -c $(options) $< -o $@

# dependencies for the project files
atoms.o      :  parameters.o    
groups.o     :  molecules.o strings.o
molecules.o  :  atoms.o   
parameters.o :       
polyana.o    :  process.o    
process.o    :  system.o 
strings.o    :     
system.o     :  molecules.o groups.o strings.o
utilities.o  :
   
# target for local installation 
install: FORCE 
	@make 
	@make -s installme 
	
installme: FORCE 
	if [ -d ~/bin ]; then \
	    cp $(program) ~/bin/  ; \
	else                        \
	    echo "\v\tNo ~/bin directory found!\n\tExecutable can be found in ../bin\v" ; \
	fi 
	
# run the test cases
test: FORCE 
	make 
	@make -s testme 
	
testme: FORCE
	cp $(program) ../test/ ; \
	cd ../test; \
	echo -e "\v\tChdir to the tests directory"; \
	echo -e "\n\tRunning the tests now..."; \
	chmod +x clean ; \
	./clean ; \
	chmod +x run; \
	./run md polyana
    
clean: FORCE
	@make -s cleanme 
	
cleanme: FORCE  
	rm -f  $(objects) $(modules) ; \
	rm -f  $(tab2plot) tab2plot.o ; \
	rm -f  $(fixpot) fixpot.o ; \
	rm -f  $(mc) montecarlo.o ; \
	cd ../test ; \
	chmod +x clean ; \
	./clean     
	
# use empty rule instead of .PHONY to comply with non-GNU make 
FORCE:
	 

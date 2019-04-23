# POLYANA - A tool for the analysis of MD trajectories

#### Authors:

Christos Dimitroulis  
Theophanes E. Raptis  
Vasilios E. Raptis  
  
#### Contact: 
The POLYANA team  

    polyana.software@gmail.com  
  
**Version**        :   2.0.1  

**Release date**   :   April 23rd, 2019  

**Web site**       :   http://cag.dat.demokritos.gr/Software.mol.php  


## Contents:
1.  Introduction  
2.  How to build and test POLYANA  
3.  How to run POLYANA - the simple way  
4.  How to work with directives  
5.  How to work with groups of atoms  
6.  How to process simulations on the fly (and why)  
7.  How to use the accompanying utility  
8.  Future goals  
9.  References  
10. Legal stuff, etc.  
                   

## 1. Introduction
This  file  contains information  about 'POLYANA',  a program  that reads output 
created by DL\_POLY Molecular Dynamics suite ('Classic' version) [1] and returns 
radial distribution functions  for centres of mass  of groups of atoms  or whole 
molecules. It has been designed with simplicity of use in mind: just press Enter 
and the program  will read DL\_POLY input  and output files  (CONTROL, FIELD and 
HISTORY) figure out what species are there in the simulation box and compute all 
radial distribution  functions  for pairs of like  and unlike molecular species. 
Also, the advanced user will be happy to find  that  POLYANA's functions  can be 
controlled and enhanced  with the aid of simple directives  placed at the end of 
DL\_POLY's CONTROL file.  

POLYANA is suitable  for systems of 'small' molecules  or groups of atoms,  i.e. 
such that their size doesn't exceed half the shortest simulation cell dimension. 
However,  POLYANA is expected to work correctly  even for large branched species 
or star-like polymers and the like,  provided the simulation cell is much larger 
than the molecules' or groups' dimensions  - the only case  one would be  really 
interested in g(r) of such large species. Indeed, tests carried out with the aid 
of companion utility verify_unfold.f90 as described in the relevant README file, 
show that POLYANA can treat *arbitrary* topologies.  

### What's new in this version
In a nutshell:

* Calculations  not restricted to molecular centres of mass,  but also extending 
  to user-defined groups of atoms.  

* Radial distribution functions  calculated over distances  well beyond half the
  simulation cell length.  
  
* Stream processing or 'on the fly' mode.  

* OpenMP parallelism.  

* Minor bugfixes and improved input facility<sup>[1](#fn1)</sup>.  

POLYANA's last version [2] was mainly intended for radial distribution functions 
of molecular *centres of mass*.  However,  it was possible  to 'hack' POLYANA by 
appropriately editing the atoms definitions in the DL\_POLY's FIELD file to mask 
certain  atom types  and compute g(r)'s  for groups of atoms  rather than  whole 
molecules.  The current version [3] provides an explicit mechanism to let users 
define groups of atoms  by inserting  a definition  with the appropriate syntax 
in FIELD and a novel directive  (*group*) in CONTROL file;  DL\_POLY will simply 
ignore any such definition in FIELD.  As regards the distance  over which radial 
distribution functions extend, POLYANA relies on an observation by Theodorou and 
Suter [4]  so as to extend  the calculation beyond  half the shortest simulation
cell length  by computing  the correct differential volume  with distance.  As a 
means of tighter integration with DL\_POLY,  POLYANA  can be combined with it in 
the style of Unix pipes to allow 'on the fly' processing  of MD trajectories (to 
do this, DL\_POLY has to be minimally modified and rebuilt  so that it redirects 
trajectory data to standard output  instead of writing a HISTORY file). Finally, 
POLYANA allows multithreaded (OpenMP) parallel execution. Users can compare time 
measurement displayed with the end of the calculations to check performance with 
number of threads.  


### Requirements
The below list (as well as most part of this document) is relevant to UNIX/Linux 
systems, but it should be fairly easy to port the project to other environments;
to build and run POLYANA, one needs: 

*  gfortran or other modern Fortran compiler  (please, edit Makefile as needed). 
   If your compiler  does not comply with Fortran2003 standard,  use alternative 
   option indicated in the Makefile. 
   
*  make; POLYANA is a small project  and one can try and compile the source code 
   manually. If make is available, it will just make your life a little easier.
   
*  Since this is a post-processing tool,  DL\_POLY itself is not needed to build
   POLYANA unless you want to run the tests - see Section 2. Of course, you need 
   DL\_POLY output files to process! 

*  bash shell; the *bash* scripts in test directory have worked on various Linux 
   platforms.The Fortran code itself should compile and run on any platform with 
   an appropriate compiler.

*  Finally, the *rdfplot* companion script calls *gnuplot* to plot output files.
   This is not a strict requirement;  POLYANA output is very simple so users can
   employ their favourite software to generate r.d.f. graphs.

## 2. How to build and test POLYANA

### Compilation 
This is straightforward. Just type

    make

and press Enter.  An executable called *polyana*  will be generated in the *bin* 
subdirectory of the project folder; you can move it to any location specified in 
your $PATH environment variable.  To build POLYANA *and* run the tests in *test* 
subfolder, type:

    make test

This will copy the executable to the test directory and launch a script therein. 
*It is assumed that  DL\_POLY's executable is  DLPOLY.X*,  as defined in its own 
Makefile  - otherwise,  one has to edit *run* script in test directory.  You can 
also run 'make', do whatever you want and run 'make test' later. Read more about 
the tests in Section 6 of this file.  

### Running the tests

NOTE: it is assumed that the name of DL_POLY's executable is DLPOLY.X as defined 
in the Makefile of its distribution.  

As mentioned in Section 1, the simplest way to run the tests is by running 'make 
test'.  Normally, POLYANA prints some messages on the screen.  During the tests, 
POLYANA will run in the background and all its messages  will be redirected to a 
file named 'polyana.out', in each test subdirectory. The g(r) output, of course, 
will be found in the corresponding RDF files (see next Section).  

Alternatively, you can go to the test directory and launch the *run* script from 
there, assuming POLYANA executable is already therein (and DL\_POLY is installed 
in your system!), so if you haven't yet,  copy POLYANA to test now.  To run the 
*run* script you must supply some arguments. Here is how:  
 
    ./run   [no arguments]

or  

    ./run h

or  

    ./run -h  ...........   will display a help screen and exit 
    
    ./run md  ...........   will run DL_POLY from the beginning in each
                            test subdirectory, after deleting whatever
                            dl_poly output files already present. 
                            
    ./run polyana .......   will run polyana from the beginning in each 
                            test subdirectory, after deleting whatever
                            polyana output already there. If no DL_POLY
                            output to process can be found in a certain
                            subdirectory, the script will print a messag
                            e and continue with the next subdirectory. 
                            
    ./run md polyana

or  

    ./run polyana md ....   will run DL_POLY and then, polyana in each
                            test subdirectory. This is the option invoke
                            d when running 'make test'. 

The last -and most obvious- choice is to change to any test subdirectory and run 
DL\_POLY and POLYANA 'manually'.  

Finally, to 'clean' the test subdirs i.e. remove all DL_POLY and POLYANA output, 
just run the *clean* script  (after chmod-ing to executable)  from the test dir. 
This is also invoked when you run 'make clean' from the source directory.  

## 3. How to run POLYANA - the simple way 
To calculate *molecular* radial distribution functions,  just run the executable 
in the directory  of your MD run  and the program will read FIELD  to figure out 
what molecules are there in the system; CONFIG to retrieve the periodic boundary 
conditions key (if this is missing  from the trajectory); and HISTORY to analyse 
the trajectory. With the end of processing, two files named RDF (*not to confuse 
with DL_POLY's RDFDAT!*) and POP, will be created. 

In RDF, the centre-of-mass radial distribution functions are given for all types 
of molecules, numbered 1, 2, 3, ... according to the order they appear in FIELD. 
E.g, if water and ethanol molecules appear in FIELD in that order, then 1=water, 
2=ethanol, and the columns 1-1,  2-2 and  1-2  for the respective g(r) functions 
will be printed in RDF.  The default values of 0.1 Angstroem  and 12.5 Angstroem 
will be used for the pair distance bin  and maximum distance, respectively - see 
next Section on how to change these values.   

In POP (standing for 'populations'), the number of type 'b' molecules around the 
average type 'a' molecule with distance,  will be given in a similar arrangement 
as the columns in RDF.  These numbers can also be obtained by integrating radial
distribution functions  - actually, rho\*dV\*g(r) -  with distance.  However, we 
don't have to carry out the integration; POLYANA will do it for us.  Unlike RDF, 
columns ab and ba of POP are not equivalent.  Therefore, in the case of a binary 
mixture, columns 1-1, 2-2, 1-2 and 2-1 can be found in the respective POP file.  

Please, note that POLYANA will process the trajectory even if the simulation has 
not been completed  - in that case a message will be emitted with the end of the 
computation letting us know  that the trajectory file was found to be abnormally 
terminated  - this would not prevent POLYANA from computing and printing results 
in RDF and POP. This is an attractive feature in that it allows users to look at 
the structure of their systems without waiting for the simulation to be over.   

Another sign of POLYANA's robustness  is the way it handles input files. To read 
periodic boundary conditions, POLYANA will look for HISTORY's appropriate header 
line. If that line is missing,  POLYANA will not complaint; it will look for the 
PBC code in CONFIG.  In the improbable case that  CONFIG too is missing, POLYANA 
will set periodic conditions to parallelepiped,  a quite common and generic type 
of simulated periodic systems, and will print a message letting the user know of
the situation and how periodic conditions can be redefined  as desired.  But how 
can the user do that? Time to read the next section. 

## 4. How to work with directives
POLYANA can read directives placed in CONTROL file,  after the 'finish' DL\_POLY 
directive,  to control its execution  and benefit from more of its capabilities. 
All POLYANA directives are listed below in alphabetical order:  

    Directive     | Description
    ---           | ---
    dr *d*        | Distance bin for the histograms in g(r) calculations
    end polyana   | Marks the end of a section of POLYANA directives
    every *n*     | Calculate every n-th step (time saver for long trajectories)
    group [total] | Compute g(r) for user-defined groups rather than molecules
    omp  *n*      | Sets the number of OpenMP threads
    pbc  *n*      | Sets periodic boundary conditions to one of the following:
        0         | no pbc's 
        1         | cubic
        2         | orthorhombic
        3         | parallelepiped
        6         | slab
    polyana       | Marks the beginning of section containing POLYANA directives
    rmax *r*      | Maximum distance for g(r) calculations
    smooth        | Smooth g(r) as in Allen & Tildesley, 1989, pp. 203-204 [5]
    start *n*     | Skip steps 1 to n-1 and process from n-th and beyond
    stop  *n*     | Skip (don't process) configurations beyond the n-th
    threads *n*   | A synonym for 'omp'
    total         | Optional argument of *group* to include intramolecular pairs
    width         | A synonym for 'dr'

Launching the every *n* command will cause POLYANA to compute pair distances and 
update histograms for every *n*-th configuration only.  This can be helpful when
processing very long trajectories.  The pbc directive, on the other hand, can be 
of use when both HISTORY's header and the CONFIG file are missing;  otherwise it 
will be overriden  by the pbc key therein - the key with higher precedence being 
the one in the HISTORY file.  

POLYANA directives are case insensitive:  start, START and Start are equivalent. 
Any number of spaces  can be inserted before a directive or  between a directive 
keyword and its numerical argument.  The *polyana*  and *end polyana* lines must 
exist and enclose the other lines  if directives are to be used.  If some or all 
directives are missing (see: 'run the simple way', Section 2) the default values 
will be used instead; same goes for missing directive arguments (all of them are 
optional). All default values are summarised below:  

    Directive     | Default value
    ---           | ---
    dr            | 0.1 [Angstroems]
    every         | 1
    group         | .FALSE.
    omp           | system-dependent
    pbc           | NOTHING (a value that is unrelated to any PBC code number)
    rmax          | computed automatically based on cell dimensions
    smooth        | .FALSE.
    start         | 1
    stop          | HUGE(integer) (exceeding any reasonable nr of steps)
    total         | .FALSE.

Finally, directives can be 'commented out' using the hash (#) character.  

The maximum distance, *r*\_max, is not assigned a default value.  Instead, it is 
calculated automatically based on the simulation cell size.  In the general case 
of a parallelepiped cell, this is done as follows:  If **c**1, **c**2 and **c**3 
are the three cell vectors, then, three heights can be defined like so:  
  
   *h*\_i = | **c**\_i . (**c**\_j x **c**\_k) / || **c**\_j x **c**\_k || |  
  
where indices i, j and k denote circular shifts of {1, 2, 3}.  Then, the maximum 
distance is defined as  
  
   *r*\_max = 0.5\*min{*h*\_1, *h*\_2, *h*\_3}  
  
In the case of cubic simulation cells,  the above expression reduces to half the 
cell size. However, in the particular case of cubic periodic conditions, POLYANA 
will extend by default the range of g(r) to L*sqrt(3)/2,  L being the cell size; 
of course this can be overriden  with the aid of the rmax directive. The user is 
referred to the previously cited paper of Theodorou and Suter for a thorough and 
in depth discussion of the topic.  

POLYANA reads the first cell in the trajectory file, computes the number of bins 
by dividing the above computed rmax by the bin width and allocates memory to the 
histogram arrays used in RDF calculations. If the trajectory was computed in the 
NPT ensemble where the simulation varies with time POLYANA will keep reading all 
subsequent cell vectors and with each new step,  *r*\-max will be updated as the 
minimum of its current and new value ( thus discarding a few elements at the end 
of the histogram arrays).  

### Example of using directives
Suppose we ran a MD simulation and saved 6000 configurations. Of them, the first 
1000 steps form the equilibration stage so we won't process them. Also, the last 
1000 will not be processed,  for whatever reason.  Our g(r) will be computed for 
distances up to 10 Angstroems and the bin to be used will equal 0.25 Angs. Then, 
the POLYANA section in the CONTROL file should look like this:  

    ...  
    [various DL_POLY directives]  
    ...  
    finish [end of DL_POLY section]  
      
    polyana   
        start   1001  
        stop    5000  
        rmax    10.0  
        dr       0.25  
    end polyana  
  
Indentation as above is not compulsory; it is used for the sake of readability.  

## 5. How to work with groups of atoms  

### Explicit definition of groups
Often we are interested in looking at specific groups of atoms rather than whole 
molecules. Suppose, for instance, we have simulated a system of water mixed with 
n-butanol, using suitable united-atom models. Then, we would like to look at the
way water molecules are arranged around the hydroxyl group and alkyl tail of the 
alcohol. To address problems of this kind, we have to do the following:  

1. Add *group* directive in *polyana ... end polyana* section of CONTROL file.

2. Optionally, add *total* directive  to take intramolecular pairs into account;
   otherwise, results will reflect only intermolecular group-group interactions. 
   
3. Add group definitions in the ATOM directives of FIELD file that correspond to 
   the molecules to be broken down into groups.
   
Group definitions obey the following general syntax:  
        
        (...(char int int) [...(char int int)] int ) [(...)]
        
where outter square brackets, [...], indicate optional arguments. In particular, 
a molecule is divided into groups, defined like so:  

                           (grouptype nat  nrep) 
        
where *grouptype* is an 8-character string, *nat* is  the number of atoms in the 
group and *nrep* is the number of times the group is repeated along the array of 
the atoms that comprise the molecule.  

Groups of different types can be grouped themselves to form larger 'supergroups' 
which can be  repeated many times,  as  for instance in the case of co-polymers. 
Then,  definitions are enclosed in parentheses with the number each 'supergroup' 
is repeated, placed at the end, such as:  
        
                         ( (A 3 1) (B 2 1) 100)
         
This pattern can be applied recursively  to define large structures of arbitrary 
complexity. Group definitions are placed in the ATOM directives of corresponding 
molecular types in FIELD file, after the number of atoms, like this: 
        
                    ATOMS int group-definitions
        
If a molecule is to be divided into groups,  these must be defined such that all 
atoms belong to one of the groups and the sum of atoms in all repeated groups be 
equal to the argument of the ATOM keyword.  
         
It is reminded that *group* directive should also be inserted in CONTROL for the
above definitions to take effect, otherwise they will be ignored so POLYANA will
fall back to its default behaviour and calculate molecular functions. Lastly, it
is noted that *group* admits an optional argument, namely *total*, to modify its 
function. Without it, intermolecular radial distribution functions are computed; 
otherwise, all group pairs, whether intra- or intermolecular, will be taken into 
account. In the latter case,  bonded pairs will give rise to sharp peaks typical 
of bonds, bond angles and dihedral angles - if they exist.  This is not the case 
with DL\_POLY which excludes bonded atom-atom pairs from similar calculations in 
its RDFDAT output files.  

#### Examples: single compounds
        
1. United-atom n-hexane as a trimer:
        
          ATOMS 6  (terminal 2 1) (midsegm 2 1) (terminal 2 1)
        
2. United-atom n-hexane as a dimer: 
        
          ATOMS 6          (bead 3 1) (bead 3 1)  
          
    or  
    
          ATOMS 6              (bead 3 2)  
        
3. United-atom n-dodecane as a tetramer:
        
          ATOMS 12             (bead 3 4)  
        
4. Copolymer A-(B2C3)100-A where consecutive Bs form a bead and Cs form another:
        
          ATOMS 502 (A 1 1) ((B 2 1) (C 3 1) 100) (A 1 1)

Finally, we note that when *nrep* equals one, it can be omitted altogether; then 
the relevant above examples can be rewritten as follows:  

    1.    ATOMS 6  (terminal 2) (midsegm 2) (terminal 2)
    2.    ATOMS 6          (bead 3) (bead 3) 
    4.    ATOMS 502 (A 1) ((B 2) (C 3) 100) (A 1)

#### Example: mixtures
        
Let's take a real-life example and assume we are using the well-known TraPPE [6]
and SPC/E [7] force fields to model a n-butanol-1/water system so the FIELD file 
looks more or less like this:  

    ...  
    MOLECULES      2  
    Butanol  
    NUMMOLS ...  
    ATOMS 6
        CH3H        15.0344         0.0000    1  
        CH2B        14.0336         0.0000    1  
        CH2B        14.0336         0.0000    1  
        CH2A        14.0336         0.2650    1  
          OC        15.9996        -0.7000    1  
          HC         1.0008         0.4350    1  
    ...  
    ...  
    FINISH  
    SPCE Water  
    NUMMOLS ...  
    ATOMS 3  
        OW      15.9996  -0.8476  
        HW       1.0080   0.4238  
        HW       1.0080   0.4238  
    CONSTRAINTS 3  
        1    2   1.0000  
        1    3   1.0000  
        2    3   1.63298  
    FINISH  
  
Now, suppose we want to compute g(r) for water and butanol's hydroxyl group and 
alkyl tail. Then, the relevant ATOM records will be modified as follows:  

    Butanol  
    NUMMOLS ...  
    ATOMS 6     (alkyl 4) (hydroxyl 2) 
    ...
    ...
    FINISH
    SPCE Water  
    NUMMOLS ...  
    ATOMS 3     (water 3) 
    ...
    ...
    FINISH  

where we are allowed to ommit *nrep* because it equals 1 in all cases. Note also 
that since POLYANA computes molecular functions by default and water is taken as 
single entity, the corresponding group definition, (water 3), is redundant. Once 
again, it is reminded that the *group* directive should be present in CONTROL or 
the group definitions will be ignored.  

### Implicit definition of groups (or how to hack FIELD)
The above method works fine when atom types are arranged in FIELD such that user
can readily group them into successive subunits of interest. However, some cases 
(e.g. complex molecular topologies or other restrictions) may require atom lines 
of the same subunit of interest to be placed far apart from each other in FIELD. 
Then, our explicit declaration scheme does not work and we have to resort to the 
the 'hacking' strategy employed with the previous POLYANA version. Let's outline 
the procedure using the last above presented example. To calculate the hydroxyl-
water g(r) we rewrite the butanol lines as follows:  

    CH3H         0.0            0.0000    1
    CH2B         0.0            0.0000    1
    CH2B         0.0            0.0000    1
    CH2A         0.0            0.2650    1
      OC        15.9996        -0.7000    1
      HC         1.0008         0.4350    1

Likewise, to calculate g(r) and density profile for the alkyl tail, we rewrite:  

    CH3H        15.0344         0.0000    1
    CH2B        14.0336         0.0000    1
    CH2B        14.0336         0.0000    1
    CH2A        14.0336         0.2650    1
      OC         0.0           -0.7000    1
      HC         0.0            0.4350    1

Of note, this trick calculates **inter**-molecular radial distribution functions 
of user-defined groups  while results obtained by *group*  combined with *total* 
argument, include all such pairs in the system. For a thorough discussion on the 
topic, the reader is referred to our first publication introducing POLYANA  (see 
references, Section 10).  As another example, assuming that we look at geometric 
centres rather than centres of mass of the molecules,  we simply set atom masses 
equal to one and the same value for all atom types.  

## 6. How to process simulations on the fly (and why)  
DL\_POLY computes atomistic radial distribution functions during MD simulations. 
It is tempting to render POLYANA capable of exchanging information with DL\_POLY 
'on the fly' and computing molecular (or atom group) r.d.f.'s during simulations 
instead of waiting for them to be over. A solution in the form of Unix pipes has 
been implemented to this purpose; to make it work, a few minor modifications are 
required, namely:
  
1.  DL\_POLY: replace HISTORY file by standard output and rebuild. 
  
2.  POLYANA: edit Makefile to include the -DSTREAMS compiler option and rebuild. 

In this way, DL\_POLY will send all trajectory records  to standard output while
POLYANA will be reading from standard input and stream again to standard output;
thus, the two applications can be combined as follows:  

        DL_POLY.X | polyana > HISTORY 

Apart from obtaining post-processing results immediately, this solution can also
be of great convenience when disk space restrictions are very tight, by allowing 
direct compression of the trajectory:  

        DL_POLY.X | polyana | gzip -c > HISTORY.gz 

Of note, when in stream mode, all POLYANA messages otherwise appearing onscreen, 
are now redirected to a file named STDOUT.  

The required modifications are very easy to implement:  

#### DL\_POLY Classic  
In file *setup_module.f* we set parameter *nrite* from 6 to some other value not
associated with another file unit, e.g. 

        C      integer, parameter :: nrite = 6
               integer, parameter :: nrite = 11  

Then, we set parameter *nhist* to 6:  

        C      integer, parameter :: nhist = 21  
               integer, parameter :: nhist = 6  

In files  *traject.f* and *traject_u.f*  we comment out the statements that open 
and close the HISTORY file:  

                 ...  
        C        open(nhist,file='HISTORY',position='append')  
  
or

        C        open(nhist,file='HISTORY',form='unformatted',position='append')  
                 ...  
        C        close (nhist)  
                 ...  
  
Then, we rebuild DL\_POLY. The new executable should be printing HISTORY records 
to the screen. If it doesn't work as expected, try *make clean* and rebuild.  
    
#### POLYANA 
The source code has been extended to be able to read HISTORY records in the form 
of incoming byte streams.  All we have to do is edit the compiler options record 
in Makefile to include the appropriate preprocessor flag,  
  
        #options    =-cpp -fopenmp -fbounds-check  
        options    =-cpp -fopenmp -fbounds-check -DSTREAMS  
  
and recompile.  The new executable should be reading HISTORY records piped to it 
and print them to screen. If it doesn't, then try *make clean* and rebuild.  

## 7. How to use the accompanying utility
RDF files are easy to plot.  To facilitate this task even further, a bash script 
is provided,  which generates a gnuplot script automatically  and then, launches 
gnuplot to run that script and plot the RDF file. The 'rdfplot' script takes two 
command-line arguments:  the number of RDF column pertaining to the species pair 
of interest, and an upper bound to the y-axis range. 
Thus: 

        ./rdfplot   2   1.5 
    
means that the second column of RDF  (pertinent to the first pair of species) is 
to be plotted with respect to distance (first column) and the y-scale will range
from 0 to 1.5. Of course, the generated gnuplot script, rdfplot.gp, can be modi-
fied by the users as they see fit.  
 
## 8. Future goals 
Apart from the above presented functionality,  many new features are coming with 
future versions. Some of them have already been implemented and are being tested 
for bugs, efficiency and so on, while others have been designed and soon will be 
added to POLYANA's quiver. The most important new functions include  

1. Mean Square Displacement (MSD).  Once molecular or group positions are known, 
   it is trivial to use them in calculating MSD.  

2. Reorientation dynamics of groups and molecules. When computing pair distances 
   to determine the corresponding r.d.f.'s, we store group-group vectors at each
   time step. These can serve to compute time-average 1st and 2nd-order Legendre 
   polynomials,  P1(t; cos a),  P2(t; cos a), where *a* denotes the angle formed 
   by instances of such a vector at time steps t0 and t0+t. These results can be
   linked to data from spectroscopy.  
   
3. In a similar vein,  we can read atom velocities from HISTORY, calculate group 
   or molecular velocities and compute their autocorrelation functions. 
   
4. Potentials of Mean Force. Molecular radial distribution functions can be used 
   to define free energy difference, A-A0=-kB\*T\*ln g(r), where kB and T denote 
   Boltzmann's constant and temperature, respectively. Such an expression can be
   stored in the style of DL\_POLY's TABLE files  and serve as pair potential in 
   coarse-grain simulations. This is currently tested for molecules; group-group 
   interactions require also bonded potentials that are not in place yet.  
   
5. Extend *group* syntax so it can handle system definitions in which records of
   atoms to group together in FIELD, are not consecutive. This feature is a work 
   in progress.  
 
## 9. References 
1. W. Smith, T. Forester *J. Mol. Graph.*, **1996**, *14*, 136.  
2. C. Dimitroulis, T. Raptis, V. Raptis *Comp. Phys. Commun.*, **2015**, *197*,
   220-226.  
3. V. Raptis, C. Dimitroulis, T. Raptis *Mol. Simul.*, **2019**, 
   DOI: 0.1080/08927022.2019.1603379  
4. D.N. Theodorou, U.W. Suter, *J. Chem. Phys.*, **1985**, *82*, 955.  
5. M.P. Allen, D.J. Tildesley, Computer Simulation of Liquids, Oxford University 
   Press, Oxford, 1989.  
6. B. Chen, J. Potoff, J. Siepmann *J. Phys. Chem. B*, **2001**, *105*, 3093.  
7. H. Berendsen, J. Grigera, T. Straatsma *J. Phys. Chem.* **1987**, *91*, 6269.  

## 10. Legal stuff, etc.
Polyana is a program for computing molecular pair distribution functions.  

### How to cite
Please refer to our most recent publication, number 3 in previous section.  

### License

Copyright (c) 2015-2019 Vasilios E. Raptis <polyana.software@gmail.com>  

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:  

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.  

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE.  

### Download
The source code for this program can also be downloaded from here:  

    http://cag.dat.demokritos.gr/Software.mol.php

### Contact
Having problems or questions about this program? You are welcome to contact us: 

    The POLYANA team
    polyana.software@gmail.com

<pre>  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
</pre>  
---

<a name="fn1">1</a> Bugfixes:  

*   Unlike RDF, columns a-b and b-a in POP file are not equivalent; both of them 
    are printed now.  

*   Issue concerning occasional crashes when reading FIELD file, has been fixed; 
    subsequent modifications ensured  and tests verified robust performance when 
    reading input files and resilience in cases of missing files or data.  
    
*   With *every n* directives, *n* must be taken into account when averaging the 
    histograms and cell volume.      
    
    
<pre>  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
</pre>  
    

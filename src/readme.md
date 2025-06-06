Hints for compiling / debugging:

1) create a folder "build" (at top level)

2) run cmake in that folder using: cmake ..
This creates DikesOvertopping.sln for your combination of Visual Studio and Intel Fortran.

3) run the unit tests before committing to the trunk
Preferred work flow is create a branch and make a pull request

4) issues are at: https://issuetracker.deltares.nl/projects/OVERS

5) automated tests run at: https://dpcbuild.deltares.nl/project/VtvInstrumentarium_Kernels_DikesOvertopping?branch=&buildTypeTab=overview&mode=builds

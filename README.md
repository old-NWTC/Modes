# Modes

The original code comes from [NWTC Modes v2.22](https://nwtc.nrel.gov/modes) (2002). This code has been updated to build using current compilers.

## Building the code
### Windows Visual Studio
A Visual Studio project file is located in [vs-build/Modes/Modes.sln](vs-build/Modes/Modes.sln)

### CMake

```
git clone <repository_url> modes  # Clone from public repo
cd modes/build                    # Change into the build directory 
cmake ../                         # Use cmake --help for help in choosing a specific build-system generator 
make                              # Build sources
make install                      # Optionally install in the modes/install/bin directory

```

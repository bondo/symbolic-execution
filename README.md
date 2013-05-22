Symbolic execution for python
=============================

This project is about implementing a "symbolic execution" tool for Python 3.

There are 3 directories:

* `report/`: Contains the report discribing the project and symbolic execution in general.
* `transformer/`: Contains Haskell code that prepares the Python code for symbolic execution.
* `instrumentation/`: Contains the python code that does the actual symbolic execution.


How to use
----------
* Install Z3 from Microsoft. Pick the unstable version, as the stable one is not yet compatible with Python 3.
* Install the Haskell platform, and whatever packages are needed to compile `transformer/Main.hs`.
* If you want to specify your own input file, change the filepath, function name and number of arguments in `instrumentation/instrument.py`.
* Run `instrumentation/instrument.py`.

The scripts directory contains scripts that you might find useful.

The "normal" place for these files is C:\ZKL\Scripts (or
  C:\ZKL\Src\Scripts or ~/Scripts) and the default System.classPath
  points there.  This makes something like "zkl md5 foo" work as
  expected.  You can modify $zklClassPath if you want to put the scripts
  somewhere else.

calendar : Print a one year calendar. Very basic. 
	zkl calendar <year>
	zkl calendar <year> <title text>

hexDump : Print a "debug" like hex dump of a file
	zkl hexDump fileName
	eg zkl hexDump Bin/zkl.exe

instructionProfiler : Print counts for instructions in a compiled object.
	zkl instructionProfiler file.zsc | vault path
	eg zkl instructionProfiler Compiler.Compiler
	   zkl instructionProfiler Built/startup.zsc

manifest : The program I use to back up and/or create the package zip
	files.
	See "manifest" in this package for an example.
	This package was created with: zkl manifest --zip Scripts/manifest
	Not documented.

md5 : Print the MD5 hash for one or more files
	zkl md5 file ....
	$ zkl md5 zkl_scripts.zip 
	zkl_scripts.zip:
	  2016-08-04; 15,411 bytes; MD5: afa0b13132f24d31a51986c51fcd1696
	Can use other hashes: --256 for SHA-256
	   Requires zklMsgHash DLL

sinebar : Useful for machinists doing precision work with gage blocks.
	Calculates the gage blocks need to make a stack, a sine bar
	stack or two stacks of the same height. Uses an 81 piece inch gage
	block set.

zgrep : Kinda like "find | xargs fgrep" for those of us who don't have
        find (Windows). Search for files containing a pattern. Will
	recursively search. Case sensitive. Fixed (ie not RE) search
	pattern.
	   zkl zgrep -R Src .glob *.zkl


extractTable.zkl, gperf.zkl : Two programs used for building gperf hash
	tables from C code.  Used when writing zkl object in C.

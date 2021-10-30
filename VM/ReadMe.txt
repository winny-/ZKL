The read me for zkl source code.
Unix: Read the Makefile as that is where I update build into.

zkl is a object oriented programming language. Visit
http://zenkinetic.com for manuals and other goodies.

zkl is freeware and is covered by the zlib license (see license.txt).

Building:
zkl is written entirely in C89. I use some C99 features: "//" comments
and a couple of "inlines". Some of the extension libraries are written
in C++.

Windows
-------
I only support Windows 10.  Use Microsoft's VC++ Express Edition (free,
no resources). Download from Microsoft.

Double click on vm_VC14.sln, compile/build, F5 to run tests (if you have
downloaded them).

Arghs!:
  - VC14 to VC2017: If your VC says you need to migrate, you'll probably
    also need to update the project properties (why oh why): Alt-F7, all
    configurations:
    - Debugging: Working Directory: Remove trailing ..
      $(ProjectDir)\..\  -->  $(ProjectDir)..
    - Build Events: Post-Build Event: Put quotes around $(OutDir),  my bad.
      call .\postBuild.bat "$(OutDir)"
      I think I've fixed all these.



Linux/FreeBSD/PC-BSD/clang
--------------------------
Read the Makefile.
Really.

There are two ways to build:
- Stand alone. The default. Builds an single executable that just runs.
- Shared library ("make lib").  You will need to build this one if you
  want to use loadable shared libraries (zlib, editline, GMP, GSL, CURL,
  YAJL (JSON), etc).  At least until I figure out how to get a shared
  library to back link to zkl (instead of libzkl.so).
  Edit and use the runzkl shell script (read and maybe edit).
The one huge plus to using the shared library is zklEditLine, which
gives you command line editing (see LibSrc/EditLine).

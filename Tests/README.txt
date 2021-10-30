This file: Tests/README.txt
//the above line used for testing, don't change

These directories contain the test suites for the zkl system.
Currently, there are over 6,700 tests in 43 files.

Unpack these tests to */Tests (the usual place being /ZKL/Tests), such
that this file is Tests/README.txt.

CD to Tests/.. to run the tests

Note:  the socket tests (Tests/Object/socket.zkl) open a socket to
localhost (ie the machine it is running on) on port 123456.  This will
probably tickle your firewall.  The test checks to see if a socket can
talk to itself.  It does not leave the local machine.

------------------------------------

To run a single pass of all the tests (in directory Tests/..):
zkl Test.testThemAll -R Tests

If on Unix (Linux, BSD), "make test" in the Tests directory
or CD to the VM directory and "make test"

To four passes:
zkl Test.testThemAll -n4 -R Tests

To run continously (until ESC is pressed):
zkl Test.testThemAll --forever -R Tests

To log results to a file, use the "--log" option:
zkl Test.testThemAll --forever --log -R Tests

To test individual test suites:
zkl Tests/fiber
or
zkl Test.testThemAll -- Tests/fiber Tests/Object/class
All the options apply

----------------------

Machines tested:
  * Eight CPUs (Intel Core i7: four cores + four hyper threads), Linux
  * Four core VMWare VMs (Windows XP and FreeBSD) on Intel i7
  * Two CPUs (OLD):
    - AMD X3800 (dual core), 2g ram, Windows XP Pro
      2,737 passes (24 hours, 6,995,772 tests, 31 sec/pass), 13 errors
      165 threads, 2,577,736,291 VMs consumed
    - Intel Pentium D (dual core), 1g ram, Windows XP Pro
      3,312 passes (51 hours, 8,425,728 tests, 56 sec/pass), 10 errors
    - Intel Core 2 Duo E6600 (dual core), 2g ram, Windows XP Pro
      1,091 passes (12 hours, 2,842,055 tests, 39.6 sec/pass), 8 errors
  * One CPU (OLD):
    - Intel Pentium D (zkl confined to a single core), 1g ram, Windows XP Pro
      509 passes (10 hours, 1,325,945 tests, 70.8 sec/pass), 0 errors
        

Bugs:
  - There are still a few GC ones but fatal errors seem rare.

----------------------

iso8601 tests:  if you don't have Src/Time/iso8601.zkl or
Build/Time/iso8601.zsc, you will get an error.  Ignore it or rename
Tests/iso8601.zkl to Tests/Xiso8601.zkl (which causes the test to be
skipped).


/* main.c : The zkl front end for standalone versions
 * If you are creating a shared library, don't use this file.
 * 
 * Copyright (c) 2006,2007-11,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define __NOT_A_DLL

#include <signal.h>
#include <stdio.h>

//#include <google/profiler.h>	// also uncomment ProfilerStart, ProfilerStop

#include "zklObject.h"
#include "zklArglist.h"
#include "zklClass.h"
#include "zklPCBlock.h"

#if 1
   // possible badness: rogue fcn is running, a onExit fcn is running
void handleCntlC(int s)
{
   extern int signalTheMotherShip(void);	// vm.c
   static int strike=0;

//!!!!? I think SIGINT in a noninterruptable system call ie screws up malloc
   if (signalTheMotherShip())
   {
      if (strike)
      {
	 printf("Cntl C still ignored, die, die die!\n");
	 exit(1);
      }
      printf("Cntl C was ignored, try again in a bit\n");
      strike = 1;
   }
   else { printf("Cntl C noted\n"); strike = 0; }
   #ifdef _MSC_VER
      signal(SIGINT,handleCntlC);
   #endif
}
#endif

int main(int argc, char* argv[])
{
#if 1
   #ifdef _MSC_VER
      signal(SIGINT,handleCntlC);
   #else
      struct sigaction sigIntHandler;
      sigIntHandler.sa_handler = handleCntlC;
      sigemptyset(&sigIntHandler.sa_mask);
      sigIntHandler.sa_flags = SA_RESTART;
      sigaction(SIGINT,&sigIntHandler,0);
   #endif
#endif

   zklConstruct(argc,argv);	// initialize

   {
      Instance *startup = vaultFind("startup",0,NoVM);
      Instance *result;

//      ProfilerStart("gooperf.dat");

      // startup may already be running, if started from a wad, it better exit
      if (startup) result = classRunConstructor(startup,NoArglist,NoVM);
      else	   result = readRootClass("startup.zsc",0,NoArglist,NoVM);

//      ProfilerStop();

      // doesn't usually get here, start up calls System.exit()
      collectGarbage(1,NoVM); collectGarbage(1,NoVM);  // close open files, etc
      	// I don't care what result actually is, only what it was
      if (!result) exit(1);	// result == 0 means exception thrown
   }

   return 0;
}

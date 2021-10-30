# The Makefile for zkl (Linux, FreeBSD and PC-BSD using clang)
# Packages needed: 
#   clang (sudo apt-get install clang)
#      GCC has issues
#      Minix3: pkgin install clang binutils
#   ncurses (libncurses5-dev, libncurses5), pthreads
#	sudo apt-get install libncurses5-dev
#	Minix3: pkgin install ncurses
# Linux is the platform I use, sporadically tested on Windows10, PC-BSD.
# PC-BSD desktop: Ready to compile as installed, just modify this Makefile.
# FreeBSD, PC-BSD: You need to modify 3 lines. Search for FreeBSD
# Minix3: *pthreads broken, don't use* You need to modify 5 lines. Search for Minix3

# To run zkl, edit the ../runzkl script and follow the instructions

# To use with shared libraries, "make lib", not "make zkl"
# Why you want to use shared libs: EditLine, BigNum, ZLib, ie extensions
# When using shared libraries, you need the libraries in the load path so
# either add $LD_LIBRARY_PATH=$PWD/../Lib to your profile or
# create a small script to run zkl: 
#   LD_LIBRARY_PATH=$PWD/../Lib $PWD/../Bin/zkl
# or ln -s ~/ZKL/Lib/libzkl.so /usr/lib
#   (/usr/local/lib isn't good enough to link with -lzkl, also need -L)

# To run under the debugger:
#    LD_LIBRARY_PATH=$zklRoot/Lib gdb $zklRoot/Bin/zkl

# To get a core dump if zkl blows a gasket (or you do a kill -SIGQUIT pid):
#    ulimit -c unlimited
#    zklDumpCore=1 zkl
# To look at that core dump:
#    LD_LIBRARY_PATH=$zklRoot/Lib gdb $zklRoot/Bin/zkl core

# Profiling: Use google-tools, see main.c and uncomment -lprofiler (below)

# :1,$s/^V^M// or :%s/^V^M//
# sudo ntfsmount /dev/da1s1 /media/CraigsArchive/ -o locale=be_BY.UTF-8


#malloc=zmalloc.o
#malloc=nedmalloc/nedmalloc.o
#threadPools=thr_pool.o	# USE_THREAD_POOLS in thread.c

files=\
allocate.o  dHash.o       kdata.o    number.o   tcpSocket.o  vm.o      \
arglist.o   dictionary.o  list.o     object.o   thread.o     void.o    \
atomic.o    fcn.o         loader.o   pcBlock.o               wad.o     \
bool.o      file.o        main.o     regex.o    typeTable.o  wildmat.o \
class.o     fxpand.o      method.o   sfmt.o     util.o                 \
data.o      gc.o          miscObj.o  pipe.o     string.o   vault.o     \
MD5/md5c.o  $(malloc) $(threadPools) async.o walker.o

exeFiles=$(files)

#-Wpadded is an interesting flag.

#clang AddressSanitizer: Link with -fsanitize=address, -fno-omit-frame-pointer
#     export ASAN_SYMBOLIZER_PATH=/usr/lib/llvm-3.8/bin/llvm-symbolizer 

#mxI  =-I/usr/pkg/include -I/usr/pkg/include/ncurses# uncomment for Minix3
#mxLib=-L/usr/pkg/lib#	uncomment for Minix3

## Clangs sanitizer options:
   # -fsanitize-memory-track-origins sloooooooooooooow but really nice
#sanitizer=-fsanitize=memory -fsanitize-memory-track-origins -fno-omit-frame-pointer#   thread, memory, address

misc=\
   -fstrict-aliasing -Wno-pointer-to-int-cast $(mxI) \
   -Wunused-variable -Wunused-function #-fstack-check #-std=c99
CC    =clang # I DO *NOT* LIKE GCC, if you must, DON'T use -O*
CFLAGS=-O3 $(sanitizer) -pthread -fpic $(misc)

   # Threads library: pThreads for Linix/FreeBSD/PC-BSD/Minix3
   # FreeBSD can alternatively use thr
libThreads=pthread#     pthread or thr
saLib=-ldl#		Linux, comment out for FreeBSD/PC-BSD/Minix3
ldOptions=$(sanitizer) $(mxLib) -$(libThreads) -lm -lc -lncurses#-lprofiler# -m32 -L/usr/lib32

pickOne:
	@echo "make zkl (stand alone) or make lib (zkl that uses shared libs, my preference)"
	@echo "  make zkl to see if you like it."
	@echo "  Lib is more of a PITA to run (see ../runzkl)"
	@echo "     but you can use extension libraries (like editLine and BigNum)"
	@echo "make clean to switch between the two."
	@echo "   BSD: MAKE SURE you sync the CFLAGS/lib lines (when switching)."
	@echo "make test if you have downloaded the tests (zkl_tests.zip)"

zkl : $(exeFiles) ../Bin
	-mv -f md5c.o MD5/		# FreeBSD, ignore any error
	$(CC) $(exeFiles) $(profile) -o zkl $(saLib) $(ldOptions)
	mv zkl ../Bin
	@echo "Stand alone executable built"

  ## FreeBSD/PC-BSD/Minix3: unComment CFLAGS line and comment out lib line
  ## if you want to "make lib". Or install GNU make.
#CFLAGS+=-DZSHLIB#  uncomment for BSD/Minix3
lib : CFLAGS+=-DZSHLIB # don't load shared libraries unless built as a shlib


#lib : libzkl.so main.o ../Bin ../Lib 
#	$(CC) main.o -o zkl -L. -lzkl $(ldOptions)
#	cp libzkl.so ../Lib
#	mv zkl ../Bin

lib : ../Bin ../Lib ../Lib/libzkl.so ../Bin/zkl

../Bin/zkl : libzkl.so	# already got a rule for zkl, which isn't right here
	$(CC) main.o -o zkl -L. -lzkl $(ldOptions)
	mv zkl ../Bin

../Lib/libzkl.so : libzkl.so
	cp libzkl.so ../Lib

libzkl.so : $(files)
	-mv -f md5c.o MD5/		# FreeBSD, ignore any error
	$(CC) $(files) -lm -shared -Wl,-soname,libzkl.so -o libzkl.so

../Bin :
	mkdir ../Bin
../Lib :
	mkdir ../Lib

clean :
	rm -f $(files) *.o zkl libzkl*

test :
	(cd ..; zklRoot=$$PWD LD_LIBRARY_PATH=Lib:$(LD_LIBRARY_PATH) Bin/zkl Test.testThemAll -ER Tests)


# The generated MD5 dependency don't work
MD5/md5c.o: MD5/md5c.c MD5/global.h MD5/md5.h

# Build the dependency lists
depends : 
	@gcc -MM $(files:.o=.c) zmalloc.c nedmalloc/nedmalloc.c thr_pool.c


allocate.o: allocate.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklList.h zklUtil.h
dHash.o: dHash.c dHash.h zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklString.h zklList.h
kdata.o: kdata.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklData.h zklList.h zklString.h zklUtil.h
number.o: number.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklFcn.h zklPCBlock.h zklUtil.h zklList.h \
 zklMethod.h zklString.h
tcpSocket.o: tcpSocket.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklData.h zklFcn.h zklUtil.h \
 zklList.h zklMethod.h zklString.h
vm.o: vm.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h zklOpcodes.h \
 zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h zklData.h \
 zklDictionary.h dHash.h zklList.h zklMethod.h zklString.h
arglist.o: arglist.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklList.h zklMethod.h zklString.h zklUtil.h
dictionary.o: dictionary.c zklObject.h zklArglist.h zklMemory.h \
 zklAtomic.h zklOpcodes.h zklNumber.h zklDictionary.h dHash.h zklFcn.h \
 zklPCBlock.h zklUtil.h zklList.h zklString.h
list.o: list.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklClass.h zklUtil.h zklData.h \
 zklDictionary.h dHash.h zklList.h zklFcn.h zklMethod.h zklString.h
object.o: object.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklClass.h zklUtil.h zklFcn.h zklPCBlock.h \
 zklList.h zklMethod.h zklString.h dHash.h
thread.o: thread.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklList.h
void.o: void.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklString.h zklList.h
atomic.o: atomic.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklList.h zklMethod.h zklString.h \
 zklUtil.h
fcn.o: fcn.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklData.h zklList.h zklMethod.h zklString.h
loader.o: loader.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklData.h zklList.h
pcBlock.o: pcBlock.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklString.h zklList.h
wad.o: wad.c
bool.o: bool.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklString.h zklList.h
file.o: file.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklData.h zklFcn.h zklPCBlock.h zklUtil.h \
 zklList.h zklString.h
main.o: main.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklClass.h zklUtil.h zklPCBlock.h
regex.o: regex.c regex.h
typeTable.o: typeTable.c zklOpcodes.h
wildmat.o: wildmat.c
class.o: class.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklData.h zklList.h zklMethod.h zklString.h MD5/global.h MD5/md5.h
fxpand.o: fxpand.c zklUtil.h zklObject.h zklArglist.h zklMemory.h \
 zklAtomic.h zklOpcodes.h zklNumber.h
method.o: method.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklClass.h zklUtil.h zklFcn.h zklPCBlock.h \
 zklList.h zklMethod.h zklString.h
sfmt.o: sfmt.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklList.h zklString.h
util.o: util.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklData.h zklList.h zklMethod.h zklString.h
data.o: data.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklData.h zklFcn.h zklPCBlock.h zklUtil.h \
 zklList.h zklString.h
gc.o: gc.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h zklOpcodes.h \
 zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h zklList.h \
 zklMethod.h zklString.h
miscObj.o: miscObj.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklData.h zklDictionary.h dHash.h zklFcn.h \
 zklPCBlock.h zklUtil.h zklList.h MD5/global.h MD5/md5.h zklMethod.h \
 zklString.h regex.h
pipe.o: pipe.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklClass.h zklUtil.h zklList.h zklMethod.h
string.o: string.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklData.h zklFcn.h zklPCBlock.h zklUtil.h \
 zklList.h zklString.h
vault.o: vault.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklClass.h zklUtil.h zklDictionary.h dHash.h \
 zklFcn.h zklPCBlock.h zklList.h zklString.h
md5c.o: MD5/md5c.c MD5/global.h MD5/md5.h
async.o: async.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklList.h
walker.o: walker.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h zklPCBlock.h zklFcn.h zklUtil.h zklClass.h \
 zklData.h zklList.h zklMethod.h zklString.h
zmalloc.o: zmalloc.c zklObject.h zklArglist.h zklMemory.h zklAtomic.h \
 zklOpcodes.h zklNumber.h nedmalloc/malloc.c.h
nedmalloc.o: nedmalloc/nedmalloc.c nedmalloc/nedmalloc.h \
 nedmalloc/malloc.c.h
thr_pool.o: thr_pool.c thr_pool.h

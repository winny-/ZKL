/* tcpSocket.c : The TCP Socket Object
 * 0.0.0.0 for listening sockets and 255.255.255.255 for broadcasting sockets
 * 127.0.0.1 is "localhost"
 * Winsock 2 is part of the MS Visual Studio PlatformSDK
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//!!! watch for VM interrupt

/* ******************************************************************** */
/* ********************** WinSock/Unix Sockets ************************ */
/* ******************************************************************** */

#ifdef _MSC_VER
   #include <winsock2.h>	// link against Ws2_32.lib
   #include <Ws2tcpip.h>	// IPv6 + getaddrinfo
//   #include <Mswsock.h>	// LPFN_DISCONNECTEX & WSAID_DISCONNECTEX
#define WIN32_LEAN_AND_MEAN
#pragma comment(lib, "Ws2_32.lib")
#elif defined(__unix__)
//#include <sys/types.h>
   #include <sys/socket.h>
   #include <arpa/inet.h>
   #include <netdb.h>
   #include <errno.h>
   #include <netinet/in.h>
   #if defined(__FreeBSD__) || defined(__minix__)
      #include <unistd.h>
   #endif
   #include <stdio.h>
   #include <time.h>
#endif

#define __NOT_A_DLL
#define __PC_INTERNALS

#include "zklObject.h"
#include "zklData.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"

static IBucketHeader *tcpBuckets, bucketHeader;

static void socketError(char *text,pVM vm)
{
   char	buf[200];
   #ifdef _MSC_VER
      sprintf(buf,"Winsock: %s (WSA error #%d)",text,WSAGetLastError());
   #else
      sprintf(buf,"Socket: %s (Error #%d)",text,errno);
   #endif
   vmThrow(vm,E_IO_ERROR,buf);
}

static void initWinSock(pVM vm)
{
#ifdef _MSC_VER
   static int winSocketInitialized = 0;

   if (!winSocketInitialized)	// Initialize WinSock and check the version
   {
      int	s;
      WORD	wVersionRequested = WINSOCK_VERSION;	// MAKEWORD(2,2);
      WSADATA	wsaData;

      s = WSAStartup(wVersionRequested, &wsaData);
      if (s || wsaData.wVersion != wVersionRequested)
      {
	 WSACleanup();
	 socketError("Wrong version",vm);
      }
      winSocketInitialized = 1;
   }
#endif
}

Instance *Network_hostname(Instance *self,pVM vm)
{
   char buf[500]; *buf='\0';

   initWinSock(vm);
   gethostname(buf,sizeof(buf));
   return stringCreate(buf,I_OWNED,vm);
}

/* ******************************************************************** */
/* ************************** Client Sockets ************************** */
/* ******************************************************************** */

#ifdef __unix__
   typedef int SOCKET;
#endif

typedef struct
{
   BInstance  instance;			// Inherit from BInstance
   SOCKET     socket;
   Instance  *hostName, *hostAddr;	// Strings, will be GC'd
} ClientSocket;		// 16

static ZKL_Object  ClientSocketObject;
static Instance	  *aClientSocket = 0;

static void clientMarker(Instance *self)
{
   instanceMark(((ClientSocket *)self)->hostAddr);
   instanceMark(((ClientSocket *)self)->hostName);
}

static ClientSocket *clientSocketCreate(SOCKET socket,pVM vm)
{
   ClientSocket	*clientSocket = (ClientSocket *)
	   ibucketAllocate(tcpBuckets,&ClientSocketObject,I_OWNED,1,vm);
   clientSocket->socket   = socket;
   clientSocket->hostName = clientSocket->hostAddr = emptyString;

   // Windows default: Graceful shutdown in background
   if (socket)
   {
      int s;
      struct linger lingerLonger;
      lingerLonger.l_onoff  = 1;	// don't close immediately
      lingerLonger.l_linger = 10;	// seconds to block until data sent

      s = setsockopt(socket,SOL_SOCKET,		// SO_LINGER, SO_DONTLINGER
		SO_LINGER,(char *)&lingerLonger,sizeof(lingerLonger));
   }

   containerIsCooked(tcpBuckets,(Instance *)clientSocket,I_OWNED);
   return clientSocket;
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

#define I_KNOW_IPV6 1	// Linux, FreeBSD, Win10 for sure

#if I_KNOW_IPV6
struct addrinfo *_getAddrInfo(char *serverName, char *port, pVM vm)
{
   int    rc;
   struct in6_addr serverAddr;
   struct addrinfo hints, *ai=NULL;

   memset(&hints, 0x00, sizeof(hints));
   hints.ai_flags    = AI_NUMERICSERV;   // port is numeric string
   hints.ai_family   = AF_UNSPEC;
   hints.ai_socktype = SOCK_STREAM;
      /* Check if we were provided the address of the server using
       * inet_pton() to convert the text form of the address to binary form.
       * If it is numeric then we want to prevent getaddrinfo() from doing
       * any name resolution.
       */
   rc = inet_pton(AF_INET, serverName, &serverAddr);
   if (rc == 1)    // valid IPv4 "ddd.ddd.ddd.ddd" address
   {
      hints.ai_family = AF_INET;
      hints.ai_flags |= AI_NUMERICHOST;
   }
   else    // valid IPv6 x:x:x:x:x:x:x:x address?
   {
      rc = inet_pton(AF_INET6, serverName, &serverAddr);
      if (rc == 1)
      {
	 hints.ai_family = AF_INET6;
	 hints.ai_flags |= AI_NUMERICHOST;
      }
   }
   hints.ai_flags |= AI_CANONNAME;	// I want a name string
   vmMightStall(vm,1);	// this can take a while
      rc = getaddrinfo(serverName, (char *)(size_t)port, &hints, &ai);
   vmMightStall(vm,0);
   if (rc != 0)
   {
      char buf[300];
      sprintf(buf,"Socket: Host not found(%d:%s)",rc,gai_strerror(rc));
      #ifndef _MSC_VER
         if (rc == EAI_SYSTEM) strcat(buf,"getaddrinfo() failed");
      #endif
      vmThrow(vm,E_IO_ERROR,buf);
   }
   return ai;
}

void *get_in_addr(struct sockaddr *sa)
{
   if (sa->sa_family == AF_INET) return &(((struct sockaddr_in*)sa)->sin_addr);
   return &(((struct sockaddr_in6*)sa)->sin6_addr);
}
#endif

    // .addrInfo(addr) --> T, 
    // eg Network.TCPClientSocket.addrInfo("google.com")
    //    ("localhost"), ("74.125.239.38")
    // IPV6 address (RFC 2373): x:x:x:x:x:x:x:x, ::, ::1, 
    //   eg ("2001:5c0:1000:11::2"),
    //   x:x:x:x:x:x:d.d.d.d (eg ::FFFF:204.152.189.116)
    //   -->L("ipv6.test-ipv6.com",L("2001:470:1:18::119"))
    // http://[1fff:0:a88:85a3::ac1f]:8001/index.html
static Instance *addrInfo(Instance *self,pArglist arglist,pVM vm)
{
   char		  *addr;
   Instance	  *list;
   Fence	   fence;

   addr = arglistGetOnlyString(arglist,0,0,vm);

#if I_KNOW_IPV6
{
   struct addrinfo *ai = _getAddrInfo(addr,0,vm);	// flies or dies

   list = tupleCreate(3,I_OWNED,vm);
   vmSetFence(vm,&fence,0,list);
      tupleAppend(list,stringCreate(ai->ai_canonname,I_OWNED,vm));

      {		// IP names
	 int	   n;
	 Instance *ipNames;
	 struct addrinfo *ptr;

	 for(n=0, ptr=ai; ptr; ptr=ptr->ai_next) n++;
	 ipNames  = tupleCreate(n,I_OWNED,vm);
	 fence.i1 = ipNames;
	 for(ptr=ai; ptr; ptr=ptr->ai_next)
	 {
	    char buf[INET6_ADDRSTRLEN];

		// ai_family: AF_INET6(10), AF_INET(2)
	    inet_ntop(ptr->ai_family, get_in_addr(ptr->ai_addr), buf,sizeof buf);
	    tupleAppend(ipNames,stringCreate(buf,I_OWNED,vm));
	 }

	 tupleAppend(list,ipNames);
      }
   vmRemoveFence(&fence,0);
   freeaddrinfo(ai);
}   
#else	// deprecated
{
   struct in_addr  iaHost;
   struct hostent *hostEntry;

   initWinSock(vm);

	// Use inet_addr() to determine if we're dealing with a name
	// or an IP address (127.0.0.0)
   iaHost.s_addr = inet_addr(addr);	// Ipv4 address (eg 127.0.0.1)?
vmMightStall(vm,1);
   if (iaHost.s_addr == INADDR_NONE)	// Not IP addr, assume name
      hostEntry = gethostbyname(addr);	// 0 == localhost  !!!deprecated
   else		      // It was a valid IP address
      hostEntry = gethostbyaddr((const char *)&iaHost,	//!!!deprecated
	      sizeof(struct in_addr), AF_INET);
vmMightStall(vm,0);
   if (hostEntry == NULL) socketError("gethostbyaddr puked",vm);

   list = tupleCreate(3,I_OWNED,vm);
   vmSetFence(vm,&fence,0,list);
	// hostEntry->h_length == 4 for IPv4
	// append the canonical name
      tupleAppend(list,stringCreate(hostEntry->h_name,I_OWNED,vm));

#if 0
      {		// aliases
	 Instance  *altNames;
	 char	  **ptr;
	 int	    n = 0;

	 for(ptr=hostEntry->h_aliases; *ptr; ptr++,n++) ;
	 altNames  = tupleCreate(n,I_OWNED,vm);
	 fence.i1 = altNames;
	 for(ptr=hostEntry->h_aliases; *ptr; ptr++)
	    tupleAppend(altNames,stringCreate(*ptr,I_OWNED,vm));

	 tupleAppend(list,altNames);
      }
#endif

      {		// alternate IP addresses
	 Instance        *addrList;
	 struct in_addr **ptr;
	 int		  n = 0;

	 for(ptr=(struct in_addr **)hostEntry->h_addr_list; *ptr; ptr++,n++) ;
	 addrList = tupleCreate(n,I_OWNED,vm);
	 fence.i1 = addrList;
	 for(ptr=(struct in_addr **)hostEntry->h_addr_list; *ptr; ptr++)
	    tupleAppend(addrList,stringCreate(inet_ntoa(**ptr),I_OWNED,vm));

	 tupleAppend(list,addrList);
      }
   vmRemoveFence(&fence,0);
}
#endif

   return list;
}

typedef struct sockaddr_in Sockaddr_in;		// too damn much typing

#if defined(__unix__)
   #define closesocket	close

   #define INVALID_SOCKET	-1
   #define SOCKET_ERROR		-1
#endif

#if defined(_MSC_VER)
	// shutdown constants
   #define SHUT_RD	SD_RECEIVE	// 0
   #define SHUT_WR	SD_SEND		// 1
   #define SHUT_RDWR	SD_BOTH		// 2
#endif

    // .connectTo(server,port) --> ClientSocket
static Instance *Client_connectTo(Instance *self,pArglist arglist,pVM vm)
{
   char		  *serverName;
   int		   s, port;
   SOCKET	   theSocket;
   ClientSocket   *clientSocket;
   Fence	   fence;

   struct addrinfo *ai = 0;
   char      buf[INET6_ADDRSTRLEN];

 
   initWinSock(vm);

   serverName = arglistGetOnlyString(arglist,0,".connectTo",vm);
   port       = (int)arglistGetInt(arglist,1,".connectTo",vm);

#if I_KNOW_IPV6
   sprintf(buf,"%d",(int)arglistGetInt(arglist,1,".connectTo(*,port)",vm));
   ai = _getAddrInfo(serverName,buf,vm);	// flies or dies

   theSocket = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
   if (theSocket == INVALID_SOCKET)
   {
      freeaddrinfo(ai);
      socketError("Can't create socket",vm);
   }
   vmMightStall(vm,1);	// cross the network, can timeout
      s = connect(theSocket, ai->ai_addr, ai->ai_addrlen);
   vmMightStall(vm,0);

   if (s == SOCKET_ERROR)
   {
      if (ai) freeaddrinfo(ai);
      closesocket(theSocket);
      socketError("Connect failed",vm);
   }
   clientSocket = clientSocketCreate(theSocket,vm);
   vmSetFence(vm,&fence,0,(Instance *)clientSocket);
   {
      inet_ntop(ai->ai_family, get_in_addr(ai->ai_addr), buf,sizeof buf);
      clientSocket->hostAddr = fence.i1 = stringCreate(buf,I_OWNED,vm);
      clientSocket->hostName = stringCreate(ai->ai_canonname, I_OWNED,vm);

      freeaddrinfo(ai);
   }
   vmRemoveFence(&fence,0);

#else	// IPv4 only

   initWinSock(vm);

	// Use inet_addr() to determine if we're dealing with a name
	// or an IP address (127.0.0.0)
   iaHost.s_addr = inet_addr(serverName);  // Ipv4 address (eg 127.0.0.1)?
   vmMightStall(vm,1);
      if (iaHost.s_addr == INADDR_NONE)	// Not IP addr, assume name
	 hostEntry = gethostbyname(serverName);	// 0 == localhost
      else		      // It was a valid IP address
	 hostEntry = gethostbyaddr((const char *)&iaHost,
		 sizeof(struct in_addr), AF_INET);
   vmMightStall(vm,0);
   if (hostEntry == NULL) socketError("gethostbyaddr puked",vm);

	// Create a TCP/IP stream socket
   theSocket = socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);

   if (theSocket == INVALID_SOCKET) socketError("Can't create socket",vm);

#if 0
   {
      LPSERVENT	servEnt;
	// Find the port number for the HTTP service on TCP
      servEnt = getservbyname("http","tcp");
      if (servEnt == NULL) saServer.sin_port = htons(80);
      else		   saServer.sin_port = htons(port);
   }
   else 
#endif
   saServer.sin_port = htons(port);	// in network byte order

	// Fill in the rest of the server address structure
   saServer.sin_family = AF_INET;
      // grab s_addr (in_addr_t) out of h_addr_list
      // h_addr_list[n] == "<ip2 addr><name>" == <4 bytes><text>
      // eg www.l.google.com/74.125.19.104 --> "\x74\x125\x19\x104www.l.google.com"
//      saServer.sin_addr   = *((LPIN_ADDR)*hostEntry->h_addr_list);
   saServer.sin_addr = *((struct in_addr *)hostEntry->h_addr_list[0]);
//memcpy((void *)&saServer.sin_addr,(void *)hostEntry->h_addr_list[0],4);

	// Connect the socket to the server
   vmMightStall(vm,1);	// cross the network, can timeout
      s = connect(theSocket,(struct sockaddr *)&saServer, sizeof(Sockaddr_in));
   vmMightStall(vm,0);

   if (s == SOCKET_ERROR)
   {
      closesocket(theSocket);
      socketError("Connect failed",vm);
   }
   clientSocket = clientSocketCreate(theSocket,vm);
   vmSetFence(vm,&fence,0,(Instance *)clientSocket);
   {
      clientSocket->hostName = stringCreate(hostEntry->h_name, I_OWNED,vm);
      clientSocket->hostAddr = stringCreate(  //!!! convert to inet_ntop
		inet_ntoa(*(struct in_addr*)hostEntry->h_addr),I_OWNED,vm);
   }
   vmRemoveFence(&fence,0);
#endif	// IPv4 only

   return (Instance *)clientSocket;
}

static void clientSocketShutDown(ClientSocket *self)
{
   SOCKET socket = self->socket;
   self->socket = 0;
   if (socket)
   {
      int s;
      #if _MSC_VER && 0
         LPFN_DISCONNECTEX DisconnectEx;
	 int n;
         s = WSAIoctl(socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
	     &disco,sizeof(GUID),&DisconnectEx,sizeof(DisconnectEx),&n,
	     NULL,NULL);
	s = DisconnectEx(socket,NULL,0,0);
//         s = WSASendDisconnect(socket,NULL);
      #else
         s = shutdown(socket,SHUT_WR);
      #endif
      s = closesocket(socket);
   }
}

static int clientSocketFree(Instance *self)
   { clientSocketShutDown((ClientSocket *)self); return 1; }

    // Client.close() --> self
static Instance *Client_close(ClientSocket *self,pArglist arglist,pVM vm)
{
   clientSocketShutDown(self);
   return (Instance *)self;
}

static int _socketDied(void)
{
   #ifdef _MSC_VER
      int errorNo = WSAGetLastError();  // WSAECONNRESET (10054) is common
      return (errorNo == WSAENETDOWN  || errorNo == WSAENETRESET    || 
	      errorNo == WSAESHUTDOWN || errorNo == WSAECONNABORTED ||
	      errorNo == WSAECONNRESET);
   #elif defined(__unix__)
      return (errno ==  EINTR || errno == ECONNREFUSED || errno == ECONNRESET);
   #endif
}

static size_t _writeToSocket(SOCKET socket,Instance *x,pVM vm)
{
   Byte   *ptr;
   size_t  size = 0;
   Byte    c;
   int	   s;

   switch (TYPEO(x))
   {
      case StringType:
	 ptr  = (Byte *)stringText(x); size = strlen((char *)ptr);
	 break;
      case IntType:
	 c = (char)convertToInt(x,vm);		// lowest byte
	 ptr = &c; size = 1;
	 break;
      case DataType:
	 ptr = dataText(x,&size);
	 break;
      case ListType:	// !!! circular --> boom, not thread safe
      case TupleType:
      {
	 size_t	   i, s;
	 Instance *t;
	 for (i = 0; (t = listGet(x,i)); i++)
	 {
	    s = _writeToSocket(socket,t,vm);
	    size += s;
	 }
	 return size;
      }
      default:
      {
	 char	buf[100];
	 sprintf(buf,"Can't write type %s to Socket",iname(x));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
   }

   vmMightStall(vm,1);
      s = send(socket,ptr,(int)size,0);
   vmMightStall(vm,0);
   if (s == SOCKET_ERROR)	// -1
   {
      if (_socketDied()) vmThrow(vm,E_THE_END,"TCPSocket.write");
      socketError("send puked",vm);
   }
   return size;
}

    // Client.write(request) --> int
static Instance *Client_write(ClientSocket *self,pArglist arglist,pVM vm)
{
   size_t    n = 0, bytesWritten = 0;
   Instance *i;

   while ((i = listGet(arglist,n++)))
      bytesWritten += _writeToSocket(self->socket,i,vm);
   return intCreate(bytesWritten,vm);
}

    // Client.read(readUntilClosed=False | bytesToRead) --> Data
//!!!This should have a timeout
static Instance *Client_read(ClientSocket *self,pArglist arglist,pVM vm)
{
   Byte	     buffer[1024];
   int	     n, readUntilClosed = 0, readN = 0;
   Instance *data;
   SOCKET    socket = self->socket;
   size_t    N;
   Fence     fence;

   if (listLen(arglist,vm))
   {
      Instance *arg = arglistGet(arglist,0,0,vm);
      N = TYPEO(arg);
      if (N == BoolType) readUntilClosed = BOOLV(arg);
      else if (N == IntType) { N = (size_t)convertToInt(arg,vm); readN = 1; }
   }

   if (readN)
   {
      data = dataCreate(N,I_OWNED,vm);
      vmSetFence(vm,&fence,0,data);
	 while(N)
	 {
	    n  = (int)(N < sizeof(buffer) ? N : sizeof(buffer));
vmMightStall(vm,1);
#if 0
unsigned long millis;
timeout = getMillis(arglistTryToGet(arglist,1),&millis,vm);
if (timeout)
{
   fd_set rfds;
   struct timespec timeout = {0,0};
   timeout.usec = millis*10;
   FD_ZERO(&rfds); FD_SET(socket,&rfds);
   s = select(1,&rfds,0,0,&timeout);
   if (!s) timedout
}
#endif
	    n = recv(socket,buffer,n,0);
vmMightStall(vm,0);
	    if (n == SOCKET_ERROR)
	    {
	       if (_socketDied())	// but still might have data?
	       {
		  #if _MSC_VER	// what a fucking mess
		     ioctlsocket(socket,FIONREAD,&n);
		     if (n) vmThrow(vm,E_IO_ERROR,"Windows can't read data in socket");
		  #endif
	       	  vmThrow(vm,E_THE_END,"TCPSocket.read");
	       }
	       socketError("recv() puked",vm);  // throws
	    }
	    if (n == 0)	// other end of socket closed
	    {
	       clientSocketShutDown(self);
	       // I know N != 0, so I wasn't able to read all data requested
	       if (dataLen(data)) break;	// some data has been read
	       // else haven't read anything
	       vmThrow(vm,E_THE_END,"TCPSocket.read");
	    }
	    dataInsertBytes(data,0,buffer,n,2,vm);
	    N -= n;
	 } // while
      vmRemoveFence(&fence,0);
      return data;
   }

   data = dataCreate(500,I_OWNED,vm);
   vmSetFence(vm,&fence,0,data);
      while (1)	// read until socket closes
      {
	 vmMightStall(vm,1);
	    n = recv(socket,buffer,sizeof(buffer),0);
	 vmMightStall(vm,0);
	 if (n == SOCKET_ERROR)
	 {
	    if (_socketDied()) n = 0;
	    else socketError("recv() puked",vm);  // throws
	 }
	 if (n == 0)	// server closed the connection, or timeout
	 {
	    clientSocketShutDown(self);
	    break;
	 }
	 dataInsertBytes(data,0,buffer,n,2,vm);
	 if (!readUntilClosed) break;
      }
   vmRemoveFence(&fence,0);
   return data;
}

    // Client.wait(timeout=block) --> Bool|1|Void
    // Can I read from socket?
    // False: Timeout, True, 1: Data available, Void: Error
static Instance *Client_wait(ClientSocket *self,pArglist arglist,pVM vm)
{
   fd_set 	  sockset;
   struct timeval timeout = { 0,0 }, *t = &timeout;
   int		  s;
   unsigned long  millis;

   s = getMillis(arglistTryToGet(arglist,0),&millis,vm);
   timeout.tv_sec  = (millis / 1000);
   timeout.tv_usec = (millis % 1000) * 1000;	// micro seconds
   if (!s) t = 0;	// block

   FD_ZERO(&sockset);
   FD_SET(self->socket, &sockset);
   vmMightStall(vm,1);
	/* Windows:  First arg is ignored.  For connection-oriented sockets,
	 *   readability can also indicate that a request to close the
	 *   socket has been received from the peer.
	 *   In summary, a socket will be identified in a particular set
	 *   when select returns if:
	 *   - Data is available for reading
	 *   - Connection has been closed/reset/terminated.
	 * Unix:  A descriptor shall be considered ready for reading when a
	 *   call to an input function with O_NONBLOCK clear would not
	 *   block, whether or not the function would transfer data
	 *   successfully.  (The function might return data, an end-of-file
	 *   indication, or an error other than one indicating that it is
	 *   blocked, and in each of these cases the descriptor shall be
	 *   considered ready for reading.)
	 */
      s = select((int)self->socket + 1, &sockset,0,0,t);
   vmMightStall(vm,0);
   switch(s)
   {
      case 0: return BoolFalse;		// timeout, no data
      case 1: return One;		// data available
      case SOCKET_ERROR:	// ie -1
//	 if (_socketDied()) return BoolTrue;	// closed
	 return Void;			// error
   }
   return Void;				// error
}

    // Client.toBool() --> Bool
static Instance *Client_toBool(ClientSocket *self,pArglist arglist,pVM vm)
   { return boolCreate(self->socket != 0); }

    // Client.len() --> int
static Instance *Client_len(ClientSocket *self,pArglist arglist,pVM vm)
{
   unsigned long n = 0;
   #ifdef _MSC_VER
      ioctlsocket(self->socket,FIONREAD,&n);
   #elif defined(__unix__)	// !!! this sucks
      fd_set sockset;
      FD_ZERO(&sockset);
      FD_SET(self->socket, &sockset);
      if (1 == select(self->socket + 1, &sockset,0,0,0))
      {
	 // The socket has data. For good measure,
	 // it's not a bad idea to test further
	 if (FD_ISSET(self->socket, &sockset)) n = 1;
      }
   #endif
   return intCreate(n,vm);
}

static const MethodTable clientMethods[] =
{
   "connectTo",	(pMethod)Client_connectTo,
   "read",	(pMethod)Client_read,
   "write",	(pMethod)Client_write,
   "close",	(pMethod)Client_close,
   "len",	(pMethod)Client_len,
   "toBool",	(pMethod)Client_toBool,
   "seek",	(pMethod)Object_noop,	// Stream
   "flush",	(pMethod)Object_noop,	// Stream
   "addrInfo",	(pMethod)addrInfo,
   "wait",	(pMethod)Client_wait,
   0,		0
};


	///////////////////////// Client Socket Properties
    // Client.hostName
static Instance *ClientSocket_hostName(ClientSocket *self, pVM vm)
   { return self->hostName; }

    // Client.hostAddr
static Instance *ClientSocket_hostAddr(ClientSocket *self, pVM vm)
   { return self->hostAddr; }

    // Client.isClosed
static Instance *Client_isClosed(ClientSocket *self,pVM vm)
   { return boolCreate(self->socket == 0); }

static const PropertyTable clientProperties[] =
{
   "hostName",	(pProperty)ClientSocket_hostName,
   "hostAddr",	(pProperty)ClientSocket_hostAddr,
   "isClosed",	(pProperty)Client_isClosed,
   0,		0
};

/* ******************************************************************** */
/* ************************** Server Sockets ************************** */
/* ******************************************************************** */

typedef struct
{
   BInstance instance;		// Inherit from BInstance
   SOCKET    listenSocket;
   Instance *hostname;		// String
   unsigned int port:32;
   unsigned int listening:1;
} ServerSocket;		// 16

static ZKL_Object  ServerSocketObject;
static Instance	  *aServerSocket = 0;

static void serverMarker(Instance *self)
   { instanceMark(((ServerSocket *)self)->hostname); }

static ServerSocket *serverSocketCreate(pVM vm)
{
   ServerSocket	*socket = (ServerSocket *)
	   ibucketAllocate(tcpBuckets,&ServerSocketObject,I_OWNED,1,vm);
   socket->listenSocket = 0;
   socket->hostname	= emptyString;
   socket->port		= 0;
   socket->listening    = 0;

   containerIsCooked(tcpBuckets,(Instance *)socket,I_OWNED);
   return socket;
}

    // sever.open(port) --> ServerSocket
    // Create a TCP/IP stream socket to "listen" with
static Instance *Server_open(Instance *self,pArglist arglist,pVM vm)
{
   SOCKET	 listenSocket;
   Sockaddr_in	 saServer;
   int		 s;
   char		 buf[500];
   int		 port = (int)arglistGetInt(arglist,0,".open",vm);
   ServerSocket	*serverSocket;
   Fence	 fence;

   initWinSock(vm);

   vmMightStall(vm,1);
      listenSocket = socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
   vmMightStall(vm,0);
   if (listenSocket == INVALID_SOCKET) socketError("Can't create socket",vm);

   #ifdef __unix__
   {
      int on = 1;
      //struct linger linger;
      setsockopt(listenSocket,SOL_SOCKET,SO_REUSEADDR,&on,sizeof(on));
      //linger.l_onoff  = 1;
      //linger.l_linger = 5;
      //setsockopt(listenSocket,SOL_SOCKET,SO_LINGER,&linger,sizeof(linger));
   }
   #endif

   saServer.sin_family	    = AF_INET;
   saServer.sin_addr.s_addr = INADDR_ANY;	// Let WinSock supply address
   saServer.sin_port	    = htons(port);	// Use port from command line

	// bind the name to the socket
   vmMightStall(vm,1);
      s = bind(listenSocket,			// Socket
//		(LPSOCKADDR)&saServer,		// Our address
		(struct sockaddr *)&saServer,	// Our address
		sizeof(struct sockaddr));	// Size of address structure
   vmMightStall(vm,0);
   if (s == SOCKET_ERROR)
   {
      closesocket(listenSocket);
      socketError("bind failed",vm);
   }

   s = gethostname(buf, sizeof(buf));
   if (s == SOCKET_ERROR)
   {
      closesocket(listenSocket);
      socketError("gethostname has issues",vm);
   }

	// Set the socket to listen
   vmMightStall(vm,1);
      s = listen(listenSocket,		// Bound socket
		  SOMAXCONN);		// Number of connection request queue
   vmMightStall(vm,0);
   if (s == SOCKET_ERROR)
   {
      closesocket(listenSocket);
      socketError("listen has a tin ear",vm);
   }

   serverSocket = serverSocketCreate(vm);
   vmSetFence(vm,&fence,0,(Instance *)serverSocket);
      serverSocket->listenSocket = listenSocket;
      serverSocket->hostname	 = stringCreate(buf,I_OWNED,vm);
      serverSocket->port	 = port;
   vmRemoveFence(&fence,0);

   return (Instance *)serverSocket;
}

    // Server.listen(handlerFcn|Pipe) --> self
    // Sits here until closed or interrupted
    // --> listen().pump(Pipe.write.fp1(0)) or .apply2(Pipe.write,0)
    // --> listen().pump(f)
#if 0
static Instance *Server_listen(ServerSocket *self,pArglist arglist,pVM vm)
{
   extern int fcnID, pipeID, strawID;	// fcn.c, pipe.c

   SOCKET    listenSocket = self->listenSocket;
   SOCKET    remoteSocket;
   int	     writeToPipe = 0, n, isPF = 0, one = 1;
   Instance *handler = 
//	       arglistGetBT(arglist,0,FcnType,"TCPServerSocket.listen",vm);
	       arglistGet(arglist,0,"TCPServerSocket.listen",vm);

   Sockaddr_in addr;
   socklen_t   addrlen = sizeof(Sockaddr_in);

   if (!listenSocket)
      vmThrow(vm,E_IO_ERROR,"TCPServerSocket.listen: socket is closed");

   #if USE_POINTER_INTS
      if (IS_PtrInt(handler))
	 vmThrow(vm,E_TYPE_ERROR,"TCPServerSocket.listen(Fcn|Pipe)");
   #endif
   n = I_OBJ_ID(handler);
   if (n == pipeID || n == strawID) writeToPipe = 1;
   else if (n == fcnID || (isPF = isPartialFcn(handler))) {}
   else vmThrow(vm,E_TYPE_ERROR,"TCPServerSocket.listen(Fcn|Pipe)");

   while (one)	// Wait for an incoming request
   {
      vmMightStall(vm,1);
         self->listening = 1;
         remoteSocket = accept(listenSocket,	// Listening socket
				(struct sockaddr *)&addr,&addrlen);
//				NULL,NULL);	// Optional client address
         self->listening = 0;
      vmMightStall(vm,0);
      if (remoteSocket == INVALID_SOCKET)	// -1, WSAEINTR(10004)
      {  // EINVAL(22) == socket not listening
	 if (!self->listenSocket) break;	// server socket was closed
	 closesocket(listenSocket);
	 socketError("accept says invalid socket",vm);
      }

	// We're connected to a client
	// New socket descriptor returned already has clients address
      { // call the client handler: h(clientSocket,serverSocket)
	 MLIST(mlist,2);
	 char      buf[100];
	 Fence	   fence;
	 Instance *clientSocket =
			(Instance *)clientSocketCreate(remoteSocket,vm);

	 *buf = 0;  // so I don't space out #ifs and forget
	 #if __unix__
	    if (!inet_ntop(addr.sin_family,&addr.sin_addr,buf,sizeof(buf)))
	       *buf = 0;	// error
	 #endif

	 vmSetFence(vm,&fence,0,clientSocket);  !!! move fence above loop
	    ((ClientSocket *)clientSocket)->hostAddr = 
		    stringCreate(buf,I_OWNED,vm);
	    if (writeToPipe)	// NO blocking, write or die
	    {
	       Instance *args = mlistBuild(mlist,clientSocket,Zero,ZNIL);
	       fence.i1 = args;
	       Pipe_write(handler,args,vm);
	    }
	    else
	    {
	       Instance *r, *args = 
	          mlistBuild(mlist,clientSocket,(Instance *)self,ZNIL);
	       fence.i1 = args;
	       r = objectRun(handler,args,0,vm);
	       if (r == VoidStop) { printf("Stop listening\n"); one = 0; }
	    }
	 vmRemoveFence(&fence,0);
      }
   } // while

   return (Instance *)self;	// gets here if socket is closed
}
#else

static Instance *_listener(Instance *pSelf,   // gets repeatedly called by zapply()
			size_t x1,void *fence,size_t x2,pVM vm)
{
   ServerSocket *self = (ServerSocket *)pSelf;
   Sockaddr_in   addr;
   socklen_t     addrlen = sizeof(Sockaddr_in);
   SOCKET	 listenSocket = self->listenSocket;
   SOCKET	 remoteSocket;

   // Wait for an incoming request
   vmMightStall(vm,1);
      self->listening = 1;
      remoteSocket = accept(listenSocket,	// Listening socket
			     (struct sockaddr *)&addr,&addrlen);
      //			NULL,NULL);	// Optional client address
      self->listening = 0;
   vmMightStall(vm,0);
   if (remoteSocket == INVALID_SOCKET)	// -1, WSAEINTR(10004)
   {  // EINVAL(22) == socket not listening
      if (!self->listenSocket) return 0;	// server socket was closed
      closesocket(listenSocket);
      socketError("accept says invalid socket",vm);
   }

     // We're connected to a client
     // New socket descriptor returned already has clients address
   {
      char      buf[100];
      Instance *clientSocket = (Instance *)clientSocketCreate(remoteSocket,vm);

      *buf = 0;  // so I don't space out #ifs and forget
      #if __unix__
	 if (!inet_ntop(addr.sin_family,&addr.sin_addr,buf,sizeof(buf)))
	    *buf = 0;	// error
      #endif

      ((Fence *)fence)->i2 = clientSocket;
      ((ClientSocket *)clientSocket)->hostAddr = stringCreate(buf,I_OWNED,vm);
      return clientSocket;
   }
}

static Instance *Server_listen(ServerSocket *self,pArglist arglist,pVM vm)
{
   extern int fcnID, pipeID, strawID;	// fcn.c, pipe.c

   int	     n, writeToPipe = 0;
   Instance *handler = arglistGet(arglist,0,"TCPServerSocket.listen",vm), *args;

   Fence fence;
   MLIST(mlist,2);

   if (!self->listenSocket)
      vmThrow(vm,E_IO_ERROR,"TCPServerSocket.listen: socket is closed");

   if (IS_PtrInt(handler))
      vmThrow(vm,E_TYPE_ERROR,"TCPServerSocket.listen(Fcn|Pipe)");
   n = I_OBJ_ID(handler);
   if (n == pipeID || n == strawID) writeToPipe = 1;
   else if (n == fcnID || isPartialFcn(handler) || n == methodID) {}
   else vmThrow(vm,E_TYPE_ERROR,"TCPServerSocket.listen(Fcn|Pipe)");

   vmSetFence(vm,&fence,0,0);	// main loop cleverly hidden inside _listener.apply2
      if (writeToPipe)	// --> listen().apply2(Pipe.write,0)
      {
	 Instance *pipeWrite =	// NO blocking, write or die
			methodCreate2(handler,"write",Pipe_write,I_OWNED,vm);
	 args = mlistBuild(mlist,pipeWrite,Zero,ZNIL);
      }
      else // fcn --> listen().apply2(f,self)
	 args = mlistBuild(mlist,handler,(Instance *)self,ZNIL);
      fence.i1 = args;
      zapply((Instance *)self,_listener,(void *)&fence, 0,0, args,0,vm);
   vmRemoveFence(&fence,0);

   return (Instance *)self;	// gets here if socket is closed
}
#endif


static void serverSocketGC(ServerSocket *self)
{
   SOCKET socket = self->listenSocket;
   self->listenSocket = 0;
   self->port	      = 0;
   self->listening    = 0;
   if (socket) { shutdown(socket,SHUT_RDWR); closesocket(socket); }
}

static int serverSocketFree(Instance *self)
   { serverSocketGC((ServerSocket *)self); return 1; }

    // Server.close() --> self
static Instance *Server_close(ServerSocket *self,pArglist arglist,pVM vm)
{
   serverSocketGC(self);
   	// handlers need to close client sockets
   return (Instance *)self;
}

    // Server.toBool() --> Bool
static Instance *Server_toBool(ServerSocket *self,pArglist arglist,pVM vm)
   { return boolCreate(self->listenSocket != 0); }

static const MethodTable serverMethods[] =
{
   "open",	(pMethod)Server_open,
   "listen",	(pMethod)Server_listen,
   "close",	(pMethod)Server_close,
   "toBool",	(pMethod)Server_toBool,
   0,		0
};


   ///////////////////////// Server Socket Properties
    // Server.hostname
static Instance *Server_hostname(ServerSocket *self, pVM vm)
   { return self->hostname; }

    // Server.port
static Instance *Server_port(ServerSocket *self, pVM vm)
   { return intCreate(self->port,vm); }

    // Server.isClosed
static Instance *Server_isClosed(ServerSocket *self,pVM vm)
   { return boolCreate(self->listenSocket == 0); }

    // Server.isListening
static Instance *Server_isListening(ServerSocket *self,pVM vm)
   { return self->listening ? BoolTrue : BoolFalse; }

static const PropertyTable serverProperties[] =
{
   "hostname",	  (pProperty)Server_hostname,
   "port",	  (pProperty)Server_port,
   "isClosed",	  (pProperty)Server_isClosed,
   "isListening", (pProperty)Server_isListening,
"addr",	  (pProperty)Server_hostname,	//!!!!!nuke!!!!
   0,		0
};

/* ******************************************************************** */
/* ******************************************************************** */

// at shut down: WSACleanup();

void tcpSocketConstruct(void)
{
   constructObject(&ClientSocketObject,NativeType,
	clientMethods,clientProperties,0, NoVM);
   ClientSocketObject.name	   = "TCPClientSocket";
   ClientSocketObject.vaultPath	   = "Network";
   ClientSocketObject.freeMe	   = clientSocketFree;
   ClientSocketObject.magicMarker  = clientMarker;
   ClientSocketObject.isize	   = sizeof(ClientSocket);
   ClientSocketObject.threadSafe   = 0;
   ClientSocketObject.isBInstance  = 1;

   constructObject(&ServerSocketObject,NativeType,
	serverMethods,serverProperties,0, NoVM);
   ServerSocketObject.name	   = "TCPServerSocket";
   ServerSocketObject.vaultPath	   = "Network";
   ServerSocketObject.freeMe 	   = serverSocketFree;
   ServerSocketObject.magicMarker  = serverMarker;
   ServerSocketObject.isize	   = sizeof(ServerSocket);
   ServerSocketObject.threadSafe   = 0;
   ServerSocketObject.isBInstance  = 1;

   {
      ZKL_Object *obj = &ClientSocketObject;
      if (sizeof(ClientSocket) < sizeof(ServerSocket))
	 obj = &ServerSocketObject;
	// Share with Dictionary/Int/Float/File on Windows
	// not many of these so not to concerned about a loose fit
	// Extra call since I'm too lazy to check what I'm hitch hiking
      tcpBuckets = ibucketHitchHike(obj,20,10,&bucketHeader,NoVM);
      ibucketPoach(tcpBuckets,&ClientSocketObject,NoVM);
      ibucketPoach(tcpBuckets,&ServerSocketObject,NoVM);
   }

#if 1
   aClientSocket = (Instance *)clientSocketCreate(0,NoVM);
   vaultAdd(0,aClientSocket,NoVM);

   aServerSocket = (Instance *)serverSocketCreate(NoVM);
   vaultAdd(0,aServerSocket,NoVM);
#else
tests use .isTypeOf(ServerSocket)
   vaultAddData("Network.TCPClientSocket.connectTo",
	methodCreate(Void,0,Client_connectTo,NoVM),NoVM);
   vaultAddData("Network.TCPServerSocket.open",
	methodCreate(Void,0,Server_open,NoVM),NoVM);
#endif
}

/* vault.c : The Vault management
 * The Vault is a global repository that holds Top Classes and Natives.
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <string.h>

#define __NOT_A_DLL
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklClass.h"
#include "zklDictionary.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"

static void buildVCache(pVM);

typedef struct
{
   Instance  instance;		// Inherit from Instance
   Instance *contents;		// a Dictionary
} Vault;

static ZKL_Object VaultObject;
static Vault	  theVault, plainJaneVault;
static SpinLock   cacheLock;

#define CONTENTS	theVault.contents

    // --> 0|obj (vm==1) or throws
Instance *vaultFind(char *vaultPath,int bitch,pVM vm)
{
   ENTRY *entry;

   if (!*vaultPath) return (Instance *)&theVault;
   entry = dictionarySearchFor(CONTENTS,vaultPath,vm);
   if (entry) return entry->value;

   if(bitch)
   {
      char buf[200];
	// don't know strlen(vaultPath) @ compile time
      sprintf(buf,"Search the Vault: Can't find %.150s",vaultPath);
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   return 0;
}

    // .resolve(name): -->True|throws, check contents for name
    // .resolve(name,8) -->BOOL, check contents for name
    // .resolve(name,N|*) -->N, check M/P for name
    // TheVault.resolve doesn't get here but v:=TheVault; v.resolve does.
    //   Which is what the compiler does for checking
    // Otherwise, just use find
static Instance *Vault_resolve(Instance *self,pArglist arglist,pVM vm)
{
   if (self == (Instance *)&plainJaneVault)
      return Object_resolveName(self,arglist,vm);

   {
      char     *path = arglistGetOnlyString(arglist,0,0,vm);
      Instance *p1   = arglistTryToGet(arglist,1);
      if (p1)
      {
	 if (TYPEO(p1) == IntType && 0x8 == (int)convertToInt(p1,vm))
	    return vaultFind(path,0,vm) ? BoolTrue : BoolFalse;
	 return Object_resolveName(self,arglist,vm);
      }
      return vaultFind(path,1,vm);
   }
}

    // .find(name) --> ? | Void
static Instance *Vault_find(Instance *self,pArglist arglist,pVM vm)
{
   char	    *path = arglistGetOnlyString(arglist,0,"Vault.find",vm);
   Instance *x;

   if (0 == strncmp(path,"TheVault.",9)) path += 9;
   x = vaultFind(path,0,vm);
   return x ? x : Void;
}

    // .bestFit(path)
    // Look through EVERY vault entry and find the longest name match
static int _bestFit(DX *dx, Instance *k, pVM vm, size_t nth)
{
   char  *path = (char *)dx->x;
   char  *key  = stringText(k);	// String or KString
   int	  n    = 0;

   while (*path && *key && *path == *key) { path++; key++; n++; }
   if (n && *key == '\0' && (*path == '\0' || *path == '.'))
   {
      dx->vm = vm;
      if (*path == '\0') { dx->n = n; return 1; }	// exact match
      if (*path == '.' && n > dx->n) dx->n = n;		// better fit
   }

   return 1;
}
static int vaultBestFit(char *path, Instance **klass)
{
   DX  dx = { (Instance *)path,0,0 };
   int n = 0;

#if 0
   if (0 == strncmp(path,"TheVault.",9))
      { dx.x = (Instance *)(path + 9); n = 9; }
#endif

   dWalk(CONTENTS,(DhashCb)_bestFit,&dx,0);
   if (dx.n)
   {
      if (klass) *klass = (Instance *)dx.vm;
      return dx.n + n;
   }
   return 0;
}
static Instance *Vault_bestFit(Instance *self,pArglist arglist,pVM vm)
  { return intCreate(vaultBestFit(arglistGetOnlyString(arglist,0,0,vm),0),vm); }

    // if path == 0/1/2, use obj.vaultPath
    // if path == 2 & class & !vaultPath, don't add
void vaultAdd(char *vaultPath, Instance *classOrNative, pVM vm)
{
   Instance *name;

   if (vaultPath <= (char *)2)
   {
      if (TYPEO(classOrNative) == ClassType)
      {
	 char *vp = classVaultPath(classOrNative);
	 if (vaultPath == (char *)2 && !*vp) return;
	 vaultPath = vp;
      }
      else vaultPath = VAULT_PATH(classOrNative);
   }

   if (*vaultPath) name = stringCat(vm,vaultPath,".",iname(classOrNative),(char *)0);
   else name = kStringCreate(iname(classOrNative),classOrNative,I_OWNED,vm);
   dictionaryAdd(CONTENTS,name,classOrNative,vm);
instanceIsOrphan(name);	//!!!!?????

   buildVCache(vm);
}

    // .add(Class|Native, [path])
static Instance *Vault_add(Instance *self,pArglist arglist,pVM vm)
{
   Instance *classOrNative = arglistGet(arglist,0,"Vault.add",vm);
   char	    *path	   = arglistTryToGetString(arglist,1,0,vm);

   if ((TYPEO(classOrNative) != NativeType)	&&
       (TYPEO(classOrNative) != ClassType))
   {
      char buf[100];
      sprintf(buf,"TheVault.add(%.70s): Not a Class or Native type",
	 iname(classOrNative));
      vmThrow(vm,E_VALUE_ERROR,buf);
   }

   vaultAdd(path,classOrNative,vm);
   instanceIsOrphan(classOrNative);
   return BoolTrue;
}

void vaultAddData(char *vaultPath, Instance *data, pVM vm)
{
   Instance *name = stringCreate(vaultPath,I_OWNED,vm);
   dictionaryAdd(CONTENTS,name,data,vm);
instanceIsOrphan(data);	// data may be from another thread
}

    // .addAs(obj,path)
static Instance *Vault_addAs(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i    = arglistGet(arglist,0,"Vault.addAs",vm);
   char	    *path = arglistGetOnlyString(arglist,1,"Vault.addAs",vm);
   vaultAddData(path,i,vm);
   return BoolTrue;
}

static int _cmpValue(DX *dx, Instance *key, Instance *x, size_t n)
{
   Instance *klass = dx->x;

   if (TYPEO(klass) == ClassType)
   {
      if (Class_eq(klass,x,dx->vm) == BoolTrue) return 0;
   }
   else		// Native && system (List, etc)
      if (klass == x) return 0;

   return 1;
}
static ENTRY *holds(Instance *klass, pVM vm)
{
   DX dx = { klass,vm };

   return dWalk(CONTENTS,(DhashCb)_cmpValue,&dx,vm);
}

    // .holds(<root class> | object)
static Instance *Vault_holds(Instance *self,pArglist arglist,pVM vm)
{
   Instance *klass = arglistGet(arglist,0,"Vault.holds",vm);
   return boolCreate(holds(klass,vm) != 0);
}

static int _closeEnough(DX *dx, Instance *key, Instance *value, size_t n)
{
   Instance *i = dx->x;

   switch(dx->n)
   {
      case ClassType:  if (classIsInstanceOf(i,value)) return 0; break;
      case FcnType:    if (fcnIsInstanceOf(i,value))   return 0; break;
      case MethodType:
      case VaultType:  if (i == value)		       return 0; break;
      default:	// Native && system (List, etc)
	 if (objectIsType(i,value)) return 0;
	 break;
   }
   return 1;	// nope
}
Instance *vaultBackTrace(Instance *i, pVM vm)
{
   DX	  dx = { i,vm,TYPEO(i),0 };
   ENTRY *entry = dWalk(CONTENTS,(DhashCb)_closeEnough,&dx,vm);

//   return entry ? (Instance *)entry->key : Void;
   if (entry) return stringCat(vm,"TheVault.",ZKL_STRING(entry->key),(char *)0);
   return Void;
}

    // .path(i) --> "path" | Void
static Instance *Vault_backTrace(Instance *self,pArglist arglist,pVM vm)
   { return vaultBackTrace(arglistGet(arglist,0,0,vm),vm); }

    // .dir(outputClass=Console) --> Utils.Helpers.vaultDir(out)
static Instance *Vault_dir(Instance *self,pArglist arglist,pVM vm)
{
   if (self == (Instance *)&plainJaneVault)
      return fcnRunFromClass(Utils_Helpers,"vaultDir",arglist,vm);
   return Object_dir(self,arglist,vm);
}

    /* .vaultChase(path)
     * Change a reference through the Vault
     * eg chase("Exception.IndexError"), chase("Walker.Walker"),
     *    chase("TheVault"), chase("TheVault.Walker.Walker")
     * Will find vars, classes, etc
     */
Instance *vaultChase(char *vaultPath, pVM vm)
{
   char	     buf[100], *ptr;
   ENTRY    *entry;
   Instance *klass;
   int	     n;

   	// I'll match "TheVault" later
   if (0 == strncmp(vaultPath,"TheVault.",9)) vaultPath += 9;

   entry = dictionarySearchFor(CONTENTS,vaultPath,vm);  // look for exact match
   if (entry) return entry->value;

   n = vaultBestFit(vaultPath,&klass);
   if (!n)
   {
      sprintf(buf,"TheVault.chase(%.80s)",vaultPath);
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   ptr = vaultPath + n;
   if (!*ptr) return klass;
   while(*ptr && klass)
   {
      size_t	n;

      if (*ptr == '.') ptr++;
      for (n = 0; n < 81 && *ptr && *ptr != '.'; n++) buf[n] = *ptr++;
      if (n > 80) vmThrow(vm,E_VALUE_ERROR,"TheVault.chase");
      buf[n] = '\0';
      IRESOLVE(klass)(&klass,buf,0,0,1,vm);  // throws, esp class
   }
   return klass;
}
Instance *Vault_chase(Instance *self,pArglist arglist,pVM vm)
   { return vaultChase(arglistGetOnlyString(arglist,0,0,vm),vm); }

    // Find things like ("Walker",".Walker.next")
    // !!! half assed vm == 1
Instance *vaultChase2(char *vaultPath,char *path, pVM vm)
{
   char	     buf[100], *ptr;
   Instance *klass;

   klass = vaultFind(vaultPath,1,vm);
   if (!klass) return 0;	// vm == 1

   ptr = path;
   while(*ptr && klass)
   {
      size_t	n;

      if (*ptr == '.') ptr++;
      for (n = 0; n < 81 && *ptr && *ptr != '.'; n++) buf[n] = *ptr++;
      if (n > 80) vmThrow(vm,E_VALUE_ERROR,"TheVault.chase");
      buf[n] = '\0';
      IRESOLVE(klass)(&klass,buf,0,0,1,vm);	// throws, esp class
   }
   return klass;
}

/* ******************************************************************** */
/* *************************** Vault Cache **************************** */
/* ******************************************************************** */

static struct V_CACHE { char *name; Instance *i; } vCache[] = 
{
   // !!IMPORTANT!! You can *NOT* change the order or shrink this list!
   // It *MUST* remain consistent for ALL VMs (it can grow however).
   // Or compiled code will blow a gasket
   // Hopefully, it is built earily enough that the Vault can't be poisoned
   "Exception",	 0,
   "List",	 0,
   "ROList",	 0,
   "Dictionary", 0,
   "Console",	 0,
};

#define VC_COUNT	 ( sizeof(vCache) / sizeof(vCache[0]) )

static CAtomicInt cacheIsBuilt;

static void buildVCache(pVM vm)	// re-entrant
{
   if (cacheIsBuilt) return;
   spinLockAcquire(&cacheLock);
      if (!cacheIsBuilt)   // chould have been built while waiting for lock
      {
	 int n,built = 1;
	 struct V_CACHE *cache;

	 for (n = 0; n < VC_COUNT; n++)
	 {
	    cache = &vCache[n];
	    if (!cache->i)
	    {
	       ENTRY *entry;
	       entry = dictionarySearchFor(CONTENTS,cache->name,vm);
	       if (entry) cache->i = entry->value;
	       else       built = 0;
	    }
	 }
	 CAI_SET(&cacheIsBuilt,built);  // cacheIsBuilt = built;
      }
   SPIN_LOCK_RELEASE(&cacheLock);
}

Instance *vcGet(int n,pVM vm)
{
   if (!cacheIsBuilt)
      vmThrow(vm,E_VM_ERROR,"opVCache.VCache: empty slot");
   if (n < 0 || n >= VC_COUNT)
      vmThrow(vm,E_VM_ERROR,"opVCache.VCache: n out of range");

   return vCache[n].i;		// I want the VM to crash if 0?
}

    // .cache()  --> Tuple
    // .cache(i) --> Void | n	
    // .cache(Void,n)   --> i
    // .cache(Void,name) --> Void | n
static Instance *Vault_cache(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = 0, *cache;
   int	     n;

   buildVCache(vm);

   if ((i = arglistTryToGet(arglist,1)))	// .cache(?,n | name)
   {
      int     t = TYPEO(i);
      if (t == IntType) return vcGet((int)convertToInt(i,vm),vm);  // n
      if (t == StringType)					   // name
      {
	 char  *name = ZKL_STRING(i);
	 ENTRY *entry;
	 entry = dictionarySearchFor(CONTENTS,name,vm);
	 if (!entry) return Void;
	 i = entry->value;
	 // fall through
      }
      else return Void;
   }
   else i = arglistTryToGet(arglist,0);

   if (i)					// .cache(i)
   {
      for (n = 0; n < VC_COUNT; n++)
      	if (i == vCache[n].i) return intCreate(n,vm);
      return Void;
   }
   						// .cache() --> contents
   cache = tupleCreate(VC_COUNT,I_OWNED,vm);
   for (n = 0; n < VC_COUNT; n++)
   {
      i = vCache[n].i;
      if (!i) i = Void;
      tupleAppend(cache,i);
   }
   return cache;
}



static const MethodTable vaultMethods[] =
{
   "add",	(pMethod)Vault_add,
   "addAs",	(pMethod)Vault_addAs,
   "resolve",	(pMethod)Vault_resolve,
   "find",	(pMethod)Vault_find,
   "holds",	(pMethod)Vault_holds,
   "path",	(pMethod)Vault_backTrace,
   "bestFit",	(pMethod)Vault_bestFit,
   "chase",	(pMethod)Vault_chase,
   "cache",	(pMethod)Vault_cache,
"dir",	(pMethod)Vault_dir,
   0,		0
};

#if 0
static const MethodTable pjvMethods[] =	// same as above, no resolve + dir
{
   "add",	(pMethod)Vault_add,
   "addAs",	(pMethod)Vault_addAs,
   "find",	(pMethod)Vault_find,
   "holds",	(pMethod)Vault_holds,
   "path",	(pMethod)Vault_backTrace,
   "bestFit",	(pMethod)Vault_bestFit,
   "dir",	(pMethod)Vault_dir,
   "chase",	(pMethod)Vault_chase,
   "cache",	(pMethod)Vault_cache,
   0,		0
};
#endif

////////////////////// Vault Properties //////////////////////////

    // .contents --> L(keys)
static Instance *Vault_contents(Instance *self, pVM vm)
   { return Dictionary_keys(CONTENTS,vm); }

    // .vault --> self
static Instance *vault_vault(Instance *self, pVM vm)
   { return (Instance *)&plainJaneVault; }

static const PropertyTable vaultProperties[] =
{
   "contents",		(pProperty)Vault_contents,
   "vault",		(pProperty)vault_vault,
   0,			0
};



/////////////////////////////////////////////////////////

    /* This is done very early, be careful
     * Note:  Since I create the Vault statically, instanceAllocate() is
     * never called, thus the Vault is INVISIBLE, not touched by GC
     */
void vaultConstruct(void)
{
   CAI_INIT(&cacheIsBuilt);
   spinLockInit(&cacheLock);

   constructObject(&VaultObject,VaultType,vaultMethods,vaultProperties,0,NoVM);
   instanceInit((Instance *)&theVault,&VaultObject,I_UNTOUCHABLE);

   // DON'T set VaultObject.magicMarker, Dictionary marker takes care of things
   // GC never actually sees the Vault, just the contents

   theVault.contents = dictionaryCreate(50,I_IMMORTAL,NoVM);

   vaultAdd(0,(Instance *)&theVault,NoVM);	// Put TheVault in the Vault

   	// Create a Vault object that doesn't hijack resolve
        // so I don't have to type so much when I'm playing in the vault
   instanceInit((Instance *)&plainJaneVault,&VaultObject,I_UNTOUCHABLE);
   vaultAddData("Vault",(Instance *)&plainJaneVault,NoVM);
}

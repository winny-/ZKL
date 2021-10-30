/* This file is machine ganerated
 *    (from vm.h.zkl and opcode.h.zkl by op2C.zkl).
 * Don't mess with it, your efforts will be in vain.
 * Generated Fri Jun 16 10:20:49 2017
 */

#include "zklOpcodes.h"	// prototype for typeToName

char *typeTable[] =
{
   "Object",           	//  0: ObjectType
   "Void",             	//  1: VoidType
   "Class",            	//  2: ClassType
   "Fcn",              	//  3: FcnType
   "Dictionary",       	//  4: DictionaryType
   "List",             	//  5: ListType
   "String",           	//  6: StringType
   "Int",              	//  7: IntType
   "Float",            	//  8: FloatType
   "PointerInt",       	//  9: PtrIntType
   "Bool",             	// 10: BoolType
   "Data",             	// 11: DataType
   "Method",           	// 12: MethodType
   "Native",           	// 13: NativeType
   "VM",               	// 14: VMType
   "ROList",           	// 15: TupleType
   "???",              	// 16: 
   "AtomicBool",       	// 17: AtomicBoolType
   "AtomicInt",        	// 18: AtomicIntType
   "File",             	// 19: FileType
   "Lock",             	// 20: LockType
   "WriteLock",        	// 21: WriteLockType
   "Deferred",         	// 22: DeferredType
   "Op",               	// 23: OpType
   "TheVault",         	// 24: VaultType
   "Walker",           	// 25: WalkerType
   "Property",         	// 26: PropertyType
};

char *typeToName(unsigned int t) {
   if(t > 26) return "Bogus";
   return typeTable[t];
}

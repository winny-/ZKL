/* This file is machine ganerated
 *    (from vm.h.zkl and opcode.h.zkl by op2C.zkl).
 * Don't mess with it, your efforts will be in vain.
 * Generated Fri Jun 16 10:20:49 2017
 */

#ifndef __ZKL_OPCODES_H
#define __ZKL_OPCODES_H

#define ZSC_PROTOCOL	"1.1"
#define WAD_PROTOCOL	"1.0"

#define ZSC_MAGIC_COOKIE	0x7a6b6c
#define WAD_MAGIC_COOKIE	0x776164
#define CODE_TAG		0x81
#define FCN_TAG			0x42
#define CLASS_TAG		0x03
#define DATA_TAG		0x5a
#define WAD_TAG		0xaa

char *typeToName(unsigned int t);	// in typeTable.c

#define opAdd                70
#define opAddToArgs         132
#define opAllocRegisters    150
#define opArg0              210
#define opArg1              211
#define opArg2              212
#define opArgN              130
#define opBlockUp           168
#define opBlockUp1          213
#define opBlockUp2          214
#define opBreak               4
#define opBreak1              1
#define opBreak2              2
#define opBreak3              3
#define opCallIMethodN      125
#define opCallIMethodNZ     120
#define opCallNB            118
#define opCallNBZ           117
#define opCallNW            116
#define opCallNWZ           115
#define opCallOMethodN      126
#define opCallOMethodNZ     119
#define opCallResult        124
#define opCallResultZ       123
#define opCatch22             8
#define opChexit            110
#define opClassN            146
#define opClassNI           147
#define opClassUp           169
#define opCreateBlock        35
#define opData               28
#define opDecReg             79
#define opDiv                74
#define opDone                0
#define opEQ                 51
#define opFalse              21
#define opFcnN              148
#define opFcnNI             149
#define opFloat              26
#define opGT                 55
#define opGTE                56
#define opGetKString         27
#define opGetReg            151
#define opGetReg0           220
#define opGetReg1           221
#define opGetReg2           222
#define opGetReg3           223
#define opGetReg4           224
#define opGetReg5           225
#define opGetRegI           153
#define opGetRegI0          215
#define opGetRegI1          216
#define opGetVar            156
#define opGetVarI           158
#define opGetX              136
#define opHopBack           209
#define opHopFalse          208
#define opHopFwd            206
#define opHopTrue           207
#define opIEEE754            29
#define opIPropertyN        127
#define opIncReg             78
#define opInt                24
#define opIntB              203
#define opIntW              204
#define opInterrupt           5
#define opJmpBack            12
#define opJmpFalse           13
#define opJmpFwd             11
#define opJmpTrue            14
#define opLT                 53
#define opLTE                54
#define opMVR               105
#define opMagic             255
#define opMarkArglist       133
#define opMod                75
#define opMul                71
#define opNEQ                52
#define opNegate             77
#define opNoText            202
#define opNoop                6
#define opNot                76
#define opOne               201
#define opParentN           160
#define opParentNI          161
#define opPop               101
#define opPop1              106
#define opPopBlock           36
#define opPush              100
#define opReCall            141
#define opReCallZ           122
#define opRegPlus            80
#define opResolve           140
#define opResolveNB         143
#define opResolveNW         144
#define opSelf              171
#define opSetArgN           131
#define opSetReg            152
#define opSetReg0           226
#define opSetReg1           227
#define opSetReg2           228
#define opSetReg3           229
#define opSetReg4           230
#define opSetReg5           231
#define opSetRegI           154
#define opSetVar            157
#define opSetVarI           159
#define opSetX              137
#define opSetXPop           102
#define opSquirt            104
#define opSrcLineB           30
#define opSrcLineW           31
#define opStar              205
#define opStompIId          145
#define opString             23
#define opSub                72
#define opSwap              103
#define opTailCall          129
#define opTailCallZ         121
#define opTailRecurse       128
#define opTheVault          172
#define opThrow               7
#define opTrue               20
#define opVCache            173
#define opVM                174
#define opVoid               22
#define opZero              200


#define AtomicBoolType       17
#define AtomicIntType        18
#define BoolType             10
#define ClassType             2
#define ClassVarType        103
#define DataType             11
#define DeferredType         22
#define DictionaryType        4
#define FcnType               3
#define FileType             19
#define FloatType             8
#define IntType               7
#define ListType              5
#define LockType             20
#define MethodType           12
#define NativeType           13
#define ObjectType            0
#define OpType               23
#define ParentType          112
#define PropertyType         26
#define PtrIntType            9
#define StringType            6
#define TupleType            15
#define UnknownType          99
#define VMType               14
#define VaultType            24
#define VoidType              1
#define WalkerType           25
#define WriteLockType        21

#endif // __ZKL_OPCODES_H

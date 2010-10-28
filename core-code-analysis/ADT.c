#include "rts.h"
#include "bc/interpreter.h"

 
static Word8 ADT_bytePool[] =
{ 0x41,0x44,0x54,0x2e,0x42,0x61,0x72,0x00
, 0x03,0x04
};
 
static GB_LinkChainResolvedInfo ADT_linkChainIndirections[] =
{ };
 
static GCStackInfo ADT_gcStackInfos[] =
{ { 3
  , 2
  , &(ADT_bytePool[8])
  }};
 
static FunctionInfo ADT_functionInfos[] =
{ {8,FunctionInfoFlag_None,&(ADT_bytePool[0])}};
 
static CallInfo ADT_callinfos[] =
{ };
 
static GB_Byte ADT_bytecode[] =
{    0xfe,0xff
,    0xa1,0x03,0x00,0x00
,    0x20,0x08
,    0x0a,0x00,0x00,0x02,0x00
,    0x08,0x08
,    0xec,0x05,0x00,0x01,0x00
,    0xf4,0x00,0x04,0x04
};
GB_ByteCodeModule ADT_bytecodeModule =
  { "ADT"
  , NULL
  , 0
  , ADT_bytecode
  , 24
  } ;
 
static GB_Word ADT_constants[] =
{ };
 
static HalfWord ADT_cafGlEntryIndices[] =
{ };
 
static GB_BytePtr ADT_globalEntries[] =
{ &(ADT_bytecode[6])};
GB_NodePtr ADT_expNode ;
static int ADT_expNode_offs[] =
  { 0 } ;
int ADT_expNode_size = 1 ;

static GB_ImpModEntry ADT_impMods[] =
         { { "Prelude"
           , 0 
           }} ;

void ADT_initModule(GB_ModEntry* modTbl, Word modTblInx) {
  gb_InitTables( ADT_bytecode
               , 24
               , ADT_cafGlEntryIndices
               , 0
               , ADT_globalEntries
               , 1
               , ADT_constants
               , ADT_gcStackInfos
               , ADT_linkChainIndirections
               , ADT_callinfos
               , 0
               , ADT_functionInfos
               , 1
               , ADT_bytePool
               , 2
               , ADT_impMods
               , 1
               , &(ADT_expNode)
               , ADT_expNode_size
               , ADT_expNode_offs
               , modTbl
               , modTblInx
               ) ;
}


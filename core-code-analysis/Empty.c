#include "rts.h"
#include "bc/interpreter.h"

 
static Word8 Empty_bytePool[] =
{ };
 
static GB_LinkChainResolvedInfo Empty_linkChainIndirections[] =
{ };
 
static GCStackInfo Empty_gcStackInfos[] =
{ };
 
static FunctionInfo Empty_functionInfos[] =
{ };
 
static CallInfo Empty_callinfos[] =
{ };
 
static GB_Byte Empty_bytecode[] =
{    0xfe,0xff};
GB_ByteCodeModule Empty_bytecodeModule =
  { "Empty"
  , NULL
  , 0
  , Empty_bytecode
  , 2
  } ;
 
static GB_Word Empty_constants[] =
{ };
 
static HalfWord Empty_cafGlEntryIndices[] =
{ };
 
static GB_BytePtr Empty_globalEntries[] =
{ };
GB_NodePtr Empty_expNode ;
static int Empty_expNode_offs[] =
  { } ;
int Empty_expNode_size = 0 ;

static GB_ImpModEntry Empty_impMods[] =
         { { "Prelude"
           , 0 
           }} ;

void Empty_initModule(GB_ModEntry* modTbl, Word modTblInx) {
  gb_InitTables( Empty_bytecode
               , 2
               , Empty_cafGlEntryIndices
               , 0
               , Empty_globalEntries
               , 0
               , Empty_constants
               , Empty_gcStackInfos
               , Empty_linkChainIndirections
               , Empty_callinfos
               , 0
               , Empty_functionInfos
               , 0
               , Empty_bytePool
               , 0
               , Empty_impMods
               , 1
               , &(Empty_expNode)
               , Empty_expNode_size
               , Empty_expNode_offs
               , modTbl
               , modTblInx
               ) ;
}


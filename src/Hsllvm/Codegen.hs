{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hsllvm.Codegen
  ()
where

import Control.Monad.State
import LLVM.AST
import LLVM.AST.Global

import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as Link
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.CallingConvention as CallConv

type Map = Map.Map

type SymbolTable = [(String, Operand)]

-- Names of registers in LLVM

type Names = Map String Int

-- | This state representation will be embedded into state monad.
data CodegenState = CodegenState
  { currentBlock :: Name
  , blocks :: Map Name BlockState
  , symtab :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names
  } deriving (Show)

data BlockState = BlockState
  { idx :: Int
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving (Show)

newtype Codegen a = Codegen
  { runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

-- | The first bytestring indicate the name of module.
-- TODO: maybe we will need a conversion here.
emptyModule :: BS.ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn def = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [def] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retTy label args body = addDefn . GlobalDefinition $ functionDefaults
  { name        = packName label
  , parameters  = ([ Parameter ty nm [] | (ty, nm) <- args ], False)
  , returnType  = retTy
  , basicBlocks = body
  }

-- | Declaration for 'extern'
external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retTy label args = addDefn . GlobalDefinition $ functionDefaults
  { name        = packName label
  , linkage     = Link.External
  , parameters  = ([ Parameter ty nm [] | (ty, nm) <- args ], False)
  , returnType  = retTy
  , basicBlocks = []
  }

-- Blocks 

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock blockName = do
  blks <- gets blocks
  cnt  <- gets blockCount
  nms  <- gets names

  let
    newBlk          = BlockState cnt [] Nothing
    (qname, supply) = resolveName blockName nms

  modify $ \s ->
    s { blocks = Map.insert (packName qname) newBlk blks, blockCount = cnt + 1, names = supply }

  return (packName qname)

setBlock :: Name -> Codegen Name
setBlock blockName = do
  modify $ \s -> s { currentBlock = blockName }
  return blockName

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock newBlk = do
  curr <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert curr newBlk (blocks s) }

current :: Codegen BlockState
current = do
  curr <- gets currentBlock
  blks <- gets blocks
  case Map.lookup curr blks of
    Just x  -> return x
    Nothing -> error $ "No specified block : " ++ show curr

-- | Create a number only fresh name.
freshName :: Codegen Word
freshName = do
  cnt <- gets count
  modify $ \s -> s { count = cnt + 1 }
  return $ cnt + 1

-- | resolveName ensure that the names are unique in LLVM SSA form.
resolveName :: String -> Names -> (String, Names)
resolveName nm nms = case Map.lookup nm nms of
  Nothing  -> (nm, Map.insert nm 1 nms)
  Just cnt -> (nm ++ show cnt, Map.insert nm (cnt + 1) nms)

-- TODO: we may remove this to reduce conversion cost
packName :: String -> Name
packName = Name . BS.toShort . BSC.pack

doubleTy :: Type
doubleTy = FloatingPointType DoubleFP

-- TODO: extend type defintion here.
local :: Name -> Operand
local = LocalReference doubleTy

global :: Name -> Const.Constant
global = Const.GlobalReference doubleTy

externf :: Name -> Operand
externf = ConstantOperand . Const.GlobalReference doubleTy

assign :: String -> Operand -> Codegen ()
assign var x = do
  stbl <- gets symtab
  modify $ \s -> s { symtab = (var, x) : stbl }

getvar :: String -> Codegen Operand
getvar var = do
  stbl <- gets symtab
  case lookup var stbl of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope : " ++ var

instr :: Instruction -> Codegen Operand
instr ins = do
  nm <- freshName
  let ref = UnName nm
  blk <- current
  let idx = stack blk
  -- := is the llvm DSL for binding instruction.
  modifyBlock $ blk { stack = (ref := ins) : idx }
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator term_ = do
  blk <- current
  modifyBlock $ blk { term = Just term_ }
  return term_

-- See the instruction generation via
-- https://hackage.haskell.org/package/llvm-hs-pure-8.0.0/docs/LLVM-AST-Instruction.html#v:Instruction

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator . Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CallConv.C [] (Right fn) (fmap (\a -> (a, [])) args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

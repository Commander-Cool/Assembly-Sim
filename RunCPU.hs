-- Simulate a very simple CPU

module RunCPU where

import CPU
import Data.Bits
import Data.Array

{-
 - runProgram :: Program -> Data -> Data
 - runProgram prog dat = newDat
 -  where
 -   iterate 
 -}
initialState = CPUState  (0,0,0,0,0,0,0,0)     -- all registers start with value zero
                         0                     -- start execution with the first instruction
                         EQ                    -- result of test starts as equal
                         Nothing
initialData  = listArray (0,10^4) [0..] :: Array Int Int

executeOne :: Instruction -> (CPUState,Data) -> (CPUState,Data)
executeOne (Load target addr1 addr2) (cpuState,dat) = nextInstruction (newCpuState, dat)
  where
    loadValue   = dat ! (addr1' + addr2')
    addr1'      = getRegisterVal addr1  cpuState
    addr2'      = getRegisterVal addr2  cpuState
    newCpuState = changeRegister target loadValue cpuState

executeOne (Store regNum addr1 addr2) (cpuState,dat) = nextInstruction (cpuState, newDat)
  where
    newDat = dat // [(addr1' + addr2', value3)]
    addr1' = getRegisterVal addr1  cpuState
    addr2' = getRegisterVal addr2  cpuState
    value3 = getRegisterVal regNum cpuState

executeOne (LoadImmediate target value) (cpuState,dat) = nextInstruction
      (changeRegister target value cpuState, dat)

executeOne (Add target arg1 arg2) (cpuState,dat) = nextInstruction
      (changeRegister  target 
                       ((getRegisterVal arg1 cpuState) + (getRegisterVal arg2 cpuState)) 
                       cpuState
      , dat)
executeOne (Multiply target arg1 arg2) (cpuState,dat) = nextInstruction
      (changeRegister  target 
                       ((getRegisterVal arg1 cpuState) * (getRegisterVal arg2 cpuState)) 
                       cpuState
      , dat)

executeOne (And target arg1 arg2) (cpuState,dat) = nextInstruction
      (changeRegister  target 
                       ((getRegisterVal arg1 cpuState) .&. (getRegisterVal arg2 cpuState)) 
                       cpuState
      , dat)

executeOne (Or target arg1 arg2) (cpuState,dat) = nextInstruction
      (changeRegister  target 
                       ((getRegisterVal arg1 cpuState) .|. (getRegisterVal arg2 cpuState)) 
                       cpuState
      , dat)

executeOne (Not target arg1) (cpuState,dat) = nextInstruction
      (changeRegister  target 
                       (complement (getRegisterVal arg1 cpuState)) 
                       cpuState
      , dat)
executeOne (Rotate target arg1 shift) (cpuState,dat) = nextInstruction
      (changeRegister  target 
                       (rotate (getRegisterVal arg1 cpuState) shift) 
                       cpuState
      , dat)
executeOne (Compare arg1 arg2) (cpuState@(CPUState regs inst _result halted),dat) = nextInstruction
      (CPUState  regs
                 inst
                 (compare (getRegisterVal arg1 cpuState) (getRegisterVal arg2 cpuState))
                 halted
      ,dat)
executeOne (Branch branchOn targetAddress) (cpuState@(CPUState regs inst result halted),dat)
    = (CPUState  regs
                 (if result `elem` branchOn 
                    then let nextInst = (getRegisterVal targetAddress cpuState)
                         in if 0 <= nextInst && nextInst < 4400
                               then nextInst
                               else error $ "tried to jump to " ++ show nextInst 
                    else inst + 1
                 )
                 result
                 halted
      ,dat)
executeOne Halt ((CPUState regs inst result halted),dat) 
    = ((CPUState regs inst result (Just ReachedHalt)),dat)

changeRegister  regNum 
                newValue
                (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8) 
                           currentInstruction
                           comparedAs
                           maybeHalted
                )
  = case regNum of
      1 -> CPUState (newValue,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      2 -> CPUState (reg1,newValue,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      3 -> CPUState (reg1,reg2,newValue,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      4 -> CPUState (reg1,reg2,reg3,newValue,reg5,reg6,reg7,reg8) 
                    currentInstruction comparedAs maybeHalted
      5 -> CPUState (reg1,reg2,reg3,reg4,newValue,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      6 -> CPUState (reg1,reg2,reg3,reg4,reg5,newValue,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      7 -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,newValue,reg8)
                    currentInstruction comparedAs maybeHalted
      8 -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,newValue) 
                    currentInstruction comparedAs maybeHalted
      _ -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8) 
                    currentInstruction comparedAs (Just IllegalRegisterNum)

getRegisterVal  regNum 
                (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8) 
                           _currentInstruction
                           _comparedAs
                           _maybeHalted
                )
  = case regNum of
      0 -> 0
      1 -> reg1
      2 -> reg2
      3 -> reg3
      4 -> reg4
      5 -> reg5
      6 -> reg6
      7 -> reg7
      8 -> reg8
      _ -> error $ "Illegal Register Number at Instr " ++ show _currentInstruction

-- Load and store one 32-bit integer of data to and from memory.
storeData addr val dat = dat // [(addr,val)]
loadData addr dat = dat ! addr

nextInstruction  (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8) 
                            currentInstruction
                            comparedAs
                            maybeHalted
                 ,dat)
  = (CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
    (currentInstruction + 1) comparedAs maybeHalted, dat)

type CurrentInstruction = Int

data HaltedBecause  = ReachedHalt
                    | IllegalRegisterNum 
      deriving (Show,Eq)

data CPUState = CPUState  (RegisterValue, RegisterValue, RegisterValue, RegisterValue,
                           RegisterValue, RegisterValue, RegisterValue, RegisterValue)
                          CurrentInstruction
                          ComparisonResult
                          (Maybe HaltedBecause)
      deriving (Show,Eq)

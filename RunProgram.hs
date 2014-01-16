-- Launch Program For CPU Simulator

module RunProgram where
import RunCPU
import CPU
import Data.Array
import System.Cmd
import System.Info
import System.IO

{- Launch a program with it's initial data (possibly call initialData)
 - and return resulting data
 -}
runProgram :: Program -> Data -> Data
runProgram prog dat = newDat
   where
     newDat = runProgram' prog (initialState,dat)

runProgram' :: Program -> (CPUState,Data) -> Data
runProgram' prog (cpuState@(CPUState regs inst result halted),dat)
          | halted /= Nothing = dat
          | otherwise         = runProgram' prog 
                                            (executeOne (prog inst)
                                                        (cpuState,dat))

-- Generate a Program
genProgram :: Code -> Program
genProgram code addr = instr
   where 
     (_,instr) = code !! addr

traceProgram :: Code -> IO ()
traceProgram code = do outh <- openFile "Trace.txt" WriteMode
                       runTrace (genProgram code) (initialState,initialData) outh 1
                       hClose outh

runTrace :: Program -> (CPUState,Data) -> Handle -> Integer -> IO ()
runTrace prog (cpuState@(CPUState regs inst result halted),dat) outh n

       | halted /= Nothing = hPutStrLn outh $ show halted

       | otherwise = do hPutStrLn outh $ show (n,inst) ++ ": " ++ show (prog inst)
                        hPutStrLn outh $ show regs ++ "\n"
                        runTrace prog (executeOne (prog inst) (cpuState,dat)) outh (n+1)

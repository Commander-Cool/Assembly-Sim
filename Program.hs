-- Program Template

module Program where

import RunProgram
import CPU
import RunCPU

{- Instruction Set
 - ---------------------------
 - Load reg1 reg2 reg3          Load address reg2+reg3 to reg1
 - Store reg1 reg2 reg3         Store reg1 to address reg2+reg3
 - LoadImmediate reg1 val       Load val into reg1
 - Add reg1 reg2 reg3           Put reg2+reg3 in reg1
 - Multiply reg1 reg2 reg3      Put reg2*reg3 in reg1
 - And reg1 reg2 reg3           Put reg2 .& reg3 in reg1
 - Or reg1 reg2 reg3            Put reg2 .| reg3 in reg1
 - Not reg1 reg2                Put .not reg2 in reg1
 - Rotate reg1 reg2 val         Put reg2 rotated by val in reg1
 - Compare reg1 reg2            Compare reg1 reg2 store result in CPUState
 - Branch [comp] reg1           Branch to instruction reg1 on comp (comp = LT,GT,EQ)
 -}

 
 {-if reg1<0 then
		reg1=reg1*(-1)
   else
		reg1=0
	return (-b)
-}

code = [
 -- (0, Instruction1 Reg1 Reg2)
	(0, LoadImmediate 1 5),
	(1, LoadImmediate 2 1),
	(2, LoadImmediate 3 1),
	(3, Multiply 1 2 3),
	(4,Halt)] -- Halt Program

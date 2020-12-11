## Start of header boilerplate #################################################

from aocbase import readInput
import re

oneLinePattern = re.compile(r"^.*$")
wsTokenPattern = re.compile(r"\S+")

def lineParse(s, f, fp):
    m = re.findall(fp, s)
    if m==None:
        raise ValueError("No token patterns found")
    return tuple(map(f, m))

def fileParse(inp, lineparser=lineParse,
                   tokenparser=lambda x:x,
                   tokenPattern=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:x[0] if len(x)==1 else x, 
                    map(lambda x:lineparser(x, tokenparser, tokenPattern),
                        inp.splitlines())))

## End of header boilerplate ###################################################

class InstructionChanger:
   def __init__(self, pc, address, newInstr):
      self._pc = pc
      self._address = address
      self._newInstr = newInstr
   def __enter__(self):
      self._oldInstr = self._pc.changeInstr(self._address, self._newInstr)
      return self
   def __exit__(self, exc_type, exc_val, exc_tb):
      self._pc.changeInstr(self._address, self._oldInstr)
      return False

class PC:

   def reset(self):
      self._pc = 0
      self._hangDetector = set()
      self._acc = 0

   def __init__(self, prg):
      self._prg = [[cmd, int(arg)] for cmd, arg in prg]
      self._prgLen = len(self._prg)
      self.reset()

   def fetch(self):
      return self._prg[self._pc]

   def run(self):
      while not (self.isFinished() or self.isHung()):
         self._hangDetector.add(self._pc)
         self.doCommand()

   def isFinished(self):
      return self._pc < 0 or self._pc >= self._prgLen

   def isHung(self):
      return self._pc in self._hangDetector

   def doCommand(self):
      cmd, arg = self.fetch()
      if cmd in self.instr:
         self.instr[cmd](self, arg)

   def doNop(self, arg):
      self._pc += 1

   def doAcc(self, arg):
      self._acc += arg
      self._pc += 1

   def doJmp(self, arg):
      self._pc += arg

   instr = {"nop":doNop, "acc":doAcc, "jmp":doJmp}

   @property
   def acc(self):
      return self._acc
   
   def findInstr(self, instr):
      for address, (cmd, arg) in enumerate(self._prg):
         if cmd == instr:
            yield address
   
   def swap(self, address, instr):
      return InstructionChanger(self, address, instr)

   def changeInstr(self, address, instr):
      oldInstr = self._prg[address][0]
      self._prg[address] = [instr, self._prg[address][1]]
      return oldInstr

def part1(pinp):
   pc = PC(pinp)
   pc.run()
   return pc.acc

def part2(pinp):
   pc = PC(pinp)
   for fromInstr, toInstr in (("jmp", "nop"), ("nop", "jmp")):
      for address in pc.findInstr(fromInstr):
         with pc.swap(address, toInstr):
            pc.reset()
            pc.run()
            if pc.isFinished():
               return pc.acc

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################

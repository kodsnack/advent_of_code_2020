import '../util/util.dart';

//const String inputFile = 'day8/example.txt';
const String inputFile = 'day8/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  Cpu cpu = Cpu(inputLines);
  return cpu.findAccAtRerun();
}

int calcResultP2(List<String> inputLines) {
  Cpu cpu = Cpu(inputLines);
  return cpu.findAccWithCorrectedProgram();
}

class Cpu {
  List<String> programLines = [];
  int accumulator = 0;
  int executionPoint = 0;

  Cpu(this.programLines);

  int findAccAtRerun() {
    executionPoint = 0;
    accumulator = 0;
    Set<int> executedLines = {};
    while (!executedLines.contains(executionPoint)) {
      final programLine = programLines[executionPoint];
      executedLines.add(executionPoint);
      executeLine(programLine);
    }
    return accumulator;
  }

  int findAccWithCorrectedProgram() {
    bool foundFaultyInstruction = false;
    int changedInstructionPointer = findNopOrJmp(0);

    while (!foundFaultyInstruction) {
      executionPoint = 0;
      accumulator = 0;

      bool ready = false;
      Set<int> executedLines = {};
      switchInstructionAt(changedInstructionPointer);

      while (!ready) {
        final programLine = programLines[executionPoint];
        executedLines.add(executionPoint);
        executeLine(programLine);
        if (executedLines.contains(executionPoint)) ready = true;
        if (executionPoint == programLines.length) ready = true;
      }

      if (executionPoint == programLines.length) {
        foundFaultyInstruction = true;
      } else {
        switchInstructionAt(changedInstructionPointer);
        changedInstructionPointer = findNopOrJmp(changedInstructionPointer + 1);
      }
    }
    return accumulator;
  }

  executeLine(String programLine) {
    String instruction = getInstruction(programLine);
    int argument = int.parse(getArgumentStr(programLine));

    if (instruction == 'nop') {
      executionPoint++;
    }

    if (instruction == 'acc') {
      accumulator += argument;
      executionPoint++;
    }

    if (instruction == 'jmp') {
      executionPoint += argument;
    }
  }

  String getInstruction(String line) {
    return line.split(' ')[0];
  }

  String getArgumentStr(String line) {
    return line.split(' ')[1];
  }

  int findNopOrJmp(int start) {
    String instruction = getInstruction(programLines[start]);
    while (instruction != 'nop' && instruction != 'jmp') {
      start++;
      instruction = getInstruction(programLines[start]);
    }
    return start;
  }

  void switchInstructionAt(int p) {
    String instruction = getInstruction(programLines[p]);
    if (instruction == 'nop') {
      programLines[p] = 'jmp ' + getArgumentStr(programLines[p]);
    }
    if (instruction == 'jmp') {
      programLines[p] = 'nop ' + getArgumentStr(programLines[p]);
    }
  }
}

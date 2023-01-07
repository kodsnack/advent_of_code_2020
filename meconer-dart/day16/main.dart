import '../util/util.dart';

//const String inputFile = 'day16/example.txt';
const String inputFile = 'day16/input.txt';

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
  List<Range> ranges = [];
  List<int> myTicketValues = [];
  List<List<int>> nearbyTickets = [];
  int section = 0;

  for (final line in inputLines) {
    // Empty lines means new section
    if (line.isEmpty) {
      section++;
      continue;
    }

    if (section == 0) {
      // Valid ranges
      final rangeName = line.split(': ')[0];
      final rangeStrs = line.split(': ')[1].split(' or ');
      Range range = Range(rangeName);
      for (final rangeStr in rangeStrs) {
        int startVal = int.parse(rangeStr.split('-')[0]);
        int endVal = int.parse(rangeStr.split('-')[1]);
        range.range.add([startVal, endVal]);
      }
      ranges.add(range);
    }

    if (section == 1) {
      // My ticket
      if (line.startsWith('your')) continue;
      myTicketValues = line.split(',').map((e) => int.parse(e)).toList();
    }

    if (section == 2) {
      // Nearby tickets
      if (line.startsWith('nearby')) continue;
      List<int> nearbyTicketValues =
          line.split(',').map((e) => int.parse(e)).toList();
      nearbyTickets.add(nearbyTicketValues);
    }
  }

  int ticketScanningErrorRate = 0;
  for (final ticket in nearbyTickets) {
    for (final value in ticket) {
      if (!isValid(value, ranges)) ticketScanningErrorRate += value;
    }
  }

  return ticketScanningErrorRate;
}

bool isValid(int value, List<Range> rangeTypes) {
  for (final rangeType in rangeTypes) {
    for (final range in rangeType.range) {
      if (value >= range[0] && value <= range[1]) return true;
    }
  }
  return false;
}

class Range {
  String rangeName;
  List<List<int>> range = [];
  Range(this.rangeName);
}

int calcResultP2(List<String> inputLines) {
  Map<String, List<List<int>>> ranges = {};
  List<int> myTicketValues = [];
  List<List<int>> nearbyTickets = [];
  int section = 0;

  for (final line in inputLines) {
    // Empty lines means new section
    if (line.isEmpty) {
      section++;
      continue;
    }

    if (section == 0) {
      // Valid ranges
      final rangeName = line.split(': ')[0];
      final rangeStrs = line.split(': ')[1].split(' or ');
      ranges[rangeName] = [];
      for (final rangeStr in rangeStrs) {
        int startVal = int.parse(rangeStr.split('-')[0]);
        int endVal = int.parse(rangeStr.split('-')[1]);
        ranges[rangeName]!.add([startVal, endVal]);
      }
    }

    if (section == 1) {
      // My ticket
      if (line.startsWith('your')) continue;
      myTicketValues = line.split(',').map((e) => int.parse(e)).toList();
    }

    if (section == 2) {
      // Nearby tickets
      if (line.startsWith('nearby')) continue;
      List<int> nearbyTicketValues =
          line.split(',').map((e) => int.parse(e)).toList();
      nearbyTickets.add(nearbyTicketValues);
    }
  }

  // Before checking starts we set all field types as possible
  List<Set<String>> SetOfPossibleFields = [];
  for (int i = 0; i < nearbyTickets[0].length; i++) {
    Set<String> possibleFields = ranges.keys.toSet();
    SetOfPossibleFields.add(possibleFields);
  }

  // Go through all nearby tickets and remove the possible fields
  // if the value is out of range for the field
  for (final ticket in nearbyTickets) {
    bool ticketIsValid = true;
    for (int value in ticket) {
      if (!allFieldsAreValid(value, ranges)) ticketIsValid = false;
    }

    if (!ticketIsValid) continue;

    for (int fieldNo = 0; fieldNo < ticket.length; fieldNo++) {
      Set<String> newPossibleFields = {};
      for (final field in SetOfPossibleFields[fieldNo]) {
        int valueToTest = ticket[fieldNo];
        if (isValidP2(valueToTest, field, ranges)) newPossibleFields.add(field);
      }
      SetOfPossibleFields[fieldNo] = newPossibleFields;
    }
  }

  // Now some field no:s can have more than one possible field type but
  // then there should be field types that occur only once. Lets find these
  bool ready = false;
  while (!ready) {
    for (final key in ranges.keys) {
      int keyCount = 0;
      int lastFoundFieldNo = 0;
      for (int fieldNo = 0; fieldNo < nearbyTickets[0].length; fieldNo++) {
        if (SetOfPossibleFields[fieldNo].contains(key)) {
          keyCount++;
          lastFoundFieldNo = fieldNo;
        }
      }

      if (keyCount == 1) SetOfPossibleFields[lastFoundFieldNo] = {key};
    }
    // If we can have only one possible field type for all fields we are done
    ready = true;
    for (int fieldNo = 0; fieldNo < nearbyTickets[0].length; fieldNo++) {
      if (SetOfPossibleFields[fieldNo].length > 1) {
        ready = false;
      }
    }
  }

  // Now there should be only one possible field type for all fields.
  // Find the field no's that starts with the word departure
  List<int> departureFieldNos = [];
  for (int fieldNo = 0; fieldNo < nearbyTickets[0].length; fieldNo++) {
    if (SetOfPossibleFields[fieldNo].first.startsWith('departure')) {
      departureFieldNos.add(fieldNo);
    }
  }

  int result = 1;
  for (int departureFieldNo in departureFieldNos) {
    result *= myTicketValues[departureFieldNo];
  }

  return result;
}

bool allFieldsAreValid(int valToTest, Map<String, List<List<int>>> ranges) {
  for (final rangeList in ranges.entries) {
    for (final range in rangeList.value) {
      if (valToTest >= range[0] && valToTest <= range[1]) return true;
    }
  }
  return false;
}

bool isValidP2(
    int valueToTest, String field, Map<String, List<List<int>>> ranges) {
  final rangeToTest = ranges[field];
  for (final range in rangeToTest!) {
    if (valueToTest >= range[0] && valueToTest <= range[1]) return true;
  }
  return false;
}

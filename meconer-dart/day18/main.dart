import '../util/util.dart';

//const String inputFile = 'day18/example.txt';
const String inputFile = 'day18/input.txt';

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
  int sum = 0;
  for (var line in inputLines) {
    TokenProvider tokenProvider = TokenProvider(line);

    final tree = buildTree(tokenProvider);
    int result = evaluateTree(tree);
    sum += result;
    print('$line = $result');
  }
  return sum;
}

int calcResultP2(List<String> inputLines) {
  int sum = 0;
  for (var line in inputLines) {
    TokenProvider tokenProvider = TokenProvider(line);

    final expr = getExpression(tokenProvider);
    int result = evaluateExpr(expr);
    sum += result;
    print('$line = $result');
  }
  return sum;
}

int evaluateExpr(Node expr) {
  final node = expr;
  if (node is ValueNode) return node.value;
  if (node is AddNode) {
    return evaluateExpr(node.leftNode) + evaluateExpr(node.rightNode);
  }
  if (node is MultiplyNode) {
    return evaluateExpr(node.leftNode) * evaluateExpr(node.rightNode);
  }
  print('Expression error');
  return 0;
}

int evaluateTree(List<Node> tree) {
  final node = tree.removeAt(0);
  late int leftValue;
  if (node is SubTree) {
    leftValue = evaluateTree(node.tree);
  } else {
    leftValue = (node as ValueNode).value;
  }
  while (tree.isNotEmpty) {
    late OpType operator;
    Node nextNode = tree.removeAt(0);
    if (nextNode is OperatorNode) {
      operator = nextNode.operator;
    } else {
      print('Must be an operator');
    }
    nextNode = tree.removeAt(0);
    late int rightValue;
    if (nextNode is SubTree) {
      rightValue = evaluateTree(nextNode.tree);
    } else {
      rightValue = (nextNode as ValueNode).value;
    }
    if (operator == OpType.plus) {
      leftValue += rightValue;
    } else {
      leftValue *= rightValue;
    }
  }
  return leftValue;
}

List<Node> buildTree(TokenProvider tokenProvider) {
  List<Node> tree = [];
  var token = tokenProvider.getNextToken();
  while (token.tokenType != TokenType.empty) {
    // Start with a value or a subexpr
    if (token.tokenType == TokenType.value) {
      tree.add(ValueNode(token.value!));
    }
    if (token.tokenType == TokenType.leftParen) {
      // Left paren. We build a subtree.
      tree.add(SubTree(buildTree(tokenProvider)));
    }

    token = tokenProvider.getNextToken();

    // We should have a left node now. Continue with the operator
    while (token.tokenType == TokenType.plus ||
        token.tokenType == TokenType.times) {
      if (token.tokenType == TokenType.plus) {
        tree.add(OperatorNode(OpType.plus));
      } else if (token.tokenType == TokenType.times) {
        tree.add(OperatorNode(OpType.times));
      } else
        print('Should be operator');

      // Now we will have a right node which should be value or another expression
      token = tokenProvider.getNextToken();
      if (token.tokenType == TokenType.leftParen) {
        tree.add(SubTree(buildTree(tokenProvider)));
      }
      if (token.tokenType == TokenType.value) {
        tree.add(ValueNode(token.value!));
      }

      // Now we could have come to the end. If next token is right paren or
      // we are out of tokens we return the expression
      token = tokenProvider.getNextToken();
      if (token.tokenType == TokenType.empty ||
          token.tokenType == TokenType.rightParen) {
        return tree;
      }
      // Otherwise it will be another operator so we continue
    }
  }
  return tree;
}

Node getExpression(TokenProvider tokenProvider) {
  // Expression is a term [* another term]
  // To include * here is not normal math. It is elf math
  Node node = getTermNode(tokenProvider);

  var token = tokenProvider.getNextToken(remove: false);

  while (token.tokenType == TokenType.times) {
    tokenProvider.advance();
    node = MultiplyNode(node, getTermNode(tokenProvider));
    token = tokenProvider.getNextToken(remove: false);
  }

  return node;
}

Node getTermNode(TokenProvider tokenProvider) {
  // Term is a factor [+ another factor]
  // The use of + here is opposite of normal math. Normal math would have *

  Node node = getFactorNode(tokenProvider);

  var token = tokenProvider.getNextToken(remove: false);

  while (token.tokenType == TokenType.plus) {
    tokenProvider.advance();
    node = AddNode(node, getFactorNode(tokenProvider));
    token = tokenProvider.getNextToken(remove: false);
  }

  return node;
}

Node getFactorNode(TokenProvider tokenProvider) {
  var token = tokenProvider.getNextToken();

  if (token.tokenType == TokenType.value) {
    return ValueNode(token.value!);
  } else if (token.tokenType == TokenType.leftParen) {
    Node node = getExpression(tokenProvider);
    token = tokenProvider.getNextToken();
    if (token.tokenType != TokenType.rightParen) {
      print('Parenthesis mismatched');
    }
    return node;
  }
  print('Returns empty node');
  return Node();
}

enum OpType { plus, times }

class Node {}

class ValueNode extends Node {
  int value;
  ValueNode(this.value);
}

class AddNode extends Node {
  Node leftNode;
  Node rightNode;

  AddNode(this.leftNode, this.rightNode);
}

class MultiplyNode extends Node {
  Node leftNode;
  Node rightNode;

  MultiplyNode(this.leftNode, this.rightNode);
}

class OperatorNode extends Node {
  OpType operator;
  OperatorNode(this.operator);
}

class SubTree extends Node {
  List<Node> tree = [];
  SubTree(this.tree);
}

extension kind on String {
  bool isDigit() {
    try {
      int _ = int.parse(this);
      return true;
    } catch (e) {
      return false;
    }
  }
}

enum TokenType { value, plus, times, leftParen, rightParen, empty, error }

class Token {
  int? value;
  TokenType tokenType;
  Token(this.tokenType);
}

class TokenProvider {
  late String line;
  TokenProvider(String s) {
    line = s.replaceAll(' ', '');
  }

  Token getNextToken({bool remove = true}) {
    if (line.isEmpty) return Token(TokenType.empty);

    String char = line.substring(0, 1);

    if (remove) line = line.substring(1);

    if (char.isDigit()) {
      final token = Token(TokenType.value);
      token.value = int.parse(char);
      return token;
    }

    if (char == '+') return Token(TokenType.plus);
    if (char == '*') return Token(TokenType.times);
    if (char == '(') return Token(TokenType.leftParen);
    if (char == ')') return Token(TokenType.rightParen);

    return Token(TokenType.error);
  }

  void advance() {
    line = line.substring(1);
  }
}

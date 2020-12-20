import re

def parse(expr):
	while expr.find('(') > -1:
		match = re.search(r'\([0-9\+\*]+\)', expr)
		match_expr = match.group(0)
		result = parse(match_expr[1:-1])
		expr = expr.replace(match_expr, str(result))
	
	left = 0
	right = 0
	operator = ''
	expr += '='
	for c in expr:
		if c.isnumeric():
			right = right*10+int(c)	
		else:
			if operator == '*':
				left *= right
			elif operator == '+':
				left += right
			else:
				left = right
			operator = c
			right = 0

	return left

def parse_add(expr):
	while expr.find('+') > -1:
		match = re.search(r'([0-9]+)\+([0-9]+)', expr)
		#expr = expr.replace(match.group(0), str(int(match.group(1)) + int(match.group(2))))
		expr = expr[:match.start()] + str(int(match.group(1)) + int(match.group(2))) + expr[match.end():]
	return expr

def parse_mult(expr):
	while expr.find('*') > -1:
		match = re.search(r'([0-9]+)\*([0-9]+)', expr)
		# expr = expr.replace(match.group(0), str(int(match.group(1)) * int(match.group(2))))
		expr = expr[:match.start()] + str(int(match.group(1)) * int(match.group(2))) + expr[match.end():]
	return expr

def parse2(expr):
	while expr.find('(') > -1:
		match = re.search(r'\([0-9\+\*]+\)', expr)
		match_expr = match.group(0)
		result = parse2(match_expr[1:-1])
		expr = expr.replace(match_expr, str(result))
		#expr = expr[:match.pos] + str(result) + expr[match.endpos:]

	expr = parse_add(expr)
	expr = parse_mult(expr)
	return int(expr)

def solveA(expressions):
	total = 0
	for expr in expressions:
		total += parse(expr)
	return total

def solveB(expressions):
	total = 0
	for expr in expressions:
		total += parse2(expr)
	return total

if __name__ == "__main__":
	with open('input.txt') as f:
		lines = [l.strip().replace(' ','') for l in f]

	assert parse('2*3+(4*5)') == 26, "Should be 26"
	assert parse('((2+4*9)*(6+9*8+6)+6)+2+4*2') == 13632, "Should print 13632"

	#print('Part 1', solveA(lines))
	print('Part 2', solveB(lines))

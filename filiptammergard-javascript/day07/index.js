const fs = require("fs");

const input = fs.readFileSync("day07/input.txt").toString().split("\n");

function computePartOne() {
  const rules = parseRules(input);
  const bags = getBagsThatCanContainBag(rules, "shiny gold");
  return bags.size;
}

function computePartTwo() {
  const rules = parseRules(input);
  const amount = countBagsInBag(rules, "shiny gold");
  return amount;
}

function parseRules(input) {
  const rules = input.reduce((accRules, line) => {
    const [, bag, otherBags] = /(\w+ \w+) bags contain (.*)\./.exec(line);
    const contains =
      otherBags !== "no other bags"
        ? otherBags.split(", ").map((other) => {
            const [, amount, bag] = /(\d+) (\w+ \w+) bags?/.exec(other);
            return { amount: parseInt(amount), bag };
          })
        : [];
    return [...accRules, { bag, contains }];
  }, []);
  return rules;
}

function getBagsThatCanContainBag(rules, bag) {
  const bagsThatDirectlyIncludeBag = rules
    .filter((rule) => rule.contains.some((content) => content.bag === bag))
    .map((rule) => rule.bag);

  bagsThatDirectlyIncludeBag.forEach((bag) => {
    bagsThatDirectlyIncludeBag.push(...getBagsThatCanContainBag(rules, bag));
  });
  return new Set(bagsThatDirectlyIncludeBag);
}

function countBagsInBag(rules, bag) {
  const rule = rules.find((rule) => rule.bag === bag);
  const sum = rule.contains.reduce((acc, { bag, amount }) => {
    return acc + amount + amount * countBagsInBag(rules, bag);
  }, 0);
  return sum;
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);

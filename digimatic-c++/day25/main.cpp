// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <iostream>
#include <stdint.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;

int input1 = 15113849;
int input2 = 4206373;

int handshake(int s, int loopSize)
{
	int64_t c = 1;
	for(int i = 0; i < loopSize; i++)
	{
		c *= s;
		c = c % 20201227;
	}
	return static_cast<int>(c);
}

int findPrivateKey(int cardPublicKey, int doorPublicKey)
{
	int cardLoopSize = 1;
	int64_t c = 1;
	while(true)
	{
		c *= 7;
		c = c % 20201227;
		if(c == cardPublicKey)
			break;
		cardLoopSize++;
	}

	c = 1;
	int doorLoopSize = 1;
	while(true)
	{
		c *= 7;
		c = c % 20201227;
		if(c == doorPublicKey)
			break;
		doorLoopSize++;
	}

	int encryptionKey1 = handshake(doorPublicKey, cardLoopSize);
	int encryptionKey2 = handshake(cardPublicKey, doorLoopSize);
	if(encryptionKey1 != encryptionKey2)
		return -1;
	return encryptionKey2;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(findPrivateKey(5764801, 17807724) == 14897079);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << findPrivateKey(input1, input2) << "\n";
	return result;
}

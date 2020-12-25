//
//  main.cpp
//  AdventOfCode201225_1
//
//  Created by Kjell-Olov Högdal on 2020-12-25.
//

#include <iostream>
#include <vector>

using Number = std::uint64_t;
using Key = Number;
using KeyPair = std::pair<Key,Key>;
using LoopSize = Number;
using SubjectNumber = Number;

KeyPair const example_keyes {5764801,17807724}; // card,door
KeyPair const public_keyes {19241437,17346587}; // card,door

struct Device {
    LoopSize this_secret_loop_size;
    SubjectNumber const this_public_key_subject_number {7};
    
    SubjectNumber transformed_subject_number(SubjectNumber device_subject_number, LoopSize device_loop_size) {
        /*
         The handshake used by the card and the door involves an operation that transforms a subject number. To transform a subject number, start with the value 1. Then, a number of times called the loop size, perform the following steps:

         Set the value to itself multiplied by the subject number.
         Set the value to the remainder after dividing the value by 20201227.
         */
        std::cout << "\nTransforming " << device_subject_number << " with loop size " << device_loop_size;
        SubjectNumber result {1};
        for (int i = 0; i<device_loop_size;++i) {
            result *= device_subject_number;
            result = result % 20201227;
        }
        std::cout << " = " << result;
        return result;
    }
    
    Key public_key() {
        return transformed_subject_number(this_public_key_subject_number, this_secret_loop_size);
    }
    
    Key private_key(Key device_public_key) {
        return transformed_subject_number(device_public_key,this_secret_loop_size);
    }
};

Key solve_private_key(KeyPair const& public_keyes) {
    Key result;

    // Test
    if (false) {
        LoopSize card_loop_size_guess {8};
        Device card {card_loop_size_guess};
        LoopSize door_loop_size_guess {11};
        Device door {door_loop_size_guess};
        std::cout << "\nTest 1 : ";
        Key card_public_key {5764801};
        bool test_passed_1 = (card.public_key() == card_public_key);
        if (test_passed_1) {
            std::cout << " PASSED :)";
        }
        else {
            std::cout << " FAILED 8-|";
        }

        std::cout << "\nTest 2 : ";
        Key door_public_key {17807724};
        bool test_passed_2 = (door.public_key() == door_public_key);
        if (test_passed_2) {
            std::cout << " PASSED :)";
        }
        else {
            std::cout << " FAILED 8-|";
        }
        
        Key card_private_key = card.private_key(door_public_key);
        std::cout << "\nCard Private Key : " << card_private_key;
        bool test_passed_3 = (card_private_key == 14897079);
        std::cout << "\nTest 3 : ";
        if (test_passed_3) {
            std::cout << " PASSED :)";
        }
        else {
            std::cout << " FAILED 8-|";
        }

        Key door_private_key = door.private_key(card_public_key);
        std::cout << "\nDoor Private Key : " << door_private_key;
        bool test_passed_4 = (door_private_key == 14897079);
        std::cout << "\nTest 4 : ";
        if (test_passed_4) {
            std::cout << " PASSED :)";
        }
        else {
            std::cout << " FAILED 8-|";
        }
        
        // Brute force to find loop size?
        // Well, it works for this small test - not for part 1 ;)
        for (Number cycle = 3; cycle < 100; ++cycle) {
            Device device {cycle};
            if (device.transformed_subject_number(7, cycle) == example_keyes.first) {
                std::cout << "\nFound Card Cycle : " << cycle;
                device.this_secret_loop_size = cycle;
                auto private_key = device.private_key(example_keyes.second);
                std::cout << "\nGives Private Key : " << private_key;
                break;
            }
        }

        for (Number cycle = 3; cycle < 100; ++cycle) {
            Device device {cycle};
            if (device.transformed_subject_number(7, cycle) == example_keyes.second) {
                std::cout << "\nFound Door Cycle : " << cycle;
                device.this_secret_loop_size = cycle;
                auto private_key = device.private_key(example_keyes.first);
                std::cout << "\nGives Private Key : " << private_key;
                break;
            }
        }
    }
    
    // Part 1
    if (true) {
        // So basically we want to know how many times to run the "transform" loop
        // to get either of our public keys?
        // And because this number of times (the secret loop size) is a constant
        // we can basically calculate also the private key as we search :)
        // Thanks to https://github.com/sjmulder/aoc/blob/master/2020/day25/solve.c
        // for putting me on the right track :)
        std::pair<bool, bool> unmatched {true,true};
        KeyPair pks = public_keyes; // public keys result
        SubjectNumber pk {1}; // public key accumulator
        SubjectNumber sk1 {1}, sk2 {1}; // secret keys (encryption key)
        std::pair<SubjectNumber, SubjectNumber> dk {1,1};
        std::pair<LoopSize,LoopSize> loop_size {0,0};
        Number const N = 20201227;
        // Run the trasnform loop until we have matched both public keys
        // and recorded the two secret numbers = the loop count i at each match :)
        for (Number i=1;unmatched.first or unmatched.second;++i) {
            pk = pk * 7 % N; // When i is the secret loop size of a device then pk is its public key :)
            sk1 = sk1 * pks.second % N; // card secret (encryption) key (transform door public key)
            sk2 = sk2 * pks.first % N; // door secret (encryption) key (transform card public key)
            if (pk == pks.first) {
                // i is now the secret loop count of card
                // AND sk1 is the card secret encryption key :)
                unmatched.first = false;
                loop_size.first = i;
                dk.first = sk1;
            }
            if (pk == pks.second) {
                // i is now the secret loop count of door
                // AND sk2 is the door encryption key :)
                unmatched.second = false;
                loop_size.second = i;
                dk.second = sk2;
            }
        }
        // We now expect dk.first == dk.second (same private key)
        // And loop_size.first is card secret loop size and loop_size.second to be door secret loop count
        std::cout << "\n\n<Card>";
        std::cout << "\n\tsecret loop size : " << loop_size.first;
        std::cout << "\n\tPrivate Key : " << dk.first;

        std::cout << "\n\n<Door>";
        std::cout << "\n\tsecret loop size : " << loop_size.second;
        std::cout << "\n\tPrivate Key : " << dk.second;
    }
    
    return result;
}

int main(int argc, const char * argv[]) {
    auto pks = solve_private_key(public_keyes);
    std::cout << "\n\n";
    return 0;
}

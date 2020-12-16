def get_last_spoken(input, count):
    spoken_numbers = {}
    last_spoken_turns = {}
    
    turns = 1
    for t in input:
        spoken_numbers[t] = 1
        last_spoken_turns[t] = (turns, 0)
        turns += 1

    last_spoken = input[-1]
    while turns <= count:
        speak = 0
        prev_turn, prev_turn_2 = last_spoken_turns[last_spoken]
        if spoken_numbers[last_spoken] - 1 != 0:
            speak = prev_turn - prev_turn_2

        if speak not in spoken_numbers:
            spoken_numbers[speak] = 1
            last_spoken_turns[speak] = (0, 0)
        else:
            spoken_numbers[speak] += 1
        
        prev_turn, _ = last_spoken_turns[speak]
        last_spoken_turns[speak] = (turns, prev_turn)
        last_spoken = speak
        turns += 1

        # if turns % 10000000 == 0:
        #     print()
        #     print("Turn count:", turns)
        #     print("Length:", len(spoken_numbers))
    return last_spoken

# assert get_last_spoken([0,3,6], 2020) == 436, "Should print 436"
# assert get_last_spoken([1,3,2], 2020) == 1, "Should print 1"
# assert get_last_spoken([2,1,3], 2020) == 10, "Should print 10"
# assert get_last_spoken([1,2,3], 2020) == 27, "Should print 27"
# assert get_last_spoken([2,3,1], 2020) == 78, "Should print 78"
# assert get_last_spoken([3,2,1], 2020) == 438, "Should print 438"
# assert get_last_spoken([3,1,2], 2020) == 1836, "Should print 1836"

print(get_last_spoken([12,20,0,6,1,17,7], 2020))

#assert get_last_spoken([0,3,6], 30000000) == 175594, "Should print 175594"

print(get_last_spoken([12,20,0,6,1,17,7], 30000000))

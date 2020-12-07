<?php declare(strict_types=1);
    include 'vendor/autoload.php';

    $passportLiterals = explode ("\n", file_get_contents ('input/day5.txt'));
    $highestSeatId = 0;
    $yourSeatId = 0;
    $seatsById = array ();

    foreach ($passportLiterals as $literal)
    {
        $seat = \Almamu\AdventOfCode2020\Day5\Seat::fromSeatString ($literal);

        if ($seat->getSeatId () > $highestSeatId)
            $highestSeatId = $seat->getSeatId ();

        $seatsById [$seat->getSeatId ()] = $seat;
    }

    // go trough all the seat ids and check for the correct one
    for ($i = 0; $i < count ($seatsById); $i ++)
    {
        if (array_key_exists ($i, $seatsById) == true)
            continue;
        if (array_key_exists ($i - 1, $seatsById) == false)
            continue;
        if (array_key_exists ($i + 1, $seatsById) == false)
            continue;

        $yourSeatId = $i;
    }

    echo "The highest seat id is {$highestSeatId}" . PHP_EOL;
    echo "Your seat id is {$yourSeatId}";
<?php declare(strict_types=1);
    $lines = explode ("\n", trim (file_get_contents ("input/day1.txt")));

    // convert all the lines to integers first
    $lines = array_map (fn (string $value) => (int) $value, $lines);
    $resultTwo = 0;
    $resultThree = 0;

    foreach ($lines as $value)
    {
        foreach ($lines as $secondValue)
        {
            $sum = $secondValue + $value;

            if ($sum == 2020)
                $resultTwo = $secondValue * $value;

            foreach ($lines as $thirdValue)
            {
                $sum = $secondValue + $value + $thirdValue;

                if ($sum == 2020)
                    $resultThree = $secondValue * $value * $thirdValue;
            }
        }
    }

    if ($resultTwo == 0)
        die ("Cannot fix your expenses report, are you sure you added the correct file?");

    if ($resultThree == 0)
        die ("Cannot fix your expenses report, there is no extra number");

    echo "Your expenses report answer is: {$resultTwo}\n";
    echo "Your extra number is: {$resultThree}";
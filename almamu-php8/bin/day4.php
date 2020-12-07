<?php declare(strict_types=1);
    include 'vendor/autoload.php';

    $passportLiterals = explode ("\n\n", file_get_contents ('input/day4.txt'));
    $validPassportsAnswer1 = array ();
    $validPassportsAnswer2 = array ();

    foreach ($passportLiterals as $literal)
    {
        try
        {
            $validPassportsAnswer1 [] = \Almamu\AdventOfCode2020\Day4\Passport::fromPassportLiteralAnswer1 ($literal);
        }
        catch (\Exception)
        {
        }

        try
        {
            $validPassportsAnswer2 [] = \Almamu\AdventOfCode2020\Day4\Passport::fromPassportLiteralAnswer2 ($literal);
        }
        catch (\Exception)
        {
        }
    }

    echo "Number of valid passports on answer 1: " . count($validPassportsAnswer1) . PHP_EOL;
    echo "Number of valid passports on answer 2: " . count ($validPassportsAnswer2) . PHP_EOL;
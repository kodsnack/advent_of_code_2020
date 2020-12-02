<?php declare(strict_types=1);
    include 'vendor/autoload.php';

    $lines = explode ("\n", trim (file_get_contents ("input/day2.txt")));
    $numberOfValidPasswordsCount = 0;
    $numberOfValidPasswordsPosition = 0;

    foreach ($lines as $line)
    {
        $parts = explode (':', $line);
        $policyString = trim ($parts [0]);
        $password = trim ($parts [1]);

        $passwordPolicyCount = \Almamu\AdventOfCode2020\Day2\PasswordPolicyCount::fromString ($policyString);
        $passwordPolicyPosition = \Almamu\AdventOfCode2020\Day2\PasswordPolicyPosition::fromString ($policyString);
        $numberOfValidPasswordsCount += intval ($passwordPolicyCount->verifyPassword ($password));
        $numberOfValidPasswordsPosition += intval ($passwordPolicyPosition->verifyPassword ($password));
    }

    echo "Number of valid passwords for the first password policy: {$numberOfValidPasswordsCount}\n";
    echo "Number of valid passwords for the second password policy: {$numberOfValidPasswordsPosition}";
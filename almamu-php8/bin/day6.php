<?php declare(strict_types=1);
    include 'vendor/autoload.php';

    $groupAnswers = explode ("\n\n", file_get_contents ('input/day6.txt'));

    function calculateCountAnswer1 (string $answers): int
    {
        $lines = explode ("\n", $answers);
        $answersCache = array ();

        foreach ($lines as $line)
        {
            for ($i = 0; $i < strlen ($line); $i ++)
            {
                $answersCache [$line [$i]] = ($answersCache [$line [$i]] ?? 0) + 1;
            }
        }

        return count ($answersCache);
    }

    function calculateCountAnswer2 (string $answers): int
    {
        $lines = explode ("\n", $answers);
        $answersCache = array ();

        foreach ($lines as $line)
        {
            for ($i = 0; $i < strlen ($line); $i ++)
            {
                $answersCache [$line [$i]] = ($answersCache [$line [$i]] ?? 0) + 1;
            }
        }

        $count = 0;

        foreach ($answersCache as $answer)
        {
            if ($answer == count ($lines))
                $count ++;
        }

        return $count;
    }

    $countAnswer1 = 0;
    $countAnswer2 = 0;

    foreach ($groupAnswers as $answer)
    {
        $countAnswer1 += calculateCountAnswer1 ($answer);
        $countAnswer2 += calculateCountAnswer2 ($answer);
    }

    echo "Total number of yes answers: {$countAnswer1}" . PHP_EOL;
    echo "Total number of yes answers in group: {$countAnswer2}";
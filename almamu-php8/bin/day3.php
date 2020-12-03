<?php declare(strict_types=1);
    include 'vendor/autoload.php';

    $lines = explode ("\n", trim (file_get_contents ("input/day3.txt")));
    $map = \Almamu\AdventOfCode2020\Day3\Map::fromLinesArray ($lines);
    $slopes = [
        new \Almamu\AdventOfCode2020\Day3\Vector2D (1, 1),
        new \Almamu\AdventOfCode2020\Day3\Vector2D (3, 1),
        new \Almamu\AdventOfCode2020\Day3\Vector2D (5, 1),
        new \Almamu\AdventOfCode2020\Day3\Vector2D (7, 1),
        new \Almamu\AdventOfCode2020\Day3\Vector2D (1, 2)
    ];
    $results = [];

    foreach ($slopes as $slope)
    {
        $numberOfTrees = 0;

        $walker = new \Almamu\AdventOfCode2020\Day3\MapWalker ($map, $slope);

        try
        {
            while (true)
            {
                $block = $walker->walk ();

                $numberOfTrees += intval ($block == \Almamu\AdventOfCode2020\Day3\Map::MAP_BLOCK_TREE);
            }
        }
        catch (\Exception)
        {

        }

        $results [] = $numberOfTrees;
    }

    // calculate the product of all the number of trees found
    $sum = 0;

    foreach ($results as $numberOfTrees)
    {
        if ($sum == 0)
            $sum = $numberOfTrees;
        else
            $sum *= $numberOfTrees;
    }

    echo "Number of trees found in your way to the end: {$results [1]}\n";
    echo "Product of number of trees found in your way to the end on all five slopes: {$sum}";
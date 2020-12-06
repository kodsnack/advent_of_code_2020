<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day5;

    class Seat
    {
        protected function __construct (
            public int $row,
            public int $column
        ) {}

        public function getSeatId (): int
        {
            return $this->row * 8 + $this->column;
        }

        public static function fromSeatString (string $position): self
        {
            $rowMin = 0;
            $rowMax = 127;
            $columnMin = 0;
            $columnMax = 7;

            for ($i = 0; $i < strlen ($position); $i ++)
            {
                switch ($position [$i])
                {
                    case 'F':
                        $rowMax -= (int) ceil (($rowMax - $rowMin) / 2);
                        break;

                    case 'B':
                        $rowMin += (int) ceil (($rowMax - $rowMin) / 2);
                        break;

                    case 'L':
                        $columnMax -= (int) ceil (($columnMax - $columnMin) / 2);
                        break;

                    case 'R':
                        $columnMin += (int) ceil (($columnMax - $columnMin) / 2);
                        break;
                }
            }

            if ($rowMax != $rowMin)
                throw new \Exception ('Seat string is not valid');
            if ($columnMax != $columnMin)
                throw new \Exception ('Seat string is not valid');

            return new self ($rowMin, $columnMin);
        }
    };
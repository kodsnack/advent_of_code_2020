<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day3;

    class Map
    {
        /** @var string Tree indicator in the map */
        public const MAP_BLOCK_TREE = '#';
        /** @var string Empty indicator in the map */
        public const MAP_BLOCK_EMPTY = '.';

        protected function __construct (
            protected array $mapLines,
            protected int $width
        ) { }

        public function getHeight (): int
        {
            return count ($this->mapLines);
        }

        public function getPosition (Vector2D $position): string
        {
            if ($position->getY () >= $this->getHeight ())
                throw new \Exception ("The y position is out of range");

            // the map expands infinitely to the right, so take that into account
            $x = ($position->getX () >= $this->width) ? ($position->getX () % $this->width) : $position->getX ();

            return $this->mapLines [$position->getY ()] [$x];
        }

        /**
         * Creates a new instance of the map based on the lines array specified
         *
         * @param string[] $lines
         * @return static
         */
        public static function fromLinesArray (array $lines): self
        {
            if (count ($lines) == 0)
                throw new \Exception ("The map doesn't have any data in it");

            // ensure the width of the map is consistent through the whole list
            $expectedWidth = strlen ($lines [0]);

            foreach ($lines as $line)
            {
                if (strlen ($line) !== $expectedWidth)
                    throw new \Exception ("One of the lines in the map is not the same size as the first one");
            }

            // finally create the map
            return new self ($lines, $expectedWidth);
        }
    };
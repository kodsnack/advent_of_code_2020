<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day3;

    class MapWalker
    {
        private Vector2D $currentPosition;

        public function __construct (protected Map $map, protected Vector2D $slope)
        {
            $this->currentPosition = new Vector2D (0, 0);
        }

        public function walk (): string
        {
            // get the block in the current position
            $current = $this->map->getPosition ($this->currentPosition);

            // increase the currentPosition by the slope
            $this->currentPosition
                ->setX ($this->currentPosition->getX () + $this->slope->getX ())
                ->setY ($this->currentPosition->getY () + $this->slope->getY ());

            return $current;
        }
    };
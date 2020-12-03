<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day3;

    class Vector2D
    {
        public function __construct (
            public int $x,
            public int $y
        ) { }

        public function getX (): int
        {
            return $this->x;
        }

        public function getY (): int
        {
            return $this->y;
        }

        public function setX (int $newX): self
        {
            $this->x = $newX;
            return $this;
        }

        public function setY (int $newY): self
        {
            $this->y = $newY;
            return $this;
        }
    };
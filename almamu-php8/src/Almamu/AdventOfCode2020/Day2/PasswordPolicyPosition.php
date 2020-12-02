<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day2;

    class PasswordPolicyPosition
    {
        public function __construct (
            public string $character,
            public int $position1,
            public int $position2
        ) { }

        public static function fromString (string $passwordPolicyString): self
        {
            if (preg_match_all ('/([0-9]+)-([0-9]+) (\w)/', $passwordPolicyString, $matches) === false)
                throw new \Exception ("Cannot parse the password policy string");

            if (count ($matches) != 4)
                throw new \Exception ("Cannot parse the password policy, not enough matches");

            $position1 = intval ($matches [1] [0]) - 1;
            $position2 = intval ($matches [2] [0]) - 1;

            if ($position1 < 0)
                throw new \Exception ("Cannot parse the password policy, first position is out of string bounds");
            if ($position2 < 0)
                throw new \Exception ("Cannot parse the password policy, second position is out of string bounds");

            return new self (
                $matches [3] [0],
                $position1,
                $position2
            );
        }

        public function verifyPassword (string $password): bool
        {
            // early return if the password is of not enough length
            if (strlen ($password) < $this->position1)
                return false;
            if ($password [$this->position1] == $this->character && strlen ($password) < $this->position2)
                return true;
            if ($password [$this->position1] == $this->character && strlen ($password) >= $this->position2 && $password [$this->position2] != $this->character)
                return true;
            if ($password [$this->position1] != $this->character && strlen ($password) >= $this->position2 && $password [$this->position2] == $this->character)
                return true;

            return false;
        }
    };
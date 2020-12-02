<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day2;

    class PasswordPolicyCount
    {
        public function __construct (
            public string $character,
            public int $minimum,
            public int $maximum
        ) { }

        public static function fromString (string $passwordPolicyString): self
        {
            if (preg_match_all ('/([0-9]+)-([0-9]+) (\w)/', $passwordPolicyString, $matches) === false)
                throw new \Exception ("Cannot parse the password policy string");

            if (count ($matches) != 4)
                throw new \Exception ("Cannot parse the password policy, not enough matches");

            return new self (
                $matches [3] [0],
                intval ($matches [1] [0]),
                intval ($matches [2] [0])
            );
        }

        public function verifyPassword (string $password): bool
        {
            $numberOfCharacters = preg_match_all ("/({$this->character})/", $password);

            return $this->minimum <= $numberOfCharacters && $numberOfCharacters <= $this->maximum;
        }
    };
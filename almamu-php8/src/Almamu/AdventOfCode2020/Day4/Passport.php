<?php declare(strict_types=1);
    namespace Almamu\AdventOfCode2020\Day4;

    class Passport
    {
        const VALID_EYE_COLORS = [
            'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'
        ];

        protected function __construct (
            public int $birthYear,
            public int $issueYear,
            public int $expirationYear,
            public string $height,
            public string $hairColor,
            public string $eyeColor,
            public string $passportId,
            public int $countryId = 0
        ) {}


        public static function fromPassportLiteralAnswer1 (string $data): self
        {
            $pairs = explode (" ", trim (str_replace ("\n", " ", $data)));
            $values = array ();

            foreach ($pairs as $pair)
            {
                $parts = explode (':', $pair);

                $values [$parts [0]] = $parts [1];
            }

            return new self (
                intval ($values ['byr'] ?? throw new \Exception ('Birth year required')),
                intval ($values ['iyr'] ?? throw new \Exception ('Issue year required')),
                intval ($values ['eyr'] ?? throw new \Exception ('Expiration year required')),
                $values ['hgt'] ?? throw new \Exception ('Height required'),
                $values ['hcl'] ?? throw new \Exception ('Hair color required'),
                $values ['ecl'] ?? throw new \Exception ('Eye color required'),
                $values ['pid'] ?? throw new \Exception ('Passport id required'),
                intval ($values ['cid'] ?? 0)
            );
        }

        public static function fromPassportLiteralAnswer2 (string $data): self
        {
            $pairs = explode (" ", trim (str_replace ("\n", " ", $data)));
            $values = array ();

            foreach ($pairs as $pair)
            {
                $parts = explode (':', $pair);

                $values [$parts [0]] = $parts [1];
            }

            $birthYear = intval ($values ['byr'] ?? throw new \Exception ('Birth year required'));
            $issueYear = intval ($values ['iyr'] ?? throw new \Exception ('Issue year required'));
            $expirationYear = intval ($values ['eyr'] ?? throw new \Exception ('Expiration year required'));
            $height = $values ['hgt'] ?? throw new \Exception ('Height required');
            $hairColor = $values ['hcl'] ?? throw new \Exception ('Hair color required');
            $eyeColor = $values ['ecl'] ?? throw new \Exception ('Eye color required');
            $passportId = $values ['pid'] ?? throw new \Exception ('Passport id required');

            if ($birthYear < 1920 || $birthYear > 2002)
                throw new \Exception ('Birth year is out of bounds');
            if ($issueYear < 2010 || $issueYear > 2020)
                throw new \Exception ('Issue year is out of bounds');
            if ($expirationYear < 2020 || $expirationYear > 2030)
                throw new \Exception ('Expiration year is out of bounds');
            if (preg_match ("/^([0-9]+(?:cm|in))$/", $height) != 1)
                throw new \Exception ('Height is not valid');
            if (str_contains ($height, 'cm') === true && (intval ($height) < 150 || intval ($height) > 193))
                throw new \Exception ('Height is not valid');
            if (str_contains ($height, 'in') === true && (intval ($height) < 59 || intval ($height) > 76))
                throw new \Exception ('Height is not valid');
            if (preg_match ("/^(#[0-9A-Fa-f]{6})$/", $hairColor) != 1)
                throw new \Exception ('Hair color is not correct format');
            if (in_array ($eyeColor, self::VALID_EYE_COLORS) === false)
                throw new \Exception ('Eye color is not valid');
            if (preg_match ("/^([0-9]{9})$/", $passportId) != 1)
                throw new \Exception ('Passport ID is not valid');

            return new self (
                $birthYear,
                $issueYear,
                $expirationYear,
                $height,
                $hairColor,
                $eyeColor,
                $passportId,
                intval ($values ['cid'] ?? 0)
            );
        }
    };
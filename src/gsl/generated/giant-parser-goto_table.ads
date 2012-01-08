package Giant.Parser.Goto_Table is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-11, 9),(-10, 8),(-9, 7),(-8, 6)
,(-7, 5),(-6, 4),(-5, 3),(-4, 2)
,(-3, 1),(-2, 23)
-- State  1
,(-10, 24)
-- State  2

-- State  3

-- State  4

-- State  5

-- State  6

-- State  7

-- State  8

-- State  9

-- State  10
,(-10, 25)

-- State  11

-- State  12

-- State  13

-- State  14

-- State  15

-- State  16

-- State  17

-- State  18

-- State  19

-- State  20

-- State  21
,(-12, 34),(-11, 9),(-10, 8),(-9, 7)
,(-8, 6),(-7, 5),(-6, 4),(-5, 3)
,(-4, 2),(-3, 32)
-- State  22
,(-13, 37),(-11, 9)
,(-10, 8),(-9, 7),(-8, 6),(-7, 5)
,(-6, 4),(-5, 3),(-4, 2),(-3, 35)

-- State  23

-- State  24

-- State  25
,(-11, 9),(-10, 8),(-9, 7),(-8, 6)
,(-7, 5),(-6, 4),(-5, 3),(-4, 2)
,(-3, 39)
-- State  26

-- State  27

-- State  28

-- State  29

-- State  30

-- State  31

-- State  32
,(-10, 24)
-- State  33

-- State  34

-- State  35
,(-10, 24)
-- State  36

-- State  37

-- State  38

-- State  39
,(-10, 24)

-- State  40

-- State  41

-- State  42

-- State  43

-- State  44
,(-12, 51),(-11, 9),(-10, 8),(-9, 7)
,(-8, 6),(-7, 5),(-6, 4),(-5, 3)
,(-4, 2),(-3, 32)
-- State  45

-- State  46
,(-13, 52),(-11, 9)
,(-10, 8),(-9, 7),(-8, 6),(-7, 5)
,(-6, 4),(-5, 3),(-4, 2),(-3, 35)

-- State  47

-- State  48

-- State  49

-- State  50

-- State  51

-- State  52

);
--  The offset vector
GOTO_OFFSET : array (0.. 52) of Integer :=
( 0,
 10, 11, 11, 11, 11, 11, 11, 11, 11, 11,
 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
 12, 22, 32, 32, 32, 41, 41, 41, 41, 41,
 41, 41, 42, 42, 42, 43, 43, 43, 43, 44,
 44, 44, 44, 44, 54, 54, 64, 64, 64, 64,
 64, 64);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  31) of Natural := ( 2,
 1, 1, 1, 1, 1, 1, 1, 1,
 1, 4, 2, 1, 3, 2, 3, 2,
 3, 2, 3, 1, 1, 1, 1, 1,
 3, 3, 4, 4, 1, 2, 2);
   Get_LHS_Rule: array (Rule range  0 ..  31) of Nonterminal := (-1,
-2,-3,-3,-3,-3,-3,-3,-3,
-3,-3,-3,-12,-12,-10,-10,-13,
-13,-11,-11,-4,-4,-4,-4,-4,
-5,-5,-7,-7,-6,-8,-9);
end Giant.Parser.Goto_Table;

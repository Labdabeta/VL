with Test_Suites; use Test_Suites;
with Tiles; use Tiles;
with Units; use Units;

package body Test_Tile is

    procedure Assert_Equal_Tiles is new Assert_Same (
        Value_Type => Tile,
        To_String => To_String);

    -- Helper function
    procedure Check_Tile_Event (
        Input : in Tile;
        Attackers : in Presence_Array;
        Create : in Boolean;
        Expected : in Tile) is
        The_Tile : Tile := Input; -- A copy of the input
    begin
        Resolve (
            This => The_Tile,
            Presence => Attackers,
            Create => Create);

        Assert_Equal_Tiles (
            Value => The_Tile,
            Expected => Expected);
    end Check_Tile_Event;

    procedure Test_All is
        Test_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((VAMPIRE, 1), FLOOR),
            LEPRECHAUN => ((LEPRECHAUN, 1), FLOOR),
            HUMAN => ((HUMAN, 1), FLOOR),
            FAIRY => ((FAIRY, 1), FLOOR),
            ZOMBIE => ((ZOMBIE, 1), FLOOR),
            NONE => ((NONE, 1), FLOOR),
            UNKNOWN => ((UNKNOWN, 1), FLOOR));
        All_Attackers : Presence_Array := (
            1 => (VAMPIRE, 1),
            2 => (LEPRECHAUN, 2),
            3 => (HUMAN, 3),
            4 => (FAIRY, 4),
            5 => (ZOMBIE, 5));
    begin
        for The_Occupant in Test_Tiles'Range loop
            Check_Tile_Event (
                Input => Test_Tiles (The_Occupant),
                Attackers => All_Attackers,
                Create => False,
                Expected => ((NONE, 0), FLOOR));
        end loop;
    end Test_All;

    procedure Test_Basic is
        The_Tile : Tile := ((NONE, 1), FLOOR);
        Attackers : Presence_Array := ((VAMPIRE, 1), (LEPRECHAUN, 2));
        Result_Tile : Tile := ((LEPRECHAUN, 2), FLOOR);
    begin
        Check_Tile_Event (The_Tile, Attackers, False, Result_Tile);
    end Test_Basic;

    procedure Test_Create is
        The_Tile : Tile := ((NONE, 1), PIT);
        No_Attackers : Presence_Array (1 .. 0);
        Fairy_Attacker : Presence_Array := (1 => (FAIRY, 1));
        Human_Attacker : Presence_Array := (1 => (HUMAN, 1));
        Empty_Result_Tile : Tile := ((NONE, 0), FLOOR);
        Fairy_Result_Tile : Tile := ((FAIRY, 1), FLOOR);
        Human_Result_Tile : Tile := ((HUMAN, 1), FLOOR);
    begin
        Check_Tile_Event (The_Tile, No_Attackers, True, Empty_Result_Tile);
        Check_Tile_Event (The_Tile, Fairy_Attacker, True, Fairy_Result_Tile);
        Check_Tile_Event (The_Tile, Human_Attacker, True, Human_Result_Tile);
    end Test_Create;

    procedure Test_Doubles is
        Test_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((VAMPIRE, 1), FLOOR),
            LEPRECHAUN => ((LEPRECHAUN, 1), FLOOR),
            HUMAN => ((HUMAN, 1), FLOOR),
            FAIRY => ((FAIRY, 1), FLOOR),
            ZOMBIE => ((ZOMBIE, 1), FLOOR),
            NONE => ((NONE, 1), FLOOR),
            UNKNOWN => ((UNKNOWN, 1), FLOOR));
        Vampire_Attacker : Presence_Array := (1 => (VAMPIRE, 2));
        Vampire_Result_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((NONE, 0), FLOOR),
            LEPRECHAUN => ((LEPRECHAUN, 1), FLOOR),
            HUMAN => ((VAMPIRE, 2), FLOOR),
            FAIRY => ((FAIRY, 1), FLOOR),
            ZOMBIE => ((VAMPIRE, 2), FLOOR),
            NONE => ((VAMPIRE, 2), FLOOR),
            UNKNOWN => ((VAMPIRE, 2), FLOOR));
        Leprechaun_Attacker : Presence_Array := (1 => (LEPRECHAUN, 2));
        Leprechaun_Result_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((LEPRECHAUN, 2), FLOOR),
            LEPRECHAUN => ((NONE, 0), FLOOR),
            HUMAN => ((HUMAN, 1), FLOOR),
            FAIRY => ((FAIRY, 1), FLOOR),
            ZOMBIE => ((LEPRECHAUN, 2), FLOOR),
            NONE => ((LEPRECHAUN, 2), FLOOR),
            UNKNOWN => ((LEPRECHAUN, 2), FLOOR));
        Human_Attacker : Presence_Array := (1 => (HUMAN, 2));
        Human_Result_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((VAMPIRE, 1), FLOOR),
            LEPRECHAUN => ((HUMAN, 2), FLOOR),
            HUMAN => ((NONE, 0), FLOOR),
            FAIRY => ((HUMAN, 2), FLOOR),
            ZOMBIE => ((ZOMBIE, 1), FLOOR),
            NONE => ((HUMAN, 2), FLOOR),
            UNKNOWN => ((HUMAN, 2), FLOOR));
        Fairy_Attacker : Presence_Array := (1 => (FAIRY, 2));
        Fairy_Result_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((FAIRY, 2), FLOOR),
            LEPRECHAUN => ((FAIRY, 2), FLOOR),
            HUMAN => ((HUMAN, 1), FLOOR),
            FAIRY => ((NONE, 0), FLOOR),
            ZOMBIE => ((ZOMBIE, 1), FLOOR),
            NONE => ((FAIRY, 2), FLOOR),
            UNKNOWN => ((FAIRY, 2), FLOOR));
        Zombie_Attacker : Presence_Array := (1 => (ZOMBIE, 2));
        Zombie_Result_Tiles : array (Occupant) of Tile := (
            VAMPIRE => ((VAMPIRE, 1), FLOOR),
            LEPRECHAUN => ((LEPRECHAUN, 1), FLOOR),
            HUMAN => ((ZOMBIE, 2), FLOOR),
            FAIRY => ((ZOMBIE, 2), FLOOR),
            ZOMBIE => ((NONE, 0), FLOOR),
            NONE => ((ZOMBIE, 2), FLOOR),
            UNKNOWN => ((ZOMBIE, 2), FLOOR));
    begin
        for The_Occupant in Test_Tiles'Range loop
            Check_Tile_Event (
                Input => Test_Tiles (The_Occupant),
                Attackers => Vampire_Attacker,
                Create => False,
                Expected => Vampire_Result_Tiles (The_Occupant));
            Check_Tile_Event (
                Input => Test_Tiles (The_Occupant),
                Attackers => Leprechaun_Attacker,
                Create => False,
                Expected => Leprechaun_Result_Tiles (The_Occupant));
            Check_Tile_Event (
                Input => Test_Tiles (The_Occupant),
                Attackers => Human_Attacker,
                Create => False,
                Expected => Human_Result_Tiles (The_Occupant));
            Check_Tile_Event (
                Input => Test_Tiles (The_Occupant),
                Attackers => Fairy_Attacker,
                Create => False,
                Expected => Fairy_Result_Tiles (The_Occupant));
            Check_Tile_Event (
                Input => Test_Tiles (The_Occupant),
                Attackers => Zombie_Attacker,
                Create => False,
                Expected => Zombie_Result_Tiles (The_Occupant));
        end loop;
    end Test_Doubles;

    procedure Test_Quads is
        Not_Vampire_Attacker : Presence_Array := (
            1 => (LEPRECHAUN, 1),
            2 => (HUMAN, 2),
            3 => (FAIRY, 3),
            4 => (ZOMBIE, 4));
        Not_Leprechaun_Attacker : Presence_Array := (
            1 => (VAMPIRE, 1),
            2 => (HUMAN, 2),
            3 => (FAIRY, 3),
            4 => (ZOMBIE, 4));
        Not_Human_Attacker : Presence_Array := (
            1 => (VAMPIRE, 1),
            2 => (LEPRECHAUN, 2),
            3 => (FAIRY, 3),
            4 => (ZOMBIE, 4));
        Not_Fairy_Attacker : Presence_Array := (
            1 => (VAMPIRE, 1),
            2 => (LEPRECHAUN, 2),
            3 => (HUMAN, 3),
            4 => (ZOMBIE, 4));
        Not_Zombie_Attacker : Presence_Array := (
            1 => (VAMPIRE, 1),
            2 => (LEPRECHAUN, 2),
            3 => (HUMAN, 3),
            4 => (FAIRY, 4));
        Empty_Tile : Tile := ((NONE, 0), FLOOR);
    begin
        Check_Tile_Event (Empty_Tile, Not_Vampire_Attacker, False, Empty_Tile);
        Check_Tile_Event (Empty_Tile, Not_Leprechaun_Attacker, False,
            Empty_Tile);
        Check_Tile_Event (Empty_Tile, Not_Human_Attacker, False, Empty_Tile);
        Check_Tile_Event (Empty_Tile, Not_Fairy_Attacker, False, Empty_Tile);
        Check_Tile_Event (Empty_Tile, Not_Zombie_Attacker, False, Empty_Tile);
    end Test_Quads;

    procedure Test_Singles is
        Empty_Tile : Tile := ((NONE, 0), FLOOR);
        Vampire_Attacker : Presence_Array := (1 => (VAMPIRE, 1));
        Vampire_Tile : Tile := ((VAMPIRE, 1), FLOOR);
        Leprechaun_Attacker : Presence_Array := (1 => (LEPRECHAUN, 1));
        Leprechaun_Tile : Tile := ((LEPRECHAUN, 1), FLOOR);
        Human_Attacker : Presence_Array := (1 => (HUMAN, 1));
        Human_Tile : Tile := ((HUMAN, 1), FLOOR);
        Fairy_Attacker : Presence_Array := (1 => (FAIRY, 1));
        Fairy_Tile : Tile := ((FAIRY, 1), FLOOR);
        Zombie_Attacker : Presence_Array := (1 => (ZOMBIE, 1));
        Zombie_Tile : Tile := ((ZOMBIE, 1), FLOOR);
    begin
        Check_Tile_Event (Empty_Tile, Vampire_Attacker, False, Vampire_Tile);
        Check_Tile_Event (Empty_Tile, Leprechaun_Attacker, False,
            Leprechaun_Tile);
        Check_Tile_Event (Empty_Tile, Human_Attacker, False, Human_Tile);
        Check_Tile_Event (Empty_Tile, Fairy_Attacker, False, Fairy_Tile);
        Check_Tile_Event (Empty_Tile, Zombie_Attacker, False, Zombie_Tile);
    end Test_Singles;

    procedure Test_Stress is
        Test_Tiles : array (Tile_Kind) of Tile := (
            BASE => ((NONE, 0), BASE),
            PIT => ((FAIRY, 3), PIT),
            FLOOR => ((LEPRECHAUN, 2), FLOOR),
            PRODUCER => ((VAMPIRE, 1), PRODUCER),
            IMPASSABLE => ((HUMAN, 0), IMPASSABLE),
            UNKNOWN => ((UNKNOWN, 0), UNKNOWN));
        Test_Results : array (Tile_Kind) of String (1 .. 3) := (
            BASE => "# 0",
            PIT => " F3",
            FLOOR => "_L2",
            PRODUCER => "$V1",
            IMPASSABLE => "XH0",
            UNKNOWN => "??0");
        Extra_Test_Tile : Tile := ((ZOMBIE, 10), FLOOR);
        Extra_Test_Result : String := "_Z10";
    begin
        for Which in Test_Tiles'Range loop
            Assert (
                To_String (Test_Tiles (Which)) = Test_Results (Which),
                "Expected: " & Test_Results (Which));
        end loop;
        Assert (
            To_String (Extra_Test_Tile) = Extra_Test_Result,
            "Expected: " & Extra_Test_Result);
    end Test_Stress;

    procedure Test_Triples is
        Test_Tiles : array (Unit) of Tile := (
            VAMPIRE => ((VAMPIRE, 1), FLOOR),
            LEPRECHAUN => ((LEPRECHAUN, 1), FLOOR),
            HUMAN => ((HUMAN, 1), FLOOR),
            FAIRY => ((FAIRY, 1), FLOOR),
            ZOMBIE => ((ZOMBIE, 1), FLOOR));
        Attack_Vectors : array (1 .. 10) of Presence_Array (1 .. 2) := (
            1 => (1 => (VAMPIRE, 2), 2 => (LEPRECHAUN, 3)),
            2 => (1 => (VAMPIRE, 2), 2 => (HUMAN, 3)),
            3 => (1 => (VAMPIRE, 2), 2 => (FAIRY, 3)),
            4 => (1 => (VAMPIRE, 2), 2 => (ZOMBIE, 3)),
            5 => (1 => (LEPRECHAUN, 2), 2 => (HUMAN, 3)),
            6 => (1 => (LEPRECHAUN, 2), 2 => (FAIRY, 3)),
            7 => (1 => (LEPRECHAUN, 2), 2 => (ZOMBIE, 3)),
            8 => (1 => (HUMAN, 2), 2 => (FAIRY, 3)),
            9 => (1 => (HUMAN, 2), 2 => (ZOMBIE, 3)),
            10 => (1 => (FAIRY, 2), 2 => (ZOMBIE, 3)));
        Result_Vectors : array (Unit, 1 .. 10) of Tile := (
            VAMPIRE => (
                1 => ((LEPRECHAUN, 3), FLOOR),
                2 => ((NONE, 0), FLOOR),
                3 => ((FAIRY, 3), FLOOR),
                4 => ((NONE, 0), FLOOR),
                5 => ((NONE, 0), FLOOR),
                6 => ((FAIRY, 3), FLOOR),
                7 => ((LEPRECHAUN, 2), FLOOR),
                8 => ((NONE, 0), FLOOR),
                9 => ((VAMPIRE, 1), FLOOR),
                10 => ((NONE, 0), FLOOR)),
            LEPRECHAUN => (
                1 => ((NONE, 0), FLOOR),
                2 => ((NONE, 0), FLOOR),
                3 => ((FAIRY, 3), FLOOR),
                4 => ((LEPRECHAUN, 1), FLOOR),
                5 => ((HUMAN, 3), FLOOR),
                6 => ((FAIRY, 3), FLOOR),
                7 => ((NONE, 0), FLOOR),
                8 => ((HUMAN, 2), FLOOR),
                9 => ((NONE, 0), FLOOR),
                10 => ((NONE, 0), FLOOR)),
            HUMAN => (
                1 => ((NONE, 0), FLOOR),
                2 => ((VAMPIRE, 2), FLOOR),
                3 => ((NONE, 0), FLOOR),
                4 => ((VAMPIRE, 2), FLOOR),
                5 => ((NONE, 0), FLOOR),
                6 => ((HUMAN, 1), FLOOR),
                7 => ((NONE, 0), FLOOR),
                8 => ((NONE, 0), FLOOR),
                9 => ((ZOMBIE, 3), FLOOR),
                10 => ((ZOMBIE, 3), FLOOR)),
            FAIRY => (
                1 => ((FAIRY, 1), FLOOR),
                2 => ((NONE, 0), FLOOR),
                3 => ((NONE, 0), FLOOR),
                4 => ((NONE, 0), FLOOR),
                5 => ((HUMAN, 3), FLOOR),
                6 => ((NONE, 0), FLOOR),
                7 => ((NONE, 0), FLOOR),
                8 => ((HUMAN, 2), FLOOR),
                9 => ((ZOMBIE, 3), FLOOR),
                10 => ((ZOMBIE, 3), FLOOR)),
            ZOMBIE => (
                1 => ((LEPRECHAUN, 3), FLOOR),
                2 => ((VAMPIRE, 2), FLOOR),
                3 => ((NONE, 0), FLOOR),
                4 => ((VAMPIRE, 2), FLOOR),
                5 => ((NONE, 0), FLOOR),
                6 => ((NONE, 0), FLOOR),
                7 => ((LEPRECHAUN, 2), FLOOR),
                8 => ((ZOMBIE, 1), FLOOR),
                9 => ((NONE, 0), FLOOR),
                10 => ((NONE, 0), FLOOR)));
    begin
        for Defender in Test_Tiles'Range loop
            for Attackers in Attack_Vectors'Range loop
                Check_Tile_Event (
                    Input => Test_Tiles (Defender),
                    Attackers => Attack_Vectors (Attackers),
                    Create => False,
                    Expected => Result_Vectors (Defender, Attackers));
            end loop;
        end loop;
    end Test_Triples;

    procedure Test_Unusual_Tiles is
        Test_Tiles : array (Tile_Kind) of Tile := (
            BASE => ((NONE, 0), BASE),
            PIT => ((NONE, 0), PIT),
            FLOOR => ((NONE, 0), FLOOR),
            PRODUCER => ((NONE, 0), PRODUCER),
            IMPASSABLE => ((NONE, 0), IMPASSABLE),
            UNKNOWN => ((NONE, 0), UNKNOWN));
        Ground_Attackers : Presence_Array := (
            1 => (VAMPIRE, 1),
            2 => (HUMAN, 2));
        Both_Attackers : Presence_Array := (
            1 => (FAIRY, 1),
            2 => (HUMAN, 2));
        Air_Attackers : Presence_Array := (1 => (FAIRY, 1));
        Ground_Results : array (Tile_Kind) of Tile := (
            BASE => ((VAMPIRE, 1), BASE),
            PIT => ((NONE, 0), PIT),
            FLOOR => ((VAMPIRE, 1), FLOOR),
            PRODUCER => ((VAMPIRE, 1), PRODUCER),
            IMPASSABLE => ((NONE, 0), IMPASSABLE),
            UNKNOWN => ((VAMPIRE, 1), UNKNOWN));
        Both_Results : array (Tile_Kind) of Tile := (
            BASE => ((HUMAN, 2), BASE),
            PIT => ((NONE, 0), PIT),
            FLOOR => ((HUMAN, 2), FLOOR),
            PRODUCER => ((HUMAN, 2), PRODUCER),
            IMPASSABLE => ((NONE, 0), IMPASSABLE),
            UNKNOWN => ((HUMAN, 2), UNKNOWN));
        Air_Results : array (Tile_Kind) of Tile := (
            BASE => ((FAIRY, 1), BASE),
            PIT => ((FAIRY, 1), PIT),
            FLOOR => ((FAIRY, 1), FLOOR),
            PRODUCER => ((FAIRY, 1), PRODUCER),
            IMPASSABLE => ((NONE, 0), IMPASSABLE),
            UNKNOWN => ((FAIRY, 1), UNKNOWN));
    begin
        for Which in Test_Tiles'Range loop
            Check_Tile_Event (
                Input => Test_Tiles (Which),
                Attackers => Ground_Attackers,
                Create => False,
                Expected => Ground_Results (Which));
            Check_Tile_Event (
                Input => Test_Tiles (Which),
                Attackers => Both_Attackers,
                Create => False,
                Expected => Both_Results (Which));
            Check_Tile_Event (
                Input => Test_Tiles (Which),
                Attackers => Air_Attackers,
                Create => False,
                Expected => Air_Results (Which));
        end loop;
    end Test_Unusual_Tiles;

end Test_Tile;

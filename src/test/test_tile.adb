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
    begin
        null;
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
        Empty_Result_Tile : Tile := ((NONE, 1), FLOOR);
        Fairy_Result_Tile : Tile := ((FAIRY, 1), FLOOR);
        Human_Result_Tile : Tile := ((HUMAN, 1), FLOOR);
    begin
        Check_Tile_Event (The_Tile, No_Attackers, True, Empty_Result_Tile);
        Check_Tile_Event (The_Tile, Fairy_Attacker, True, Fairy_Result_Tile);
        Check_Tile_Event (The_Tile, Human_Attacker, True, Human_Result_Tile);
    end Test_Create;

    procedure Test_Doubles is
    begin
        null;
    end Test_Doubles;

    procedure Test_Quads is
    begin
        null;
    end Test_Quads;

    procedure Test_Singles is
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
    end Test_Singles;

    procedure Test_Stress is
    begin
        null;
    end Test_Stress;

    procedure Test_Triples is
    begin
        null;
    end Test_Triples;

    procedure Test_Unusual_Tiles is
    begin
        null;
    end Test_Unusual_Tiles;

end Test_Tile;

with Test_Suites; use Test_Suites;
with Actions; use Actions;
with Units; use Units;
with Tiles; use Tiles;

package body Test_Action is
    procedure Assert_Action_IO is new Assert_IO (
        Any_Type => Action,
        To_String => To_String);

    procedure Assert_Equal_Action_Set (
        First : in Action_Array;
        Second : in Action_Array;
        Message : in String) is
        Found : Boolean;
    begin
        if First'Length /= Second'Length then
            Assert (False, Message);
        end if;

        for First_Index in First'Range loop
            Found := False;
            for Second_Index in Second'Range loop
                if First (First_Index) = Second (Second_Index) then
                    Found := True;
                end if;
            end loop;
            if not Found then
                Assert (False, Message);
            end if;
        end loop;
        Assert (True, Message);
    end Assert_Equal_Action_Set;

    procedure Test_Distance is
        Nearby_Move : array (Unit) of Action := (
            VAMPIRE => ((1, 1), (3, 1), MOVE, VAMPIRE, 1),
            LEPRECHAUN => ((1, 1), (2, 1), MOVE, LEPRECHAUN, 1),
            HUMAN => ((1, 1), (2, 1), MOVE, HUMAN, 1),
            FAIRY => ((1, 1), (2, 1), MOVE, FAIRY, 1),
            ZOMBIE => ((1, 1), (2, 1), MOVE, ZOMBIE, 1));
        Far_Move : array (Unit) of Action := (
            VAMPIRE => ((1, 1), (4, 1), MOVE, VAMPIRE, 1),
            LEPRECHAUN => ((1, 1), (3, 1), MOVE, LEPRECHAUN, 1),
            HUMAN => ((1, 1), (3, 1), MOVE, HUMAN, 1),
            FAIRY => ((1, 1), (3, 1), MOVE, FAIRY, 1),
            ZOMBIE => ((1, 1), (3, 1), MOVE, ZOMBIE, 1));
        Nearby_Diagonal_Move : array (Unit) of Action := (
            VAMPIRE => ((1, 1), (2, 2), MOVE, VAMPIRE, 1),
            LEPRECHAUN => ((1, 1), (2, 2), MOVE, LEPRECHAUN, 1),
            HUMAN => ((1, 1), (2, 2), MOVE, HUMAN, 1),
            FAIRY => ((1, 1), (2, 2), MOVE, FAIRY, 1),
            ZOMBIE => ((1, 1), (2, 2), MOVE, ZOMBIE, 1));
        Far_Diagonal_Move : array (Unit) of Action := (
            VAMPIRE => ((1, 1), (3, 3), MOVE, VAMPIRE, 1),
            LEPRECHAUN => ((1, 1), (3, 3), MOVE, LEPRECHAUN, 1),
            HUMAN => ((1, 1), (3, 3), MOVE, HUMAN, 1),
            FAIRY => ((1, 1), (3, 3), MOVE, FAIRY, 1),
            ZOMBIE => ((1, 1), (3, 3), MOVE, ZOMBIE, 1));
        Near_Create : Action := ((1, 1), (2, 1), CREATE, LEPRECHAUN, 1);
        Near_Diagonal_Create : Action :=
            ((1, 1), (2, 2), CREATE, LEPRECHAUN, 1);
        Far_Create : Action := ((1, 1), (3, 1), CREATE, LEPRECHAUN, 1);
        Far_Diagonal_Create : Action := ((1, 1), (3, 2), CREATE, LEPRECHAUN, 1);
        Bad_Create : Action := ((1, 1), (2, 1), CREATE, HUMAN, 1);
        Too_Close_Create : Action := ((1, 1), (1, 1), CREATE, LEPRECHAUN, 1);
        Valid_Destroy : Action := ((1, 1), (2, 1), DESTROY, HUMAN, 1);
        Valid_Infect : Action := ((1, 1), (2, 1), INFECT, ZOMBIE, 1);
        Valid_Spawn : Action := ((1, 1), (1, 1), SPAWN, VAMPIRE, 1);
    begin
        for Mover in Nearby_Move'Range loop
            Assert (Is_Valid (Nearby_Move (Mover)),
                "Movement of " & Occupant'Image (Mover) &
                " should have been valid");
            Assert (not Is_Valid (Far_Move (Mover)),
                "Movement of " & Occupant'Image (Mover) &
                " should have been invalid");
            Assert (Is_Valid (Nearby_Diagonal_Move (Mover)),
                "Movement of " & Occupant'Image (Mover) &
                " should have been valid");
            Assert (not Is_Valid (Far_Diagonal_Move (Mover)),
                "Movement of " & Occupant'Image (Mover) &
                " should have been invalid");
        end loop;

        Assert (Is_Valid (Near_Create),
            "Creation nearby should have been valid.");
        Assert (Is_Valid (Near_Diagonal_Create),
            "Creation nearby on diagonal should have been valid.");
        Assert (not Is_Valid (Far_Create),
            "Creation far should have been invalid.");
        Assert (not Is_Valid (Far_Diagonal_Create),
            "Creation far on diagonal should have been invalid.");
        Assert (not Is_Valid (Bad_Create),
            "Creation by human should have been invalid.");
        Assert (not Is_Valid (Too_Close_Create),
            "Creation on same tile should have been invalid.");
        Assert (Is_Valid (Valid_Destroy),
            "Valid destroy should have been valid.");
        Assert (Is_Valid (Valid_Infect),
            "Valid infect should have been valid.");
        Assert (Is_Valid (Valid_Spawn),
            "Valid spawn should have been valid.");
    end Test_Distance;

    procedure Test_IO is
        Sample_Move : Action := ((1, 1), (1, 1), MOVE, VAMPIRE, 1);
        Move_String : String := " 1, 1 ->  1, 1 (" & ASCII.ESC & "[31m" & " 1)";
        Sample_Action : Action := ((1, 1), (1, 1), SPAWN, VAMPIRE, 1);
        Action_String : String := " 1, 1 ~>  1, 1 (" & ASCII.ESC & "[31m 1)";
    begin
        Assert_Action_IO (Sample_Move);
        Assert (
            To_String (Sample_Move) = Move_String,
            "Expected: " & Move_String);

        Assert_Action_IO (Sample_Action);
        Assert (
            To_String (Sample_Action) = Action_String,
            "Expected: " & Action_String);
    end Test_IO;

    procedure Test_Query_Actions is
        --  A team 1 vampire at (10,10) on a 20x20 board
        Central_Vampire_Results : Action_Array := (
            1 => (Source => (10, 10), Target => (10, 8),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            2 => (Source => (10, 10), Target => (9, 9),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            3 => (Source => (10, 10), Target => (10, 9),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            4 => (Source => (10, 10), Target => (11, 9),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            5 => (Source => (10, 10), Target => (8, 10),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            6 => (Source => (10, 10), Target => (9, 10),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            7 => (Source => (10, 10), Target => (11, 10),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            8 => (Source => (10, 10), Target => (12, 10),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            9 => (Source => (10, 10), Target => (9, 11),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            10 => (Source => (10, 10), Target => (10, 11),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            11 => (Source => (10, 10), Target => (11, 11),
                Kind => MOVE, Unit => VAMPIRE, Team => 1),
            12 => (Source => (10, 10), Target => (10, 12),
                Kind => MOVE, Unit => VAMPIRE, Team => 1));

        --  A team 1 leprechaun at (10,10) on a 20x20 board
        Central_Leprechaun_Results : Action_Array := (
            1 => (Source => (10, 10), Target => (9, 9),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            2 => (Source => (10, 10), Target => (10, 9),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            3 => (Source => (10, 10), Target => (11, 9),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            4 => (Source => (10, 10), Target => (9, 10),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            5 => (Source => (10, 10), Target => (11, 10),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            6 => (Source => (10, 10), Target => (9, 11),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            7 => (Source => (10, 10), Target => (10, 11),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            8 => (Source => (10, 10), Target => (11, 11),
                Kind => MOVE, Unit => LEPRECHAUN, Team => 1),
            9 => (Source => (10, 10), Target => (9, 9),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            10 => (Source => (10, 10), Target => (10, 9),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            11 => (Source => (10, 10), Target => (11, 9),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            12 => (Source => (10, 10), Target => (9, 10),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            13 => (Source => (10, 10), Target => (11, 10),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            14 => (Source => (10, 10), Target => (9, 11),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            15 => (Source => (10, 10), Target => (10, 11),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1),
            16 => (Source => (10, 10), Target => (11, 11),
                Kind => CREATE, Unit => LEPRECHAUN, Team => 1));

        --  A team 1 human at (10,10) on a 20x20 board
        Central_Human_Results : Action_Array := (
            1 => (Source => (10, 10), Target => (9, 9),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            2 => (Source => (10, 10), Target => (10, 9),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            3 => (Source => (10, 10), Target => (11, 9),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            4 => (Source => (10, 10), Target => (9, 10),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            5 => (Source => (10, 10), Target => (11, 10),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            6 => (Source => (10, 10), Target => (9, 11),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            7 => (Source => (10, 10), Target => (10, 11),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            8 => (Source => (10, 10), Target => (11, 11),
                Kind => MOVE, Unit => HUMAN, Team => 1),
            9 => (Source => (10, 10), Target => (9, 9),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            10 => (Source => (10, 10), Target => (10, 9),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            11 => (Source => (10, 10), Target => (11, 9),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            12 => (Source => (10, 10), Target => (9, 10),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            13 => (Source => (10, 10), Target => (11, 10),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            14 => (Source => (10, 10), Target => (9, 11),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            15 => (Source => (10, 10), Target => (10, 11),
                Kind => DESTROY, Unit => HUMAN, Team => 1),
            16 => (Source => (10, 10), Target => (11, 11),
                Kind => DESTROY, Unit => HUMAN, Team => 1));

        --  A team 1 zombie at (10,10) on a 20x20 board
        Central_Zombie_Results : Action_Array := (
            1 => (Source => (10, 10), Target => (9, 9),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            2 => (Source => (10, 10), Target => (10, 9),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            3 => (Source => (10, 10), Target => (11, 9),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            4 => (Source => (10, 10), Target => (9, 10),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            5 => (Source => (10, 10), Target => (11, 10),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            6 => (Source => (10, 10), Target => (9, 11),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            7 => (Source => (10, 10), Target => (10, 11),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            8 => (Source => (10, 10), Target => (11, 11),
                Kind => MOVE, Unit => ZOMBIE, Team => 1),
            9 => (Source => (10, 10), Target => (9, 9),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            10 => (Source => (10, 10), Target => (10, 9),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            11 => (Source => (10, 10), Target => (11, 9),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            12 => (Source => (10, 10), Target => (9, 10),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            13 => (Source => (10, 10), Target => (11, 10),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            14 => (Source => (10, 10), Target => (9, 11),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            15 => (Source => (10, 10), Target => (10, 11),
                Kind => INFECT, Unit => ZOMBIE, Team => 1),
            16 => (Source => (10, 10), Target => (11, 11),
                Kind => INFECT, Unit => ZOMBIE, Team => 1));

        --  A team 1 fairy at (10,10) on a 20x20 board
        Central_Fairy_Results : Action_Array := (
            1 => (Source => (10, 10), Target => (9, 9),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            2 => (Source => (10, 10), Target => (10, 9),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            3 => (Source => (10, 10), Target => (11, 9),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            4 => (Source => (10, 10), Target => (9, 10),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            5 => (Source => (10, 10), Target => (11, 10),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            6 => (Source => (10, 10), Target => (9, 11),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            7 => (Source => (10, 10), Target => (10, 11),
                Kind => MOVE, Unit => FAIRY, Team => 1),
            8 => (Source => (10, 10), Target => (11, 11),
                Kind => MOVE, Unit => FAIRY, Team => 1));

        --  A team 1 base at (1, 1) on a 20x20 board
        Corner_Base_Results : Action_Array := (
            1 => (Source => (1, 2), Target => (1, 2),
                Kind => SPAWN, Unit => VAMPIRE, Team => 1),
            2 => (Source => (2, 1), Target => (2, 1),
                Kind => SPAWN, Unit => VAMPIRE, Team => 1),
            3 => (Source => (2, 2), Target => (2, 2),
                Kind => SPAWN, Unit => VAMPIRE, Team => 1),
            4 => (Source => (1, 2), Target => (1, 2),
                Kind => SPAWN, Unit => LEPRECHAUN, Team => 1),
            5 => (Source => (2, 1), Target => (2, 1),
                Kind => SPAWN, Unit => LEPRECHAUN, Team => 1),
            6 => (Source => (2, 2), Target => (2, 2),
                Kind => SPAWN, Unit => LEPRECHAUN, Team => 1),
            7 => (Source => (1, 2), Target => (1, 2),
                Kind => SPAWN, Unit => HUMAN, Team => 1),
            8 => (Source => (2, 1), Target => (2, 1),
                Kind => SPAWN, Unit => HUMAN, Team => 1),
            9 => (Source => (2, 2), Target => (2, 2),
                Kind => SPAWN, Unit => HUMAN, Team => 1),
            10 => (Source => (1, 2), Target => (1, 2),
                Kind => SPAWN, Unit => FAIRY, Team => 1),
            11 => (Source => (2, 1), Target => (2, 1),
                Kind => SPAWN, Unit => FAIRY, Team => 1),
            12 => (Source => (2, 2), Target => (2, 2),
                Kind => SPAWN, Unit => FAIRY, Team => 1),
            13 => (Source => (1, 2), Target => (1, 2),
                Kind => SPAWN, Unit => ZOMBIE, Team => 1),
            14 => (Source => (2, 1), Target => (2, 1),
                Kind => SPAWN, Unit => ZOMBIE, Team => 1),
            15 => (Source => (2, 2), Target => (2, 2),
                Kind => SPAWN, Unit => ZOMBIE, Team => 1));
        Empty_Array : Actions.Action_Array (1 .. 0) := (
            others => ((1, 1), (1, 1), Actions.MOVE, Units.VAMPIRE, 1));
    begin
        Assert_Equal_Action_Set (
            First => Central_Vampire_Results,
            Second => Get_Actions_From (
                What => ((VAMPIRE, 1), FLOOR),
                Where => (10, 10),
                Board_Size => (20, 20)),
            Message => "Wrong central vampire actions.");
        Assert_Equal_Action_Set (
            First => Central_Leprechaun_Results,
            Second => Get_Actions_From (
                What => ((LEPRECHAUN, 1), FLOOR),
                Where => (10, 10),
                Board_Size => (20, 20)),
            Message => "Wrong central leprechaun actions.");
        Assert_Equal_Action_Set (
            First => Central_Human_Results,
            Second => Get_Actions_From (
                What => ((HUMAN, 1), FLOOR),
                Where => (10, 10),
                Board_Size => (20, 20)),
            Message => "Wrong central human actions.");
        Assert_Equal_Action_Set (
            First => Central_Zombie_Results,
            Second => Get_Actions_From (
                What => ((ZOMBIE, 1), FLOOR),
                Where => (10, 10),
                Board_Size => (20, 20)),
            Message => "Wrong central zombie actions.");
        Assert_Equal_Action_Set (
            First => Central_Fairy_Results,
            Second => Get_Actions_From (
                What => ((FAIRY, 1), FLOOR),
                Where => (10, 10),
                Board_Size => (20, 20)),
            Message => "Wrong central fairy actions.");
        Assert_Equal_Action_Set (
            First => Corner_Base_Results,
            Second => Get_Actions_From (
                What => ((NONE, 1), BASE),
                Where => (1, 1),
                Board_Size => (20, 20)),
            Message => "Wrong corner base actions.");
        Assert_Equal_Action_Set (
            First => Empty_Array,
            Second => Get_Actions_From (
                What => ((NONE, 1), FLOOR),
                Where => (5, 5),
                Board_Size => (20, 20)),
            Message => "Wrong empty tile actions.");
    end Test_Query_Actions;

    procedure Test_Stress is
        Spawn_Move : Action := ((1, 1), (1, 1), MOVE, ZOMBIE, 1);
        Move_Spawn : Action := ((1, 1), (1, 2), SPAWN, ZOMBIE, 1);
        Leprechaun_Destroy : Action := ((1, 1), (1, 2), DESTROY, LEPRECHAUN, 1);
        Human_Create : Action := ((1, 1), (1, 2), CREATE, HUMAN, 1);
        Fairy_Infect : Action := ((1, 1), (1, 2), INFECT, FAIRY, 1);
        Vampire_Spawn : Action := ((1, 1), (1, 2), SPAWN, VAMPIRE, 1);
    begin
        Assert (not Is_Valid (Spawn_Move),
            "Spawn type movement should have been invalid");
        Assert (not Is_Valid (Move_Spawn),
            "Movement type spawn should have been invalid");
        Assert (not Is_Valid (Leprechaun_Destroy),
            "Leprechaun should not have been able to destroy");
        Assert (not Is_Valid (Human_Create),
            "Human should not have been able to create");
        Assert (not Is_Valid (Fairy_Infect),
            "Fairy should not have been able to infect");
        Assert (not Is_Valid (Vampire_Spawn),
            "Vampire should not have been able to spawn");
    end Test_Stress;
end Test_Action;

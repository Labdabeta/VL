with Test_Suites; use Test_Suites;
with Actions; use Actions;
with Units; use Units;

package body Test_Action is
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
    end Test_Distance;

    procedure Test_Stress is
    begin
        null;
    end Test_Stress;
end Test_Action;

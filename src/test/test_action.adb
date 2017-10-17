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

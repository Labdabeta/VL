with Coordinates;
with Units;

package Actions is
    type Action_Kind is (MOVE, CREATE, DESTROY, INFECT, SPAWN);

    type Action is record
        Source, Target : Coordinates.Coordinate;
        Kind : Action_Kind;
        Unit : Units.Unit;
        Team : Positive;
    end record;

    Is_Move_Actor : constant array (Units.Unit) of Boolean := (
        Units.LEPRECHAUN => True,
        Units.HUMAN => True,
        Units.ZOMBIE => True,
        others => False);

    Move_Action_Type : constant array (Units.Unit) of Action_Kind := (
        Units.LEPRECHAUN => CREATE,
        Units.HUMAN => DESTROY,
        Units.ZOMBIE => INFECT,
        others => MOVE);

    function Is_Valid (
        This : in Action)
        return Boolean;

    type Action_Array is array (Positive range <>) of Action;
end Actions;

with Test_Suites; use Test_Suites;
with Coordinates; use Coordinates;

package body Test_Coordinate is

    procedure Assert_Equal_Naturals is new Assert_Equal (
        Value_Type => Natural);

    procedure Assert_Equal_Coordinate_Set (
        First : in Coordinate_List;
        Second : in Coordinate_List;
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
    end Assert_Equal_Coordinate_Set;

    procedure Test_Adjacent is
        Top_Left_Results : Coordinate_List (1 .. 3) := (
            1 => (1, 2), 2 => (2, 2), 3 => (2, 1));
        Bottom_Right_Results : Coordinate_List (1 .. 3) := (
            1 => (9, 8), 2 => (8, 8), 3 => (8, 9));
        Central_Results : Coordinate_List (1 .. 8) := (
            1 => (4, 4), 2 => (4, 5), 3 => (4, 6),
            4 => (5, 4),              5 => (5, 6),
            6 => (6, 4), 7 => (6, 5), 8 => (6, 6));
    begin
        Assert_Equal_Coordinate_Set (
            First => Get_Adjacent_Within (
                Source => (1, 1), Bounds => (9, 9)),
            Second => Top_Left_Results,
            Message => "Wrong Top Left Results");
        Assert_Equal_Coordinate_Set (
            First => Get_Adjacent_Within (
                Source => (9, 9), Bounds => (9, 9)),
            Second => Bottom_Right_Results,
            Message => "Wrong Bottom Right Results");
        Assert_Equal_Coordinate_Set (
            First => Get_Adjacent_Within (
                Source => (5, 5), Bounds => (9, 9)),
            Second => Central_Results,
            Message => "Wrong Central Results");
    end Test_Adjacent;

    procedure Test_Distance is
        Origin : Coordinate := (
            X => 1,
            Y => 1);
        Some_Point : Coordinate := (
            X => 2,
            Y => 2);
        Another_Point : Coordinate := (
            X => 3,
            Y => 1);
        Expected_Distance : Natural := 1;
        Another_Distance : Natural := 2;
    begin
        Assert_Equal_Naturals (
            Value =>
                Distance (
                    Source => Some_Point,
                    Destination => Origin),
            Expected => Expected_Distance);

        Assert_Equal_Naturals (
            Value =>
                Distance (
                    Source => Origin,
                    Destination => Some_Point),
            Expected => Expected_Distance);

        Assert_Equal_Naturals (
            Value =>
                Distance (
                    Source => Another_Point,
                    Destination => Origin),
            Expected => Another_Distance);
    end Test_Distance;

    procedure Test_Stress is
        Origin : Coordinate := (
            X => 1,
            Y => 1);
        Some_Point : Coordinate := (
            X => 2,
            Y => 2);
        Expected_Distance : Natural := 1;
    begin
        Assert_Equal_Naturals (
            Value =>
                Distance (
                    Source => Origin,
                    Destination => Origin),
            Expected => 0);
    end Test_Stress;

end Test_Coordinate;

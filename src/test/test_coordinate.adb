with Test_Suites; use Test_Suites;
with Coordinates; use Coordinates;

package body Test_Coordinate is

    procedure Assert_Equal_Naturals is new Assert_Equal (
        Value_Type => Natural);

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

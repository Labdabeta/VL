with Test_Suites;

package Test_Coordinate is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Test_Distance;
    Test_Distance_Name : aliased String := "Distance Test";

    procedure Test_Stress;
    Test_Stress_Name : aliased String := "Stress Test";

    procedure Test_Adjacent;
    Test_Adjacent_Name : aliased String := "Adjacent Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Test_Distance'Access,
            Name => Test_Distance_Name'Access),
        2 => (
            Callback => Test_Stress'Access,
            Name => Test_Stress_Name'Access),
        3 => (
            Callback => Test_Adjacent'Access,
            Name => Test_Adjacent_Name'Access));
end Test_Coordinate;

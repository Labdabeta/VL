with Test_Suites;

package Test_Action is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Test_Distance;
    Test_Distance_Name : aliased String := "Distance Test";

    procedure Test_Stress;
    Test_Stress_Name : aliased String := "Stress Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Test_Distance'Access,
            Name => Test_Distance_Name'Access),
        2 => (
            Callback => Test_Stress'Access,
            Name => Test_Stress_Name'Access));
end Test_Action;

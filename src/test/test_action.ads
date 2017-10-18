with Test_Suites;

package Test_Action is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Test_Distance;
    Test_Distance_Name : aliased String := "Distance Test";

    procedure Test_Stress;
    Test_Stress_Name : aliased String := "Stress Test";

    procedure Test_Query_Actions;
    Test_Query_Actions_Name : aliased String := "Get Actions Test";

    procedure Test_IO;
    Test_IO_Name : aliased String := "Action IO Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Test_Distance'Access,
            Name => Test_Distance_Name'Access),
        2 => (
            Callback => Test_Stress'Access,
            Name => Test_Stress_Name'Access),
        3 => (
            Callback => Test_Query_Actions'Access,
            Name => Test_Query_Actions_Name'Access),
        4 => (
            Callback => Test_IO'Access,
            Name => Test_IO_Name'Access));
end Test_Action;

with Test_Suites;

package Test_Board is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Basic_Test;
    Basic_Test_Name : aliased String := "Basic Test";

    procedure Action_Test;
    Action_Test_Name : aliased String := "Action Test";

    procedure IO_Test;
    IO_Test_Name : aliased String := "IO Test";

    procedure Utility_Test;
    Utility_Test_Name : aliased String := "Utility Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Basic_Test'Access,
            Name => Basic_Test_Name'Access),
        2 => (
            Callback => IO_Test'Access,
            Name => IO_Test_Name'Access),
        3 => (
            Callback => Action_Test'Access,
            Name => Action_Test_Name'Access),
        4 => (
            Callback => Utility_Test'Access,
            Name => Utility_Test_Name'Access));
end Test_Board;

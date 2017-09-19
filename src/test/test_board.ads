with Test_Suites;

package Test_Board is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Basic_Test;
    Basic_Test_Name : aliased String := "Basic Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Basic_Test'Access,
            Name => Basic_Test_Name'Access));
end Test_Board;

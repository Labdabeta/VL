with Test_Suites;

package Test_Net is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Test_Server;
    Test_Server_Name : aliased String := "Server Test";

    procedure Test_Client;
    Test_Client_Name : aliased String := "Client Test";

    procedure Test_Stress;
    Test_Stress_Name : aliased String := "Stress Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Test_Server'Access,
            Name => Test_Server_Name'Access),
        2 => (
            Callback => Test_Client'Access,
            Name => Test_Client_Name'Access),
        3 => (
            Callback => Test_Stress'Access,
            Name => Test_Stress_Name'Access));
end Test_Net;

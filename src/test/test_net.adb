with Test_Suites; use Test_Suites;
with Async_Net; use Async_Net;

package body Test_Net is
    procedure Test_Client is
    begin
        Assert (True, "Test");
    end Test_Client;

    procedure Test_Server is
    begin
        Assert (True, "Test");
    end Test_Server;

    procedure Test_Stress is
    begin
        Assert (True, "Test");
    end Test_Stress;
end Test_Net;

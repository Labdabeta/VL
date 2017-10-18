package Test_Suites is
    -- A callback is a procedure that takes no arguments
    type Test_Case_Callback is access procedure;

    type Test_Case is record
        Callback : Test_Case_Callback;
        Name : access String;
    end record;

    type Test_Suite is array (Integer range <>) of Test_Case;

    type Test_Suites is array (Integer range <>) of access constant Test_Suite;

    function Run_Suite (
        Suite : in Test_Suite)
        return Boolean;

    procedure Assert (
        Expression : in Boolean;
        Message : in String);

    generic
        type Value_Type is (<>);
    procedure Assert_Equal (
        Value : in Value_Type;
        Expected : in Value_Type);

    generic
        type Value_Type is private;
        with function "=" (X, Y : in Value_Type) return Boolean is <>;
        with function To_String (X : in Value_Type) return String;
    procedure Assert_Same (
        Value : in Value_Type;
        Expected : in Value_Type);

    generic
        type Any_Type (<>) is private;
        with function "=" (X, Y : in Any_Type) return Boolean is <>;
        with function To_String (X : in Any_Type) return String;
    procedure Assert_IO (
        Value : in Any_Type);
private
    Successes : Natural := 0;
    IO_Num : Natural := 0;
end Test_Suites;

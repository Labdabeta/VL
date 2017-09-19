with Ada.Text_IO; use Ada.Text_IO;

package body Test_Suites is
    Failed_Assert : exception;

    procedure Assert (
        Expression : in Boolean;
        Message : in String) is
    begin
        if not Expression then
            Put ("Test case failed: ");
            Put_Line (Message);
            raise Failed_Assert;
        end if;
        Successes := Successes + 1;
    end Assert;

    procedure Assert_Equal (
        Value : in Value_Type;
        Expected : in Value_Type) is
    begin
        if not (Value = Expected) then
            Put_Line ("Equality assertion failed.");
            Put_Line ("Expected: " & Value_Type'Image (Expected));
            Put_Line ("Got: " & Value_Type'Image (Value));
            raise Failed_Assert;
        end if;
        Successes := Successes + 1;
    end Assert_Equal;

    procedure Assert_Same (
        Value : in Value_Type;
        Expected : in Value_Type) is
    begin
        if not (Value = Expected) then
            Put_Line ("Equality assertion failed.");
            Put_Line ("Expected: " & To_String (Expected));
            Put_Line ("Got: " & To_String (Value));
            raise Failed_Assert;
        end if;
        Successes := Successes + 1;
    end Assert_Same;

    function Run_Suite (
        Suite : in Test_Suite)
        return Boolean is
        Result : Boolean := True;
    begin
        for Suite_Number in Suite'Range loop
            Successes := 0;
            begin
                Suite (Suite_Number).Callback.all;
                if Successes = 1 then
                    Put_Line ("Case " & Suite (Suite_Number).Name.all &
                    " passed with " & Natural'Image (Successes) & " success.");
                else
                    Put_Line ("Case " & Suite (Suite_Number).Name.all &
                    " passed with " & Natural'Image (Successes) &
                    " successes.");
                end if;
            exception
                when Failed_Assert =>
                    Put_Line ("Case " & Suite (Suite_Number).Name.all &
                    " failed after " & Natural'Image (Successes) &
                    " successes.");
                    Result := False;
            end;
        end loop;

        return Result;
    end Run_Suite;
end Test_Suites;

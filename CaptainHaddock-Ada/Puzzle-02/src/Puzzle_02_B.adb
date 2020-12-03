with Password_Database; use Password_Database;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Text_IO; -- use Ada.Text_IO;

-- Each policy actually describes two positions in the password, 
-- where 1 means the first character, 2 means the second character, and so on. 
-- (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) 
-- Exactly one of these positions must contain the given letter. 
-- Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

-- How many passwords are valid according to the new interpretation of the policies?


procedure Puzzle_02_A is

    some_Password : Password_Item;
    Count_Good_Passwords : Natural := 0;

--  DAT_File_Name : String(1..2**15); -- 32_768
    Database : Ada.Wide_Wide_Text_IO.File_Type;
    Missing_FileName : exception;


begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    Ada.Wide_Wide_Text_IO.open(File => Database,
         Mode => Ada.Wide_Wide_Text_IO.In_File,
         Name => Argument(1));

    while not Ada.Wide_Wide_Text_IO.end_of_file(Database) loop
        some_Password := get_Next_Password(Database);
        if is_OK(some_Password) then
            put(Standard_Error,"!"); -- some trace breadcum ...
            Count_Good_Passwords := Count_Good_Passwords +1;
        else
            put(Standard_Error,"."); -- some trace breadcum ...
        end if;
    end loop;

    New_Line;
    Ada.Text_IO.Put_Line("Number of good Passwords detected =" & Natural'Image(Count_Good_Passwords));

    Ada.Wide_Wide_Text_IO.close(Database);

    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        Ada.Text_IO.put_line("usage: "& Command_Name & " Password_File_Name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        Ada.Text_IO.put_line(Ada.Text_IO.Standard_Error,"File '"&Argument(1)&"' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"No results!");
        set_Exit_Status(Failure);
        raise;
end Puzzle_02_A;

with Password_Database_UTF8; use Password_Database_UTF8;

with Ada.Command_Line; use Ada.Command_Line;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Ada.Strings.UTF_Encoding; 
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings.UTF_Encoding;


-- Each line gives the password policy and then the password. 
-- The password policy indicates the lowest and highest number of times a given letter 
-- must appear for the password to be valid. 
-- For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

procedure Puzzle_02_A_UTF8 is

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
    Ada.Wide_Wide_Text_IO.Put_Line("Number of good Passwords detected =" & Natural'Wide_Wide_Image(Count_Good_Passwords));

    Ada.Wide_Wide_Text_IO.close(Database);

    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        Ada.Wide_Wide_Text_IO.put_line("usage: "& Wide_Wide_Strings.Decode(Strings.Encode(Command_Name,UTF_8),UTF_8) & " Password_File_Name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        Ada.Wide_Wide_Text_IO.put_line(Ada.Wide_Wide_Text_IO.Standard_Error,"File '"& Wide_Wide_Strings.Decode(Strings.Encode(Argument(1),UTF_8),UTF_8) & "' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"No results!");
        set_Exit_Status(Failure);
        raise;
end Puzzle_02_A_UTF8;

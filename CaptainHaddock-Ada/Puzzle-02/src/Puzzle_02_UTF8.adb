-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-02 
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------
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

procedure Puzzle_02_UTF8 is

    some_Password : Password_Item;
    Count_Good_Passwords_Rule_1, Count_Good_Passwords_Rule_2 : Natural := 0;

--  DAT_File_Name : String(1..2**15); -- 32_768
    Database : File_Type;
    Missing_FileName : exception;


begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    open(File => Database,
         Mode => In_File,
         Name => Argument(1));

    while not end_of_file(Database) loop
        some_Password := get_Next_Password(Database);

        -- Verify first rule
        if is_OK_Rule_1(some_Password) then
            -- put(Standard_Error,"!"); -- some trace breadcum ...
            Count_Good_Passwords_Rule_1 := Count_Good_Passwords_Rule_1 +1;
        else
            null;
            -- put(Standard_Error,"."); -- some trace breadcum ...
        end if;

        -- Verify second rule
        if is_OK_Rule_2(some_Password) then
            -- put(Standard_Error,"!"); -- some trace breadcum ...
            Count_Good_Passwords_Rule_2 := Count_Good_Passwords_Rule_2 +1;
        else
            null;
            -- put(Standard_Error,"."); -- some trace breadcum ...
        end if;
    end loop;

    New_Line;
    Put_Line("Number of good Passwords detected according to Rule #1 =" & Natural'Wide_Wide_Image(Count_Good_Passwords_Rule_1));
    Put_Line("Number of good Passwords detected according to Rule #2 =" & Natural'Wide_Wide_Image(Count_Good_Passwords_Rule_2));

    close(Database);

    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        put_line("usage: "& Wide_Wide_Strings.Decode(Strings.Encode(Command_Name,UTF_8),UTF_8) & " Password_File_Name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        put_line(Standard_Error,"File '"& Wide_Wide_Strings.Decode(Strings.Encode(Argument(1),UTF_8),UTF_8) & "' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"Error reading the structure of some record in the file !");
        set_Exit_Status(Failure);
        raise;
end Puzzle_02_UTF8;

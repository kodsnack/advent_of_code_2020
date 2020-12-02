with Password_Database; use Password_Database;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;


procedure Puzzle_02_A is

    some_Password : Password_Item;
    Count_Good_Passwords : Natural := 0;

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
        -- put(Standard_Error,"."); -- some trace breadcum ...
        some_Password := get_Next_Password(Database);
        if is_OK(some_Password) then
            put(Standard_Error,"!"); -- some trace breadcum ...
            Count_Good_Passwords := Count_Good_Passwords +1;
        end if;
    end loop;

    New_Line;
    Put_Line("Number of good Passwords detected =" & Natural'Image(Count_Good_Passwords));

    close(Database);

    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        put_line("usage: "& Command_Name & " Password_File_Name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        put_line(Standard_Error,"File '"&Argument(1)&"' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"No results!");
        set_Exit_Status(Failure);
        raise;
end Puzzle_02_A;

with Expenses; use Expenses;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.text_IO; use Ada.text_IO;


procedure Puzzle_02_B is

    my_Expenses : Ordered_Expenses.Set;
    index_A, index_B, index_C : Ordered_Expenses.Cursor;

    Special_expense : natural := 2020;

--  DAT_File_Name : String(1..2**15); -- 32_768
    Missing_FileName : exception;

    use Ordered_Expenses;
begin

    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    open_Password_database(File_Name => Argument(1); Database_File => Database);

   while not end_of_file(Database) loop
      get_line(File  => Database, Item => Number_Str, Last => Str_length);
      Int_IO.get(from => Number_Str(1..Str_length), Item => Cost, Last => Int_length);
   end loop;

    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        put_line("usage: "& Command_Name & " expenses_file_name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        put_line(Standard_Error,"file '"&Argument(1)&"' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"no results!");
        set_Exit_Status(Failure);
        raise;
end Puzzle_02_B;

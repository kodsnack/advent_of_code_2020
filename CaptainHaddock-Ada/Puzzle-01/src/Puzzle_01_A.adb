-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-01 
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------
with Expenses; use Expenses;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.text_IO; use Ada.text_IO;


procedure Puzzle_01_A is

    my_Expenses : Ordered_Expenses.Set;
    index_A, index_B : Ordered_Expenses.Cursor;

    Special_expense : natural := 2020;

--  DAT_File_Name : String(1..2**15); -- 32_768
    Missing_FileName : exception;

    use Ordered_Expenses;

begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    my_Expenses := get_expenses(Argument(1));

    index_A := First(my_Expenses);
    while Has_Element(index_A) loop
        index_B:= Next(index_A);
        while  Has_Element(index_B) loop
            -- put(Standard_Error,"."); -- breadcum ...
            if Element(index_A) + Element(index_B) = Special_expense then
                    -- Found Special_expense !
                    new_line;
                    put(Expense'Image(Special_expense) &" = ");
                    put(Expense'Image(Element(index_A)) &" + ");
                    put(Expense'Image(Element(index_B)));
                    new_line;
                    -- Print-out the magic Number !
                    put("Puzzle #1 Number = ");
                    put(Expense'Image(Element(index_A) * Element(index_B)));
                    new_line;
            end if;
            exit when Element(index_A)+Element(index_B) > Special_expense;
            Next(index_B);
        end loop;
        next(index_A);   
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
end Puzzle_01_A;

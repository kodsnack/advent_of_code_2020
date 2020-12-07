-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-01 
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;

package body Expenses is

package Int_IO is new Integer_IO(Expense);

function get_expenses(File_Name : String) return Ordered_Expenses.Set is
   
   subtype Str_length_t is Natural range  0..12;

   Str_length, Int_Length : Str_length_t;
   Number_Str : String(1..Str_length_t'Last) := (others => ' ');
   Expences : Ordered_Expenses.Set;
   Cost : expense;
   DAT_file : File_Type;

   use Ordered_Expenses;
   
begin
   open(File => DAT_file,
      Mode => In_File,
      Name => File_Name);
   
   while not end_of_file(DAT_file) loop
      get_line(File  => DAT_file, Item => Number_Str, Last => Str_length);
      Int_IO.get(from => Number_Str(1..Str_length), Item => Cost, Last => Int_length);
      Insert(Container => Expences, New_Item => Cost);
   end loop;
   
   return Expences;

exception
      
   when others =>
      close (DAT_file);
      raise;
end get_expenses;

end Expenses;

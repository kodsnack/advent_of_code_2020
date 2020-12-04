-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-01 
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;

package Expenses is

   subtype Expense is Integer;
   
   package Ordered_Expenses  is new Ada.Containers.Ordered_Sets (
         Element_Type => Expense);
         -- "<" (Left, Right : Expense) return Boolean,
         -- "<" (Left, Right : Expense) return Boolean);

   function get_expenses(File_Name : String) return Ordered_Expenses.Set;

end Expenses;

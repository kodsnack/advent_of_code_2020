-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Assertions;
use Ada.Assertions;

package body Forest is

   function Value( c : Character) return Trees;

   -- ======================================================
   function get_Map(File_Name : String) return Map is
   
      Map_File             : FILE_Type;
      South_Border_index   : Natural := 0;    
      East_Border_index    : WE_Dimension := 1;

      WE_Line              : String(1..East_Border+2); -- + accounting for CR LF

      Some_Map             : Map;


   begin
      open(File => Map_File,
         Mode => In_File,
         Name => File_Name);

      while not End_of_File(Map_File) and South_Border_index < South_Border loop
         South_Border_index := South_Border_index + 1;
         East_Border_index := 1;

         Get_Line(File => Map_File, Item => WE_Line, Last => East_Border_index );

         assert(East_Border_index <= East_border, "Line in file is greater than holding Map capacity !");
         -- put(Standard_Error,'.'); -- Trace bredcum

         for WE_Row_index in 1 .. East_Border_index loop -- WE_Dimension'Range loop
            Some_Map(WE_Row_index, South_Border_index) := Value(WE_Line(WE_Row_index));
         end loop;

      end loop;

      close(Map_File);
      return Some_Map;
      
   end get_Map;

   -- ======================================================
   function Value( c : Character) return Trees is
   -- GNAT libraries has a function for this ...
      Tree_value_conversion_not_defined : Exception;
   begin
      case c is
         when '#' => return Tree;
         when '.' => return None;
         when others => Raise Tree_value_conversion_not_defined;
      end case;
   end Value;


end Forest;

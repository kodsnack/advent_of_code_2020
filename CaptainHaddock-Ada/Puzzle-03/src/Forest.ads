-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------

with Ada.Strings.Bounded;
use Ada.Strings.Bounded;

with Ada.Text_IO;
use Ada.Text_IO;


package Forest is

   subtype Dimension is Positive;

   NW_Origin : constant Dimension := 1;
   East_border : constant Dimension := 31;
   South_border : constant Dimension := 323;

   subtype WE_Dimension is Positive range NW_Origin .. East_border;
   type WE_infinity is mod East_border -1; -- range 0 .. East_border - 1
   subtype NS_Dimension is Positive range NW_Origin .. South_border;

   type Trees is (Tree, None);
   for Trees use (
         Tree =>  Character'Pos('#'),
         None => Character'Pos('.'));
   
   type Map is array (WE_Dimension, NS_Dimension) of Trees; 
   
   function get_Map(File_Name : String) return Map;

end Forest;

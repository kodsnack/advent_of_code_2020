with Ada.Strings.Bounded;
use Ada.Strings.Bounded;

with Ada.Text_IO;
use Ada.Text_IO;


package Forest is

   type Dimension is Positive;
   Origin : Dimension := 1;

   type Coordinate is record 
      X, Y : Dimension := Origin; -- initial location upper-left
   end record;

   type Trees is (None, Tree);
   
   type Forest is array (X, Y : Dimension) of Trees; 
   
   Bottom_border : Positive;

   function get_Map(File_Name : String) return Forest;

end Forest;

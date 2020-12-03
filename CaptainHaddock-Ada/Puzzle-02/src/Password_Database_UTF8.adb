with Ada.Assertions; use Ada.Assertions;

with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO;
use Ada.Wide_Wide_Text_IO;

with Ada.Strings.Wide_Wide_Maps;
use Ada.Strings.Wide_Wide_Maps;

with Ada.Strings.Wide_Wide_Fixed;
use Ada.Strings.Wide_Wide_Fixed;

with Ada.Strings.UTF_Encoding; 
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings.UTF_Encoding;

package body Password_Database_UTF8 is


   package Database_Record_IO is new Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO(Database_Record);
   package Password_IO is new Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO(Password_String);

   -- ======================================================
   function get_Next_Password(Database_File : Ada.Wide_Wide_Text_IO.File_Type) return Password_Item is


   Database_String : Database_Record.Bounded_Wide_Wide_String;
   Some_Item : Password_Item;
   Last_char : Positive;
   Delimiter_Position : Positive;


   begin
      -- Remember : Max_occurrence is stored in the database as a Negative value
--      Assert( Password.Policy.Min_occurrence  <= abs(Password.Policy.Max_occurrence), "Error in the range of min/max occurrences !");

      -- get record with varying length
      Database_String := Database_Record_IO.Get_Line(Database_File);
      New_Line;
      Database_Record_IO.put_Line(Database_String);
      -- Transform String into an 'Password' object
      
      -- get Min_occurrence
      Min_occurrence_IO.get(  From => Database_Record.To_Wide_Wide_String(Database_String), 
            Item => Some_Item.Policy.Min_occurrence, 
            Last => Last_char);

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.Length(Database_String) - Last_char);

      Database_Record_IO.put_Line(Database_String);

      -- get Max_occurrence
--      Max_occurrence_IO.get(  From => Database_Record.To_Wide_Wide_String(Database_String)(Last_Char+1 .. Database_Record.Length(Database_String)), 
      Max_occurrence_IO.get(  From => Database_Record.To_Wide_Wide_String(Database_String), 
            Item => Some_Item.Policy.Max_occurrence, 
            Last => Last_char);
      Some_Item.Policy.Max_occurrence := abs(Some_Item.Policy.Max_occurrence);

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.length(Database_String) - Last_char );

      -- get Delimiter_1
      Delimiter_Position := 1; 
      -- assert( Element(Source => Database_String, Index => 1) = Some_Item.Policy.Delimiter_1), "Delimiter should be ' ' ");
      if  Database_Record.Element(Source => Database_String, Index => Delimiter_Position) /= Some_Item.Policy.Delimiter_1 then
         New_Line;
         Put_Line(Database_Record.To_Wide_Wide_String(Database_String));
         Put(Database_Record.Element(Source => Database_String, Index => Delimiter_Position));
         Put_Line(Standard_Error, "Delimiter should be '"& Some_Item.Policy.Delimiter_1 &"' ");
      end if;

      -- get Letter_must_have
      Delimiter_Position := 2; 
      Some_Item.Policy.Letter_must_have := Database_Record.Element(Source => Database_String, Index => Delimiter_Position);
      put(Database_Record.Element(Source => Database_String, Index => Delimiter_Position));


  --    assert( Element(Source => Database_String, Index => 4) = Some_Policy.Delimiter_2(1)), "Delimiter should be ':' !";
  --    assert( Element(Source => Database_String, Index => 3) = Some_Policy.Delimiter_2(2)), "Delimiter should be ' ' !";

      -- get Delimiter
      Delimiter_Position := 3;
      if  Database_Record.Element(Source => Database_String, Index => Delimiter_Position) /= Some_Item.Policy.Delimiter_2(1) then 
         New_Line;
         Put_Line(Database_Record.To_Wide_Wide_String(Database_String));
         Put(Database_Record.Element(Source => Database_String, Index => Delimiter_Position));
         Put_Line(Standard_Error, "-" & Database_Record.Element(Source => Database_String, Index => Delimiter_Position) & "- Delimiter should be '" & Some_Item.Policy.Delimiter_2(1) & "' ");
      end if;

      -- get Delimiter
      Delimiter_Position := 4;
      if  Database_Record.Element(Source => Database_String, Index => Delimiter_Position) /= Some_Item.Policy.Delimiter_2(2) then 
         New_Line;
         Put_Line(Database_Record.To_Wide_Wide_String(Database_String));
         Put(Database_Record.Element(Source => Database_String, Index => Delimiter_Position));
         Put_Line(Standard_Error, "-" & Database_Record.Element(Source => Database_String, Index => Delimiter_Position) & "- Delimiter should be '" & Some_Item.Policy.Delimiter_2(2) & "' ");
      end if;

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.length(Database_String) - Delimiter_Position );
      -- put_line("Password length = "& Natural'Wide_Wide_Image(Database_Record.length(Database_String)));
      -- get Password, coping with varying string length

      some_Item.Password :=  Password_String.To_Bounded_Wide_Wide_String( 
                                    Database_Record.To_Wide_Wide_String(Database_String));
                                   
      return Some_Item;
   end get_Next_Password;


   -- ======================================================
   function is_OK(Password : Password_Item) return Boolean is

      Occurrences : natural := 0;

   begin
      Occurrences := Ada.Strings.Wide_Wide_Fixed.Count(
            Source => to_Wide_Wide_String(Password.Password), 
            Set => to_Set(Password.Policy.Letter_must_have));
      New_Line;
      Min_occurrence_IO.put(Password.Policy.Min_occurrence);
      Max_occurrence_IO.put( - Password.Policy.Max_occurrence);
      put(Password.Policy.Delimiter_1);
      put(Password.Policy.Letter_must_have);
      put(Password.Policy.Delimiter_2);
      Password_IO.put(Password.Password);
      New_Line;

      -- Remember : Max_occurrence is stored in the database as a Negative value
--      if Occurrences in Password.Policy.Min_occurrence  .. abs(Password.Policy.Max_occurrence) then 
      if Password.Policy.Min_occurrence <= Occurrences  and Occurrences <= abs(Password.Policy.Max_occurrence) then 
         return True;
      else
         return False;
      end if;

   end is_OK;


end Password_Database_UTF8;

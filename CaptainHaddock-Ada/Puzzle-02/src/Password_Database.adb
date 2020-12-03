with Ada.Assertions; use Ada.Assertions;

-- with Ada.Text_IO;

package body Password_Database is


   -- ======================================================
   function get_Next_Password(Database_File : File_Type) return Password_Item is


   Database_String : Database_Record.Bounded_String;
   Some_Policy : Password_Policy;
   Some_Item : Password_Item;
   Last_char : Positive;


   begin
      -- Remember : Max_occurrence is stored in the database as a Negative value
--      Assert( Password.Policy.Min_occurrence  <= abs(Password.Policy.Max_occurrence), "Error in the range of min/max occurrences !");

      -- get record with varying length
      Database_String := Database_Record_IO.Get_Line(Database_File);

      -- Transform String into an 'Password' object
      Min_occurrence_IO.get(  From => Database_Record.To_String(Database_String), 
            Item => Some_Policy.Min_occurrence, 
            Last => Last_char);

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.Length(Database_String) - Last_char);


      Max_occurrence_IO.get(  From => Database_Record.To_String(Database_String)(Last_Char+1 .. Database_Record.Length(Database_String)), 
            Item => Some_Policy.Max_occurrence, 
            Last => Last_char);
      Some_Policy.Max_occurrence := abs(Some_Policy.Max_occurrence);

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.length(Database_String) - Last_char);

     -- assert( Element(Source => Database_String, Index => 1) = Some_Policy.Delimiter_1), "Delimiter should be ' ' ");
      Some_Policy.Letter_must_have := Database_Record.Element(Source => Database_String, Index => 2);
  --    assert( Element(Source => Database_String, Index => 3) = Some_Policy.Delimiter_2(1)), "Delimiter should be ' ' !";
  --    assert( Element(Source => Database_String, Index => 4) = Some_Policy.Delimiter_2(2)), "Delimiter should be ':' !";
   
      -- Get password, coping with varying string length
      
      some_Item.Password :=  String_Password.To_Bounded_String( 
                                    Database_Record.To_String(
                                          Database_Record.Tail(  Source => Database_String,
                                                                 Count => Database_Record.length(Database_String) - 4)
                                    ));
                                   
      Some_Item.Policy := Some_Policy;
      return Some_Item;
   end get_Next_Password;


   -- ======================================================
   function is_OK(Password : Password_Item) return Boolean is

      Occurrences : natural := 0;

      function Count_Occurrences(Letter : Character; in_String : String) return Natural is
         Count : natural := 0;
      begin

         for Index in in_String'Range loop
            if in_String(Index) = Letter then
               Count := Count +1;
            end if;
         end loop;

         return Count;
      end Count_Occurrences;

   begin
      Occurrences := Count_Occurrences(Letter => Password.Policy.Letter_must_have, 
                                       in_String => to_String(Password.Password));

      -- Remember : Max_occurrence is stored in the database as a Negative value
      if Occurrences in Password.Policy.Min_occurrence  .. abs(Password.Policy.Max_occurrence) then 
         return True;
      else
         return False;
      end if;

   end is_OK;


end Password_Database;

-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-02 
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------
with Ada.Assertions; use Ada.Assertions;

with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO;
use Ada.Wide_Wide_Text_IO;

with Ada.Strings.Wide_Wide_Maps;
use Ada.Strings.Wide_Wide_Maps;

with Ada.Characters.Conversions;
use Ada.Characters.Conversions;

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
      -- get record with varying length
      Database_String := Database_Record_IO.Get_Line(Database_File);


      -- Transform String into an 'Password' object
      
      -- get Min_occurrence
      Min_occurrence_IO.get(  From => Database_Record.To_Wide_Wide_String(Database_String), 
            Item => Some_Item.Policy.Min_occurrence, 
            Last => Last_char);

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.Length(Database_String) - Last_char);


      -- get Max_occurrence
--      Max_occurrence_IO.get(  From => Database_Record.To_Wide_Wide_String(Database_String)(Last_Char+1 .. Database_Record.Length(Database_String)), 
      Max_occurrence_IO.get(  From => Database_Record.To_Wide_Wide_String(Database_String), 
            Item => Some_Item.Policy.Max_occurrence, 
            Last => Last_char);

      -- Remember : Max_occurrence is seen in the database as a Negative value, so let's put it rigth
      Some_Item.Policy.Max_occurrence := abs(Some_Item.Policy.Max_occurrence);

      Assert( Some_Item.Policy.Min_occurrence  <= Some_Item.Policy.Max_occurrence, 
         "Error in the definition of the Min-Max range : encountered 'Max' < 'Min' !?!");


      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.length(Database_String) - Last_char );

      -- get Delimiter_1
      Delimiter_Position := 1; 
      assert( Database_Record.Element(Source => Database_String, Index => Delimiter_Position) = Some_Item.Policy.Delimiter_1, 
         "Delimiter_1 should be '" & to_Character(Some_Item.Policy.Delimiter_1) & "' ");
      
      -- get Letter_must_have
      Delimiter_Position := 2; 
      Some_Item.Policy.Letter_must_have := Database_Record.Element(Source => Database_String, Index => Delimiter_Position);
      
      -- get Delimiter
      Delimiter_Position := 3;
      assert( Database_Record.Element(Source => Database_String, Index => Delimiter_Position) = Some_Item.Policy.Delimiter_2(1), 
         "Delimiter_2.1 should be '" & to_Character(Some_Item.Policy.Delimiter_2(1)) & "' !");

      -- get Delimiter
      Delimiter_Position := 4;
      assert( Database_Record.Element(Source => Database_String, Index => Delimiter_Position) = Some_Item.Policy.Delimiter_2(2), 
         "Delimiter_2.2 should be '" & to_Character(Some_Item.Policy.Delimiter_2(2)) & "' !");

      Database_String := Database_Record.Tail( Source => Database_String,
                               Count => Database_Record.length(Database_String) - Delimiter_Position );

      -- get Password, coping with varying string length
      some_Item.Password :=  Password_String.To_Bounded_Wide_Wide_String( 
                                    Database_Record.To_Wide_Wide_String(Database_String));
                                   
      return Some_Item;
   end get_Next_Password;


   -- ======================================================
   function is_OK_Rule_1(Password : Password_Item) return Boolean is
   -- Password must contain at least 'Min_occurrence' of characters 'Letter_must_have' in the password
   -- and may have up to maximum 'Max_occurrence' of this 'Letter_must_have'.

      Occurrences : natural := 0;

   begin
      Occurrences := Ada.Strings.Wide_Wide_Fixed.Count(
            Source => to_Wide_Wide_String(Password.Password), 
            Set => to_Set(Password.Policy.Letter_must_have));

      if Occurrences in Password.Policy.Min_occurrence  .. Password.Policy.Max_occurrence then 
         return True;
      else
         return False;
      end if;

   end is_OK_Rule_1;

   -- ======================================================
   function is_OK_Rule_2(Password : Password_Item) return Boolean is
   -- Password must contain character 'Letter_must_have' in the password
   -- at either character postion defined by 'Min_occurrence' and 'Max_occurrence', but not both.
 
      Occurrences : natural := 0;
      Rule_Status : Boolean := False;
      a, b : Boolean := False; -- sub-rules

   begin
      Occurrences := Ada.Strings.Wide_Wide_Fixed.Count(
            Source => to_Wide_Wide_String(Password.Password), 
            Set => to_Set(Password.Policy.Letter_must_have));

      -- get status at position 'a' and 'b'
         a := Password_String.Element(Source => Password.Password, Index => Password.Policy.Min_occurrence) = Password.Policy.Letter_must_have;
         b := Password_String.Element(Source => Password.Password, Index => Password.Policy.Max_occurrence) = Password.Policy.Letter_must_have;

      -- verify statements
            Rule_Status := (a and not b) or (b and not a);

      return Rule_Status;

   end is_OK_Rule_2;

end Password_Database_UTF8;

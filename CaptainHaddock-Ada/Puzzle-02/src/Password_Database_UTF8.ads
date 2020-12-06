-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-02 
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------
with Ada.Strings.Wide_Wide_Bounded;
use Ada.Strings.Wide_Wide_Bounded;

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;


package Password_Database_UTF8 is

   Max_Password_Length : constant Positive := 1_000;

   package Password_String is new Generic_Bounded_Length(Max_Password_Length);

   use Password_String;
   subtype Password_Str is Bounded_Wide_Wide_String; -- rename of a type

   package Database_Record    is new Generic_Bounded_Length(Max_Password_Length);


   type Password_Policy is record
      Min_occurrence : Natural := 0; -- Minimum number of occcurrence of Letter_must_have
      Max_occurrence : Integer := Max_Password_Length; -- Maximum number of occcurrence allowed of Letter_must_have (value in file  will be considered as a Negative value, which will be converted Positive)
      Delimiter_1 : Wide_Wide_Character := ' ';
      Letter_must_have : Wide_Wide_Character := ' ';
      Delimiter_2 : Wide_Wide_String(1..2) := ": ";
   end record;
   
   type Password_Item is record -- to handle correctly the length of the password given in the file
      Policy : Password_Policy;
      Password : Password_Str := Null_Bounded_Wide_Wide_String; 
   end record;

   package Min_occurrence_IO is new Integer_IO(Natural); use Min_occurrence_IO;
   package Max_occurrence_IO is new Integer_IO(Integer); use Max_occurrence_IO;


   function get_Next_Password(Database_File : File_Type) return Password_Item;

   function is_OK_Rule_1(Password : Password_Item) return Boolean;
   function is_OK_Rule_2(Password : Password_Item) return Boolean;


end Password_Database_UTF8;
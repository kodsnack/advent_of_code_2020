with Ada.Strings.Wide_Wide_Bounded;
with Ada.Text_IO.Wide_Wide_Bounded_IO;
with Ada.Wide_Wide_Text_IO;

package Password_Database is

   Max_Password_Length : Positive := 1_000);

   package String_Password is new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length(Max_Password_Length);
   use String_Password;
   subtype Password_Str is Bounded_String; -- rename of a type

   package Database_Record is new Ada.Text_IO.Wide_Wide_Bounded_IO(String_Password);


   type Password_Policy is record
      Min_occurrence : Positive := 0; -- Minimum number of occcurrence of Letter_must_have
      Max_occurrence : Integer := Max_Password_Length; -- Maximum number of occcurrence allowed of Letter_must_have (value in file  will be considered as a Negative value, which will be converted Positive)
      Delimiter_1 : Character := ' ';
      Letter_must_have : Wide_Wide_Character := ' ';
      Delimiter_2 : String(1..2) := " :";
   end record;

--   Max_Record_Length : Positive := Password_Policy'Size/8 + Max_Password_Length);
   
   type Password_Item is record -- to handle correctly the length of the password given in the file
      Policy : Password_Policy;
      Password : Password_Str := Null_Bounded_String; 
   end record;


   procedure open_Password_database(File_Name : String; Database_File : out Ada.Text_IO.File_Type);
   
   function get_Next_Password(Database_File : Ada.Text_IO.File_Type) return Password_Item;

   function is_OK(Password : Password_Item) return Boolean;

end Password_Database;
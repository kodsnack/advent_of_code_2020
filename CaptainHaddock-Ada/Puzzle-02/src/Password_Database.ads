with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;


package Password_Database is

   Max_Password_Length : constant Positive := 1_000;

   package String_Password is new Ada.Strings.Bounded.Generic_Bounded_Length(Max_Password_Length);

   use String_Password;
   subtype Password_Str is Bounded_String; -- rename of a type

   package Database_Record    is new Ada.Strings.Bounded.Generic_Bounded_Length(Max_Password_Length);
   package Database_Record_IO is new Bounded_IO(Database_Record);


   type Password_Policy is record
      Min_occurrence : Natural := 0; -- Minimum number of occcurrence of Letter_must_have
      Max_occurrence : Integer := Max_Password_Length; -- Maximum number of occcurrence allowed of Letter_must_have (value in file  will be considered as a Negative value, which will be converted Positive)
      Delimiter_1 : Character := ' ';
      Letter_must_have : Character := ' ';
      Delimiter_2 : String(1..2) := " :";
   end record;
   
   type Password_Item is record -- to handle correctly the length of the password given in the file
      Policy : Password_Policy;
      Password : Password_Str := Null_Bounded_String; 
   end record;

   package Min_occurrence_IO is new Integer_IO(Natural); use Min_occurrence_IO;
   package Max_occurrence_IO is new Integer_IO(Integer); use Max_occurrence_IO;


   function get_Next_Password(Database_File : File_Type) return Password_Item;

   function is_OK(Password : Password_Item) return Boolean;

end Password_Database;
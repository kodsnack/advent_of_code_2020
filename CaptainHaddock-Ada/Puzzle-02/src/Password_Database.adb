with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Storage_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Wide_Wide_Text_IO.Text_Streams;
Ada.Streams.Stream_IO
type Stream_Access is access all Root_Stream_Type'Class;


package body Password_Database is


   -- ======================================================
   function get_Next_Password(Database_File : File_Type) return Password_Item is

   package Policy_IO is new Storage_IO(Password_Policy);

   Database_String : Database_Record.Bounded_String;
   Password_Buffer : Policy_IO.Buffer_Type;
   some_Policy : Password_Policy;
   some_Item : Password_Item;

   begin
      -- Remember : Max_occurrence is stored in the database as a Negative value
      Assert( Password.Policy.Min_occurrence  <= abs(Password.Policy.Max_occurrence), "Error in the range of min/max occurrences !");

      -- get record with varying length
      Database_String := Database_Record.Get_Line(Database);

      -- Transform String into an 'Password' object
      
      
      Policy_IO.read(Buffer => Password_Buffer, Item => some_Policy );

      -- Get password, coping with varying string length
      some_Item.Password := To_Bounded_String(Bounded_Slice(Database_String, some_Policy'Length, length(Database_String) - some_Policy'Length ) );

      return some_Item;
   exception  
      when End_of_File_reached =>
         close(File => Database);

   end get_Next_Password;


   -- ======================================================
   function is_OK(Password : Password_Item) return Boolean is

      Occurrences : natural := 0;

      function Count_Occurrences(Letter : Wide_Wide_Character; in_String : Wide_Wide_String) return Natural is
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
      Occurrences := Count_Occurrences(Letter => Letter_must_have, in_String => Password.Password);
      -- Remember : Max_occurrence is stored in the database as a Negative value
      if Occurrences in Password.Policy.Min_occurrence  .. abs(Password.Policy.Max_occurrence) then 
         return True;
      else
         return False;
      end if;

   end is_OK;


end Password_Database;

-- ------------------------------------------------
-- Author : William J. FRANCK
-- e-Mail : william@sterna.io
--
-- Initial creation date : 2020-12-03
-- ------------------------------------------------
-- License : CC-BY-SA 
-- ------------------------------------------------

With Forest;
Use Forest;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;


procedure Puzzle_03 is

    myMap : Forest.Map;
--    NS_Line_index, WE_Row_index : Forest.Dimension ;
--  DAT_File_Name : String(1..2**15); -- 32_768
    Database : File_Type;
    Missing_FileName : exception;


    type Step_increment is record 
        to_east  : Natural range 0 .. Forest.East_border;
        to_South : Natural range 0 .. Forest.South_border;
    end record;

    type Test_Slope is record
        Step : Step_increment;
        Nb_trees : Natural := 0;
    end record;
       
    type Test_Slopes is array (1.. 5) of Test_Slope;

    Test_Cases : Test_Slopes := (
     (Step => (to_east => 1, to_South => 1), Nb_trees => 0),
     (Step => (to_east => 3, to_South => 1), Nb_trees => 0),
     (Step => (to_east => 5, to_South => 1), Nb_trees => 0),
     (Step => (to_east => 7, to_South => 1), Nb_trees => 0),
     (Step => (to_east => 1, to_South => 2), Nb_trees => 0));


    Magic_Number : Long_Integer := 1;
    
    -- ==============================================================
    function get_Nb_Trees(This_map: Map ; for_slope : Step_increment) return Natural is

        East_Location  : WE_Dimension := NW_Origin;
        South_Location : NS_Dimension := NW_Origin;

        Nb_trees : natural := 0;
    
    begin
        -- Start at origin, has the origin location a Tree ?
        if This_map (NW_Origin, NW_Origin) = TREE then
            Nb_trees := 1;
        end if;

        -- go through the forest until you meet the South border, while counting trees on your traversal
        while South_Location + for_slope.to_South <= Forest.South_border loop
               -- East_Location  := WE_Dimension(WE_infinity'Mod(East_Location + for_slope.to_East -1) +1); -- /!\ modulus starts at 0.

                if East_Location + for_slope.to_East >  East_border then
                   East_Location := East_Location + for_slope.to_East - East_border;
                else
                   East_Location := East_Location + for_slope.to_East;
                end if;
                South_Location := South_Location + for_slope.to_South;
                --put("X="&Integer'Image(East_Location) & "Y=" & Integer'Image(South_Location)&"/");
                if This_map (East_Location, South_Location) = Forest.Tree then
                    Nb_Trees := Nb_Trees +1;
                end if;
        end loop;
        return Nb_Trees;
    end get_Nb_Trees;

-- ----------------------    
begin
    -- get the filename
    if Argument_Count /= 1 then
        raise Missing_FileName;
    end if;

    -- get the Tree-map
    myMap := Forest.get_Map(Argument(1));

    new_Line;
    for i in Test_Slopes'Range loop

        Test_Cases(i).Nb_Trees := get_Nb_Trees(This_map => myMap, for_slope => Test_Cases(i).Step );
        put_line("On slope"&Integer'Image(i)&", I'v encountered "& Test_Cases(i).Nb_trees'Image & " Trees." );
        Magic_Number := Magic_number * Long_Integer(Test_Cases(i).Nb_Trees);
    end loop;

    put_line("Magic number ="&Long_Integer'Image(Magic_Number) );
    set_Exit_Status(Success);

exception
    When Missing_FileName =>
        put_line("usage: "& Command_Name & " Tree-Map_file_name");
        set_Exit_Status(Failure);
    
    when Status_Error =>
        put_line(Standard_Error,"file '"&Argument(1)&"' not found!");
        set_Exit_Status(Failure);
        raise;
  
    when others => 
        put_line(Standard_Error,"Error when Reading the file !");
        set_Exit_Status(Failure);
        raise;
end Puzzle_03;

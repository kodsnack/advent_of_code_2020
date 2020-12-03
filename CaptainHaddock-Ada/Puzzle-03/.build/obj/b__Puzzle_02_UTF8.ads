pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Community 2020 (20200818-84)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_puzzle_02_utf8" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#fd252ea9#;
   pragma Export (C, u00001, "puzzle_02_utf8B");
   u00002 : constant Version_32 := 16#67c8d842#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#6dba5b66#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#01a73f89#;
   pragma Export (C, u00005, "ada__command_lineB");
   u00006 : constant Version_32 := 16#3cdef8c9#;
   pragma Export (C, u00006, "ada__command_lineS");
   u00007 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00007, "systemS");
   u00008 : constant Version_32 := 16#632dfee6#;
   pragma Export (C, u00008, "system__secondary_stackB");
   u00009 : constant Version_32 := 16#f9f0587f#;
   pragma Export (C, u00009, "system__secondary_stackS");
   u00010 : constant Version_32 := 16#ba5b22d1#;
   pragma Export (C, u00010, "ada__exceptionsB");
   u00011 : constant Version_32 := 16#f540b206#;
   pragma Export (C, u00011, "ada__exceptionsS");
   u00012 : constant Version_32 := 16#35e1815f#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerB");
   u00013 : constant Version_32 := 16#cfec26ee#;
   pragma Export (C, u00013, "ada__exceptions__last_chance_handlerS");
   u00014 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#776ed810#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#ce3e0e21#;
   pragma Export (C, u00016, "system__soft_links__initializeB");
   u00017 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00017, "system__soft_links__initializeS");
   u00018 : constant Version_32 := 16#896564a3#;
   pragma Export (C, u00018, "system__parametersB");
   u00019 : constant Version_32 := 16#4f09ab30#;
   pragma Export (C, u00019, "system__parametersS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00022, "system__storage_elementsB");
   u00023 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00023, "system__storage_elementsS");
   u00024 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#37322c0b#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00028, "system__exceptions__machineB");
   u00029 : constant Version_32 := 16#5c74e542#;
   pragma Export (C, u00029, "system__exceptions__machineS");
   u00030 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#1416bc8d#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00036, "system__traceback_entriesB");
   u00037 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00037, "system__traceback_entriesS");
   u00038 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00038, "system__traceback__symbolicB");
   u00039 : constant Version_32 := 16#46491211#;
   pragma Export (C, u00039, "system__traceback__symbolicS");
   u00040 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00040, "ada__exceptions__tracebackB");
   u00041 : constant Version_32 := 16#ae2d2db5#;
   pragma Export (C, u00041, "ada__exceptions__tracebackS");
   u00042 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00042, "system__address_imageB");
   u00043 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00043, "system__address_imageS");
   u00044 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00044, "system__wch_conB");
   u00045 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00045, "system__wch_conS");
   u00046 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00046, "system__wch_stwB");
   u00047 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00047, "system__wch_stwS");
   u00048 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00048, "system__wch_cnvB");
   u00049 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00049, "system__wch_cnvS");
   u00050 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00050, "interfacesS");
   u00051 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00051, "system__wch_jisB");
   u00052 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00052, "system__wch_jisS");
   u00053 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00053, "ada__stringsS");
   u00054 : constant Version_32 := 16#cd3494c7#;
   pragma Export (C, u00054, "ada__strings__utf_encodingB");
   u00055 : constant Version_32 := 16#80baeb4a#;
   pragma Export (C, u00055, "ada__strings__utf_encodingS");
   u00056 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00056, "ada__strings__utf_encoding__stringsB");
   u00057 : constant Version_32 := 16#fe1d64b5#;
   pragma Export (C, u00057, "ada__strings__utf_encoding__stringsS");
   u00058 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00058, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00059 : constant Version_32 := 16#91eda35b#;
   pragma Export (C, u00059, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00060 : constant Version_32 := 16#f9576a72#;
   pragma Export (C, u00060, "ada__tagsB");
   u00061 : constant Version_32 := 16#b6661f55#;
   pragma Export (C, u00061, "ada__tagsS");
   u00062 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00062, "system__htableB");
   u00063 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00063, "system__htableS");
   u00064 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00064, "system__string_hashB");
   u00065 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00065, "system__string_hashS");
   u00066 : constant Version_32 := 16#56941de9#;
   pragma Export (C, u00066, "system__unsigned_typesS");
   u00067 : constant Version_32 := 16#d2ae2792#;
   pragma Export (C, u00067, "system__val_lluB");
   u00068 : constant Version_32 := 16#3b5a900b#;
   pragma Export (C, u00068, "system__val_lluS");
   u00069 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00069, "system__val_utilB");
   u00070 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00070, "system__val_utilS");
   u00071 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00071, "system__case_utilB");
   u00072 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00072, "system__case_utilS");
   u00073 : constant Version_32 := 16#fac29b11#;
   pragma Export (C, u00073, "ada__wide_wide_text_ioB");
   u00074 : constant Version_32 := 16#bc504bbc#;
   pragma Export (C, u00074, "ada__wide_wide_text_ioS");
   u00075 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00075, "ada__streamsB");
   u00076 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00076, "ada__streamsS");
   u00077 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00077, "ada__io_exceptionsS");
   u00078 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00078, "interfaces__c_streamsB");
   u00079 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00079, "interfaces__c_streamsS");
   u00080 : constant Version_32 := 16#41b27041#;
   pragma Export (C, u00080, "system__crtlS");
   u00081 : constant Version_32 := 16#ec9c64c3#;
   pragma Export (C, u00081, "system__file_ioB");
   u00082 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00082, "system__file_ioS");
   u00083 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00083, "ada__finalizationS");
   u00084 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00084, "system__finalization_rootB");
   u00085 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00085, "system__finalization_rootS");
   u00086 : constant Version_32 := 16#5f851299#;
   pragma Export (C, u00086, "system__os_libB");
   u00087 : constant Version_32 := 16#d872da39#;
   pragma Export (C, u00087, "system__os_libS");
   u00088 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00088, "system__stringsB");
   u00089 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00089, "system__stringsS");
   u00090 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00090, "system__file_control_blockS");
   u00091 : constant Version_32 := 16#b56e43ba#;
   pragma Export (C, u00091, "password_database_utf8B");
   u00092 : constant Version_32 := 16#aa2c1b52#;
   pragma Export (C, u00092, "password_database_utf8S");
   u00093 : constant Version_32 := 16#9094876d#;
   pragma Export (C, u00093, "ada__assertionsB");
   u00094 : constant Version_32 := 16#1a0b0d2c#;
   pragma Export (C, u00094, "ada__assertionsS");
   u00095 : constant Version_32 := 16#dcf8e2cf#;
   pragma Export (C, u00095, "system__assertionsB");
   u00096 : constant Version_32 := 16#c5d6436f#;
   pragma Export (C, u00096, "system__assertionsS");
   u00097 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00097, "ada__charactersS");
   u00098 : constant Version_32 := 16#e753e265#;
   pragma Export (C, u00098, "ada__characters__conversionsB");
   u00099 : constant Version_32 := 16#761d31b0#;
   pragma Export (C, u00099, "ada__characters__conversionsS");
   u00100 : constant Version_32 := 16#ff8f7125#;
   pragma Export (C, u00100, "ada__strings__wide_wide_boundedB");
   u00101 : constant Version_32 := 16#9a0f2f2b#;
   pragma Export (C, u00101, "ada__strings__wide_wide_boundedS");
   u00102 : constant Version_32 := 16#f4eea38a#;
   pragma Export (C, u00102, "ada__strings__wide_wide_mapsB");
   u00103 : constant Version_32 := 16#cf20fccc#;
   pragma Export (C, u00103, "ada__strings__wide_wide_mapsS");
   u00104 : constant Version_32 := 16#d5d8c501#;
   pragma Export (C, u00104, "system__storage_pools__subpoolsB");
   u00105 : constant Version_32 := 16#e136d7bf#;
   pragma Export (C, u00105, "system__storage_pools__subpoolsS");
   u00106 : constant Version_32 := 16#57674f80#;
   pragma Export (C, u00106, "system__finalization_mastersB");
   u00107 : constant Version_32 := 16#0b3c2f2b#;
   pragma Export (C, u00107, "system__finalization_mastersS");
   u00108 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00108, "system__img_boolB");
   u00109 : constant Version_32 := 16#fd821e10#;
   pragma Export (C, u00109, "system__img_boolS");
   u00110 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00110, "system__ioB");
   u00111 : constant Version_32 := 16#961998b4#;
   pragma Export (C, u00111, "system__ioS");
   u00112 : constant Version_32 := 16#35d6ef80#;
   pragma Export (C, u00112, "system__storage_poolsB");
   u00113 : constant Version_32 := 16#732d884c#;
   pragma Export (C, u00113, "system__storage_poolsS");
   u00114 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00114, "system__storage_pools__subpools__finalizationB");
   u00115 : constant Version_32 := 16#8bd8fdc9#;
   pragma Export (C, u00115, "system__storage_pools__subpools__finalizationS");
   u00116 : constant Version_32 := 16#5252521d#;
   pragma Export (C, u00116, "system__stream_attributesB");
   u00117 : constant Version_32 := 16#d573b948#;
   pragma Export (C, u00117, "system__stream_attributesS");
   u00118 : constant Version_32 := 16#3e25f63c#;
   pragma Export (C, u00118, "system__stream_attributes__xdrB");
   u00119 : constant Version_32 := 16#2f60cd1f#;
   pragma Export (C, u00119, "system__stream_attributes__xdrS");
   u00120 : constant Version_32 := 16#502e73ef#;
   pragma Export (C, u00120, "system__fat_fltS");
   u00121 : constant Version_32 := 16#761c7ae2#;
   pragma Export (C, u00121, "system__fat_lfltS");
   u00122 : constant Version_32 := 16#0cccd408#;
   pragma Export (C, u00122, "system__fat_llfS");
   u00123 : constant Version_32 := 16#a368b3ae#;
   pragma Export (C, u00123, "system__fat_sfltS");
   u00124 : constant Version_32 := 16#ec6e32dd#;
   pragma Export (C, u00124, "ada__strings__wide_wide_superboundedB");
   u00125 : constant Version_32 := 16#3109c9fc#;
   pragma Export (C, u00125, "ada__strings__wide_wide_superboundedS");
   u00126 : constant Version_32 := 16#99e40162#;
   pragma Export (C, u00126, "ada__strings__wide_wide_searchB");
   u00127 : constant Version_32 := 16#ff3339af#;
   pragma Export (C, u00127, "ada__strings__wide_wide_searchS");
   u00128 : constant Version_32 := 16#8eac1373#;
   pragma Export (C, u00128, "system__compare_array_unsigned_32B");
   u00129 : constant Version_32 := 16#c666157c#;
   pragma Export (C, u00129, "system__compare_array_unsigned_32S");
   u00130 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00130, "system__address_operationsB");
   u00131 : constant Version_32 := 16#1b57d1c8#;
   pragma Export (C, u00131, "system__address_operationsS");
   u00132 : constant Version_32 := 16#ace85504#;
   pragma Export (C, u00132, "ada__strings__wide_wide_fixedB");
   u00133 : constant Version_32 := 16#bd30f6e5#;
   pragma Export (C, u00133, "ada__strings__wide_wide_fixedS");
   u00134 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00134, "system__concat_3B");
   u00135 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00135, "system__concat_3S");
   u00136 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00136, "system__concat_2B");
   u00137 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00137, "system__concat_2S");
   u00138 : constant Version_32 := 16#69a65218#;
   pragma Export (C, u00138, "ada__wide_wide_text_io__integer_auxB");
   u00139 : constant Version_32 := 16#c82731e0#;
   pragma Export (C, u00139, "ada__wide_wide_text_io__integer_auxS");
   u00140 : constant Version_32 := 16#55290f9c#;
   pragma Export (C, u00140, "ada__wide_wide_text_io__generic_auxB");
   u00141 : constant Version_32 := 16#80f4b573#;
   pragma Export (C, u00141, "ada__wide_wide_text_io__generic_auxS");
   u00142 : constant Version_32 := 16#db42ae56#;
   pragma Export (C, u00142, "system__img_biuB");
   u00143 : constant Version_32 := 16#90b695a4#;
   pragma Export (C, u00143, "system__img_biuS");
   u00144 : constant Version_32 := 16#244fa59d#;
   pragma Export (C, u00144, "system__img_llbB");
   u00145 : constant Version_32 := 16#d171855a#;
   pragma Export (C, u00145, "system__img_llbS");
   u00146 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00146, "system__img_lliB");
   u00147 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00147, "system__img_lliS");
   u00148 : constant Version_32 := 16#cd1fde06#;
   pragma Export (C, u00148, "system__img_llwB");
   u00149 : constant Version_32 := 16#781da6cc#;
   pragma Export (C, u00149, "system__img_llwS");
   u00150 : constant Version_32 := 16#811cd12a#;
   pragma Export (C, u00150, "system__img_wiuB");
   u00151 : constant Version_32 := 16#fef71236#;
   pragma Export (C, u00151, "system__img_wiuS");
   u00152 : constant Version_32 := 16#65de8d35#;
   pragma Export (C, u00152, "system__val_intB");
   u00153 : constant Version_32 := 16#bda40698#;
   pragma Export (C, u00153, "system__val_intS");
   u00154 : constant Version_32 := 16#5276dcb7#;
   pragma Export (C, u00154, "system__val_unsB");
   u00155 : constant Version_32 := 16#63926050#;
   pragma Export (C, u00155, "system__val_unsS");
   u00156 : constant Version_32 := 16#914b0305#;
   pragma Export (C, u00156, "system__val_lliB");
   u00157 : constant Version_32 := 16#6435fd0b#;
   pragma Export (C, u00157, "system__val_lliS");
   u00158 : constant Version_32 := 16#1593dca4#;
   pragma Export (C, u00158, "system__wch_wtsB");
   u00159 : constant Version_32 := 16#a04f629b#;
   pragma Export (C, u00159, "system__wch_wtsS");
   u00160 : constant Version_32 := 16#eca5ecae#;
   pragma Export (C, u00160, "system__memoryB");
   u00161 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00161, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_32%s
   --  system.compare_array_unsigned_32%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.memory%s
   --  system.memory%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.characters.conversions%s
   --  ada.characters.conversions%b
   --  ada.command_line%s
   --  ada.command_line%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  system.fat_sflt%s
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes.xdr%s
   --  system.stream_attributes.xdr%b
   --  system.stream_attributes%b
   --  ada.strings.wide_wide_maps%s
   --  ada.strings.wide_wide_maps%b
   --  ada.strings.wide_wide_search%s
   --  ada.strings.wide_wide_search%b
   --  ada.strings.wide_wide_fixed%s
   --  ada.strings.wide_wide_fixed%b
   --  ada.strings.wide_wide_superbounded%s
   --  ada.strings.wide_wide_superbounded%b
   --  ada.strings.wide_wide_bounded%s
   --  ada.strings.wide_wide_bounded%b
   --  system.val_lli%s
   --  system.val_lli%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  system.val_int%s
   --  system.val_int%b
   --  system.wch_wts%s
   --  system.wch_wts%b
   --  ada.wide_wide_text_io%s
   --  ada.wide_wide_text_io%b
   --  ada.wide_wide_text_io.generic_aux%s
   --  ada.wide_wide_text_io.generic_aux%b
   --  ada.wide_wide_text_io.integer_aux%s
   --  ada.wide_wide_text_io.integer_aux%b
   --  system.assertions%s
   --  system.assertions%b
   --  ada.assertions%s
   --  ada.assertions%b
   --  password_database_utf8%s
   --  password_database_utf8%b
   --  puzzle_02_utf8%b
   --  END ELABORATION ORDER

end ada_main;

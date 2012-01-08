with Input_Sources.File;
with Tree_Readers;
with Sax.Readers;

with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Ada.Exceptions; use  Ada.Exceptions;
with Ada.Text_IO;

with Giant.XML_File_Access;

procedure Testxml_Val is



   ------
   The_Tree_Reader  : Tree_Readers.Tree_Reader;
   The_XML_Document : Dom.Core.Document;

   -- A_Node : DOM.Core.Node;
   Nodes_List : DOM.Core.Node_List;

begin

   -- Load xml tree
   Giant.XML_File_Access.Load_XML_File_Validated
     ("/home/schwiemn/CVS_Hpro/giant/src/config/giant_global_config.xml",
      The_Tree_Reader,
      The_XML_Document);

   if False then
      -- check file type
      if (Giant.XML_File_Access.Does_XML_Document_Belong_To_Type
             ("giant_visualisation_style_file",
             The_XML_Document)
          = False ) then

         Ada.Text_IO.Put_Line("Wrong type of File");
      end if;
   end if;

   -- Access File Content
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("----------------------------------");
   Ada.Text_IO.Put_Line("File loeaded successful");

   DOM.Core.Nodes.Print (The_XML_Document,
          Print_Comments => True,
          Print_XML_PI => True,
          With_URI => True);

exception
   when E : Sax.Readers.XML_Fatal_Error =>
            -- Input_Sources.Close (Read);
            Ada.Text_IO.Put_Line (Exception_Message (E));
            Tree_Readers.Free (The_Tree_Reader);
end Testxml_Val;

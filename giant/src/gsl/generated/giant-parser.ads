with Giant.Gsl, Giant.Gsl.Syntax_Tree, Giant.Gsl.Types;
use  Giant.Gsl, Giant.Gsl.Syntax_Tree, Giant.Gsl.Types;
with Giant.Gsl_Identifiers;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package Giant.Parser is 

   procedure YYParse;

   function Get_Syntax_Tree return Syntax_Node;

   function Get_Error_Message return Unbounded_String;

end Giant.Parser;

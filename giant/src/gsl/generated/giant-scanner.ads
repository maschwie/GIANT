-- A lexical scanner generated by aflex
with Text_IO; use Text_IO;
--# line 1 "Giant.Scanner.aflex"
-- GSL Scanner rules
-- Compile using aflex, use scanner.aflex as parser
--
-- $Revision: 1.2 $
-- $Date: 2003-10-05 20:49:11 $
-- $Author: keulsn $
--
-- First Author: keulsn
--
-- definitions section
--# line 20 "Giant.Scanner.aflex"


with Giant.Parser.Tokens;
use Giant.Parser.Tokens;

package Giant.Scanner is

   function YYLex return Token;
end Giant.Scanner;

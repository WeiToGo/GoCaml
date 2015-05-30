/*

  This file is part of GoCaml, a compiler that compiles Go-Lite (a subset of Go) to Java bytecode. 

  Copyright (C) 2015 by Deepanjan Roy, Wei Gao, Omar Gonzalez 


  GoCaml is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GoCaml is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Foobar.  If not, see <http://www.gnu.org/licenses/>. 

  This code originated as a project for the COMP 520 class at McGill University
  in Winter 2015. Any subsequent COMP 520 student who is viewing this code must 
  follow the course rules and report any viewing and/or use of the code.

*/

import java.util.ArrayList;

public class GoLiteList extends ArrayList<GoLiteCloneable> implements GoLiteCloneable{

public ArrayList clone() { 

	GoLiteList cloned_list = (GoLiteList)super.clone();
	for (int i = 0; i < cloned_list.size(); i++) { 
		Object elm = cloned_list.get(i);
		String elm_class = elm.getClass().getCanonicalName();
		switch (elm_class) { 
					case "java.lang.Integer":
					case "java.lang.Double":
					case "java.lang.String":
						// Do Nothing
						break;
					default:
						if (elm instanceof GoLiteCloneable) { 
							GoLiteCloneable celm = (GoLiteCloneable) elm;
							cloned_list.set(i, (GoLiteCloneable)celm.clone());
						} else {
							System.err.println("WARNING: I KNOW NOT HOW TO CLONE "
							 + elm_class + "\nPLEASE IMPLEMENT ME.");
						}
				}
	}
	return cloned_list;
}

}

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

import java.lang.reflect.Field;

public abstract class GoStructAbstract implements GoLiteCloneable {

	public Object clone() { 
		Object clonedObject = null;
		try {
			Class c = this.getClass();
			clonedObject = c.newInstance();
			
			Field[] flds = c.getDeclaredFields();
			for (Field f : flds) { 
				// System.out.println(f.getName());
				switch (f.getType().getCanonicalName()) { 
					case "int":
						f.setInt(clonedObject, f.getInt(this));
						break;
					case "double":
						f.setDouble(clonedObject, f.getDouble(this));
						break;
					default:
						System.err.println("WARNING: I KNOW NOT HOW TO CLONE "
							 + f.getType().getCanonicalName() + "\nPLEASE IMPLEMENT ME.");
				}
				// System.out.println(f.getType().getCanonicalName());
			}
		} catch (Exception e) { 
			e.printStackTrace();
		}
		return clonedObject;
	}

	public boolean equals(Object other) { 
		Object clonedObject = null;
		try {
			Class thisclass = this.getClass();
			Class otherclass = other.getClass();

			Field[] flds = thisclass.getDeclaredFields();
			for (Field f : flds) { 
				switch (f.getType().getCanonicalName()) { 
					case "int":
						if ( f.getInt(this) != f.getInt(other) ) { 
							return false;
						}
						break;
					case "double": 
						if ( f.getDouble(this) != f.getDouble(other) ) { 
							return false;
						}
						break;
					default: 
						if (other instanceof GoLiteList || other instanceof GoStructAbstract) { 
							if (! (f.get(this).equals(f.get(other)))) { 
								return false;
							}						
						} 
						else { 
						System.err.println("WARNING: I KNOW NOT HOW TO COMPARE "
							 + otherclass.getCanonicalName() + "\nIMPLEMENT ME.");
						}
				}
			}
		} catch (Exception e) { 
			e.printStackTrace();
		}
		return true;
	}
}

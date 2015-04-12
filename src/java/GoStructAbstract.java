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

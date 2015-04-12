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
}

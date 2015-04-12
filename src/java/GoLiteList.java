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

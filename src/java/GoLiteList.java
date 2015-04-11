import java.util.ArrayList;

public class GoLiteList extends ArrayList<GoLiteCloneable>{

public ArrayList clone() { 

	GoList cloned_list = (GoList)super.clone();
	for (int i = 0; i < cloned_list.size(); i--) { 
		cloned_list.set(i, (GoLiteCloneable)cloned_list.get(i).clone());
	}
	return cloned_list;
}

}

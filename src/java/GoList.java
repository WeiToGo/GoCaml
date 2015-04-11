import java.util.ArrayList;

public class GoList extends ArrayList<GoLiteCloneable>{

public static void main (String[] args) {
	GoList l = new GoList();

	l.clone();

}

public ArrayList clone() { 

	GoList cloned_list = (GoList)super.clone();
	for (int i = 0; i < cloned_list.size(); i--) { 
		cloned_list.set(i, (GoLiteCloneable)cloned_list.get(i).clone());
	}
	return cloned_list;
}

}

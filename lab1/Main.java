import java.util.ArrayList;
import java.util.List;

public class Main {
    public static <A,B> List<B> map(List<A> values, MapFunction<A,B> mapFunction) {
        List<B> result = new ArrayList<>();

        for(A value : values) {
            result.add(mapFunction.map(value));
        }

        return result;
    }

    public static <A> List<A> filter(List<A> values, FilterFunction<A> filterFunction) {
        List<A> result = new ArrayList<>();

        for(A value : values) {
            if(filterFunction.filter(value)) {
                result.add(value);
            }
        }

        return result;
    }

    public static <A,B> B fold(List<A> values, B initialValue, 
                                FoldFunction<A,B> foldFunction) {
        B result = initialValue;

        for(A value : values) {
            result = foldFunction.fold(result, value);
        }

        return result;
    } 
    
    public static <A,B,C> List<C> zipWith(List<A> list1, List<B> list2, 
                                                ZipFunction<A,B,C> zipFunction) {
        List<C> result = new ArrayList<>();
        
        for(int i = 0; i < list1.size(); i++) {
            result.add(zipFunction.zip(list1.get(i), list2.get(i)));
        }

        return result;
    }

    public static void main(String[] args) {
        List<Integer> list = new ArrayList<>();
        list.add(10);
        list.add(20);
        list.add(30);

        System.out.println(map(list, value -> value + 2));
        System.out.println(map(list, value -> value + "x"));
        System.out.println(filter(list, value -> value > 10));
        System.out.println(filter(list, value -> value == 20));
        System.out.println(fold(list, 0, Integer::sum));
        System.out.println(fold(list, "", (a,b) -> a + b));

        List<Integer> list1 = new ArrayList<>();
        list1.add(3);
        list1.add(4);
        list1.add(5);

        List<Integer> list2 = new ArrayList<>();
        list2.add(5);
        list2.add(4);
        list2.add(3);

        System.out.println(zipWith(list1, list2, (value1, value2) -> value1 + value2));
    }
}
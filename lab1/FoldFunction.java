public interface FoldFunction<A,B> {
    B fold(B accu, A value);
}

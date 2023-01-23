package info.kgeorgiy.ja.stupnikov.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final List<E> elementData;
    private final Comparator<? super E> comparator;

    private ArraySet(final List<E> subList, final Comparator<? super E> comparator) {
        this.elementData = subList;
        this.comparator = comparator;
    }

    public ArraySet() {
        comparator = null;
        this.elementData = List.of();
    }

    public ArraySet(final Collection<? extends E> c) {
        this(c, null);
    }

    public ArraySet(final ArraySet<E> c) {
        elementData = c.elementData;
        comparator = c.comparator;
    }

    @SuppressWarnings("unchecked")
    private int compare(final E a, final E b) {
        return this.comparator != null ? comparator.compare(a, b) : ((Comparable<E>) a).compareTo(b);
    }

    public ArraySet(final Collection<? extends E> c, final Comparator<? super E> comparator) {
        this.comparator = comparator;
        final Set<E> set = new TreeSet<>(comparator);
        set.addAll(c);
        elementData = List.copyOf(set);
    }

    @Override
    public Iterator<E> iterator() {
        return elementData.iterator();
    }

    @Override
    public int size() {
        return elementData.size();
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    private int find(final E element) {
        final int index = Collections.binarySearch(elementData, element, comparator);
        return index < 0 ? - index - 1 : index;
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(final Object o) {
        return Collections.binarySearch(elementData, (E) o, comparator) >= 0;
    }

    @Override
    public SortedSet<E> subSet(final E fromElement, final E toElement) {
        if (compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }
        return new ArraySet<>(elementData.subList(find(fromElement), find(toElement)), comparator);
    }

    @Override
    public SortedSet<E> headSet(final E toElement) {
        return new ArraySet<>(elementData.subList(0, find(toElement)), comparator);
    }

    @Override
    public SortedSet<E> tailSet(final E fromElement) {
        return new ArraySet<>(elementData.subList(find(fromElement), size()), comparator);
    }

    private E get(int i) {
        if (i >= size()) {
            throw new NoSuchElementException();
        }
        return elementData.get(i);
    }

    @Override
    public E first() {
        return get(0);
    }

    @Override
    public E last() {
        return get(size() - 1);
    }
}

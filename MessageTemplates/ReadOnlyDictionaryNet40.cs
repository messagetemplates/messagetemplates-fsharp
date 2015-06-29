using System.Linq;

namespace System.Collections.Generic
{
#if USE_READONLY_COLLECTION_40
    //
    // Summary:
    //     Represents a strongly-typed, read-only collection of elements.
    //
    // Type parameters:
    //   T:
    //     The type of the elements.This type parameter is covariant. That is, you can use
    //     either the type you specified or any type that is more derived. For more information
    //     about covariance and contravariance, see Covariance and Contravariance in Generics.
    public interface IReadOnlyCollection<out T> : IEnumerable<T>, IEnumerable
    {
        //
        // Summary:
        //     Gets the number of elements in the collection.
        //
        // Returns:
        //     The number of elements in the collection.
        int Count { get; }
    }

    public interface IReadOnlyDictionary<TKey, TValue> :
        IReadOnlyCollection<KeyValuePair<TKey, TValue>>,
        IEnumerable<KeyValuePair<TKey, TValue>>,
        IEnumerable
    {
        //
        // Summary:
        //     Gets the element that has the specified key in the read-only dictionary.
        //
        // Parameters:
        //   key:
        //     The key to locate.
        //
        // Returns:
        //     The element that has the specified key in the read-only dictionary.
        //
        // Exceptions:
        //   T:System.ArgumentNullException:
        //     key is null.
        //
        //   T:System.Collections.Generic.KeyNotFoundException:
        //     The property is retrieved and key is not found.
        TValue this[TKey key] { get; }

        //
        // Summary:
        //     Gets an enumerable collection that contains the keys in the read-only dictionary.
        //
        // Returns:
        //     An enumerable collection that contains the keys in the read-only dictionary.
        IEnumerable<TKey> Keys { get; }
        //
        // Summary:
        //     Gets an enumerable collection that contains the values in the read-only dictionary.
        //
        // Returns:
        //     An enumerable collection that contains the values in the read-only dictionary.
        IEnumerable<TValue> Values { get; }

        //
        // Summary:
        //     Determines whether the read-only dictionary contains an element that has the
        //     specified key.
        //
        // Parameters:
        //   key:
        //     The key to locate.
        //
        // Returns:
        //     true if the read-only dictionary contains an element that has the specified key;
        //     otherwise, false.
        //
        // Exceptions:
        //   T:System.ArgumentNullException:
        //     key is null.
        bool ContainsKey(TKey key);
        //
        // Summary:
        //     Gets the value that is associated with the specified key.
        //
        // Parameters:
        //   key:
        //     The key to locate.
        //
        //   value:
        //     When this method returns, the value associated with the specified key, if the
        //     key is found; otherwise, the default value for the type of the value parameter.
        //     This parameter is passed uninitialized.
        //
        // Returns:
        //     true if the object that implements the System.Collections.Generic.IReadOnlyDictionary`2
        //     interface contains an element that has the specified key; otherwise, false.
        //
        // Exceptions:
        //   T:System.ArgumentNullException:
        //     key is null.
        bool TryGetValue(TKey key, out TValue value);
    }

    public interface IReadOnlyList<out T> :
        IReadOnlyCollection<T>, IEnumerable<T>, IEnumerable
    {
        T this[int index] { get; }
    }

    public class ReadOnlyDictionary<TKey, TValue> :
        IReadOnlyDictionary<TKey, TValue>
    {
        private readonly Dictionary<TKey, TValue> dictionary;

        public ReadOnlyDictionary(Dictionary<TKey, TValue> dictionary)
        {
            this.dictionary = dictionary;
        }

        public TValue this[TKey key] { get { return this.dictionary[key]; } }
        public int Count { get { return this.dictionary.Count; } }
        public IEnumerable<TKey> Keys { get { return this.dictionary.Keys; } }
        public IEnumerable<TValue> Values { get { return this.dictionary.Values; } }
        public bool ContainsKey(TKey key) { return this.dictionary.ContainsKey(key); }
        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator() {
            return this.dictionary.GetEnumerator();
        }
        public bool TryGetValue(TKey key, out TValue value) {
            return this.dictionary.TryGetValue(key, out value);
        }
        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable)this.dictionary).GetEnumerator();
        }
    }

    public class ReadOnlyList<T> : IReadOnlyList<T>
    {
        private readonly T[] elements;

        public ReadOnlyList(T[] elements)
        {
            this.elements = elements;
        }
        public T this[int index] { get { return this.elements[index]; } }
        public int Count { get { return this.elements.Length; } }
        public IEnumerator<T> GetEnumerator() {
            return this.elements.AsEnumerable().GetEnumerator();
        }
        IEnumerator IEnumerable.GetEnumerator() {
            return ((IEnumerable)this.elements).GetEnumerator();
        }
    }
#endif

    public static class Net40ReadOnlyDictionaryExtensions
    {
        public static IReadOnlyList<T> ToListNet40<T>(
            this T[] source)
        {
#if USE_READONLY_COLLECTION_40
            return new ReadOnlyList<T>(source.ToArray());
#else
            return source;
#endif
        }

        public static IReadOnlyDictionary<TKey, TElement> ToDictionary40<TSource, TKey, TElement>(
            this IEnumerable<TSource> source,
            Func<TSource, TKey> keySelector,
            Func<TSource, TElement> elementSelector)
        {
#if USE_READONLY_COLLECTION_40
            return new ReadOnlyDictionary<TKey, TElement>(
                source.ToDictionary(keySelector, elementSelector));
#else
            return source.ToDictionary(keySelector, elementSelector);
#endif
        }
    }
}

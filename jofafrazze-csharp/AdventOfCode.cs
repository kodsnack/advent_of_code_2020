using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AdventOfCode
{
    public struct GenericPosition2D<T> : IComparable<GenericPosition2D<T>>
    {
        public T x;
        public T y;

        public GenericPosition2D(GenericPosition2D<T> p)
        {
            x = p.x;
            y = p.y;
        }
        public GenericPosition2D(T x, T y)
        {
            this.x = x;
            this.y = y;
        }
        public int CompareTo(GenericPosition2D<T> p)    // Reading order
        {
            if (!EqualityComparer<T>.Default.Equals(y, p.y))
                return Comparer<T>.Default.Compare(y, p.y);
            else
                return Comparer<T>.Default.Compare(x, p.x);
        }
        public override bool Equals(Object obj)
        {
            return obj is GenericPosition2D<T> && Equals((GenericPosition2D<T>)obj);
        }
        public bool Equals(GenericPosition2D<T> p)
        {
            return EqualityComparer<T>.Default.Equals(x, p.x) && EqualityComparer<T>.Default.Equals(y, p.y);
        }
        public override int GetHashCode()
        {
            var hashCode = 1502939027;
            hashCode = hashCode * -1521134295 + base.GetHashCode();
            hashCode = hashCode * -1521134295 + x.GetHashCode();
            hashCode = hashCode * -1521134295 + y.GetHashCode();
            return hashCode;
        }
        public static bool operator ==(GenericPosition2D<T> p1, GenericPosition2D<T> p2) { return p1.Equals(p2); }
        public static bool operator !=(GenericPosition2D<T> p1, GenericPosition2D<T> p2) { return !p1.Equals(p2); }
        public static bool operator <(GenericPosition2D<T> p1, GenericPosition2D<T> p2) { return p1.CompareTo(p2) < 0; }
        public static bool operator <=(GenericPosition2D<T> p1, GenericPosition2D<T> p2) { return p1.CompareTo(p2) <= 0; }
        public static bool operator >(GenericPosition2D<T> p1, GenericPosition2D<T> p2) { return p1.CompareTo(p2) > 0; }
        public static bool operator >=(GenericPosition2D<T> p1, GenericPosition2D<T> p2) { return p1.CompareTo(p2) >= 0; }
        public static GenericPosition2D<T> operator +(GenericPosition2D<T> p1, T k)
        {
            return p1 + new GenericPosition2D<T>(k, k);
        }
        public static GenericPosition2D<T> operator +(GenericPosition2D<T> p1, GenericPosition2D<T> p2)
        {
            GenericPosition2D<T> p = new GenericPosition2D<T>(p1);
            p.x = Extensions.Add(p.x, p2.x);
            p.y = Extensions.Add(p.y, p2.y);
            return p;
        }
        public static GenericPosition2D<T> operator -(GenericPosition2D<T> p1, T k)
        {
            GenericPosition2D<T> p = new GenericPosition2D<T>(p1);
            p.x = Extensions.Subtract(p.x, k);
            p.y = Extensions.Subtract(p.y, k);
            return p;
        }
        public static GenericPosition2D<T> operator -(GenericPosition2D<T> p1, GenericPosition2D<T> p2)
        {
            GenericPosition2D<T> p = new GenericPosition2D<T>(p1);
            p.x = Extensions.Subtract(p.x, p2.x);
            p.y = Extensions.Subtract(p.y, p2.y);
            return p;
        }
        public static GenericPosition2D<T> operator *(GenericPosition2D<T> p1, T k)
        {
            GenericPosition2D<T> p = new GenericPosition2D<T>(p1);
            p.x = Extensions.Multiply(p.x, k);
            p.y = Extensions.Multiply(p.y, k);
            return p;
        }
        public static GenericPosition2D<T> operator *(GenericPosition2D<T> p1, GenericPosition2D<T> p2)
        {
            GenericPosition2D<T> p = new GenericPosition2D<T>(p1);
            p.x = Extensions.Multiply(p.x, p2.x);
            p.y = Extensions.Multiply(p.y, p2.y);
            return p;
        }
        public static GenericPosition2D<T> operator /(GenericPosition2D<T> p1, T k)
        {
            GenericPosition2D<T> p = new GenericPosition2D<T>(p1);
            p.x = Extensions.Divide(p.x, k);
            p.y = Extensions.Divide(p.y, k);
            return p;
        }
        public T ManhattanDistance(GenericPosition2D<T> p = new GenericPosition2D<T>())
        {
            return Extensions.Add(
                Extensions.Abs(Extensions.Subtract(x, p.x)), 
                Extensions.Abs(Extensions.Subtract(y, p.y))
                );
        }
    }

    public struct GenericPosition3D<T> : IComparable<GenericPosition3D<T>>
    {
        public T x;
        public T y;
        public T z;

        public GenericPosition3D(GenericPosition3D<T> p)
        {
            x = p.x;
            y = p.y;
            z = p.z;
        }
        public GenericPosition3D(T x, T y, T z)
        {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        public int CompareTo(GenericPosition3D<T> p)
        {
            if (!EqualityComparer<T>.Default.Equals(z, p.z))
                return Comparer<T>.Default.Compare(z, p.z);
            else if (!EqualityComparer<T>.Default.Equals(y, p.y))
                return Comparer<T>.Default.Compare(y, p.y);
            else
                return Comparer<T>.Default.Compare(x, p.x);
        }
        public override bool Equals(Object obj)
        {
            return obj is GenericPosition3D<T> && Equals((GenericPosition3D<T>)obj);
        }
        public bool Equals(GenericPosition3D<T> p)
        {
            return 
                EqualityComparer<T>.Default.Equals(x, p.x) &&
                EqualityComparer<T>.Default.Equals(y, p.y) &&
                EqualityComparer<T>.Default.Equals(z, p.z);
        }
        public override int GetHashCode()
        {
            var hashCode = 1502939027;
            hashCode = hashCode * -1521134295 + base.GetHashCode();
            hashCode = hashCode * -1521134295 + x.GetHashCode();
            hashCode = hashCode * -1521134295 + y.GetHashCode();
            hashCode = hashCode * -1521134295 + z.GetHashCode();
            return hashCode;
        }
        public static bool operator ==(GenericPosition3D<T> p1, GenericPosition3D<T> p2) { return p1.Equals(p2); }
        public static bool operator !=(GenericPosition3D<T> p1, GenericPosition3D<T> p2) { return !p1.Equals(p2); }
        public static bool operator <(GenericPosition3D<T> p1, GenericPosition3D<T> p2) { return p1.CompareTo(p2) < 0; }
        public static bool operator <=(GenericPosition3D<T> p1, GenericPosition3D<T> p2) { return p1.CompareTo(p2) <= 0; }
        public static bool operator >(GenericPosition3D<T> p1, GenericPosition3D<T> p2) { return p1.CompareTo(p2) > 0; }
        public static bool operator >=(GenericPosition3D<T> p1, GenericPosition3D<T> p2) { return p1.CompareTo(p2) >= 0; }
        public static GenericPosition3D<T> operator +(GenericPosition3D<T> p1, T k)
        {
            return p1 + new GenericPosition3D<T>(k, k, k);
        }
        public static GenericPosition3D<T> operator +(GenericPosition3D<T> p1, GenericPosition3D<T> p2)
        {
            GenericPosition3D<T> p = new GenericPosition3D<T>(p1);
            p.x = Extensions.Add(p.x, p2.x);
            p.y = Extensions.Add(p.y, p2.y);
            p.z = Extensions.Add(p.z, p2.z);
            return p;
        }
        public static GenericPosition3D<T> operator -(GenericPosition3D<T> p1, T k)
        {
            GenericPosition3D<T> p = new GenericPosition3D<T>(p1);
            p.x = Extensions.Subtract(p.x, k);
            p.y = Extensions.Subtract(p.y, k);
            p.z = Extensions.Subtract(p.z, k);
            return p;
        }
        public static GenericPosition3D<T> operator -(GenericPosition3D<T> p1, GenericPosition3D<T> p2)
        {
            GenericPosition3D<T> p = new GenericPosition3D<T>(p1);
            p.x = Extensions.Subtract(p.x, p2.x);
            p.y = Extensions.Subtract(p.y, p2.y);
            p.z = Extensions.Subtract(p.z, p2.z);
            return p;
        }
        public static GenericPosition3D<T> operator *(GenericPosition3D<T> p1, T k)
        {
            GenericPosition3D<T> p = new GenericPosition3D<T>(p1);
            p.x = Extensions.Multiply(p.x, k);
            p.y = Extensions.Multiply(p.y, k);
            p.z = Extensions.Multiply(p.z, k);
            return p;
        }
        public static GenericPosition3D<T> operator *(GenericPosition3D<T> p1, GenericPosition3D<T> p2)
        {
            GenericPosition3D<T> p = new GenericPosition3D<T>(p1);
            p.x = Extensions.Multiply(p.x, p2.x);
            p.y = Extensions.Multiply(p.y, p2.y);
            p.z = Extensions.Multiply(p.z, p2.z);
            return p;
        }
        public static GenericPosition3D<T> operator /(GenericPosition3D<T> p1, T k)
        {
            GenericPosition3D<T> p = new GenericPosition3D<T>(p1);
            p.x = Extensions.Divide(p.x, k);
            p.y = Extensions.Divide(p.y, k);
            p.z = Extensions.Divide(p.z, k);
            return p;
        }
        public T ManhattanDistance(GenericPosition3D<T> p = new GenericPosition3D<T>())
        {
            return Extensions.Add(
                Extensions.Abs(Extensions.Subtract(x, p.x)),
                Extensions.Abs(Extensions.Subtract(y, p.y)), 
                Extensions.Abs(Extensions.Subtract(z, p.z))
                );
        }
    }

    public class Map
    {
        public int width;
        public int height;
        public GenericPosition2D<int> pos;
        public char[,] data;

        public Map(int w, int h, GenericPosition2D<int> p, char fill = '\0')
        {
            width = w;
            height = h;
            pos = p;
            data = new char[w, h];
            for (int i = 0; i < w * h; i++)
            {
                data[i % w, i / w] = fill;
            }
        }

        public Map(Map m)
        {
            width = m.width;
            height = m.height;
            pos = m.pos;
            data = new char[width, height];
            for (int y = 0; y < height; y++)
                for (int x = 0; x < width; x++)
                    data[x, y] = m.data[x, y];
        }

        public char this[GenericPosition2D<int> p]
        {
            get
            {
                return data[p.x, p.y];
            }
            set
            {
                data[p.x, p.y] = value;
            }
        }

        public bool HasPosition(GenericPosition2D<int> p)
        {
            return p.x >= 0 && p.x < width && p.y >= 0 && p.y < height;
        }

        public void Expand(int n, char fill) { Expand(n, n, n, n, fill); }
        public void Expand(int top, int right, int bottom, int left, char fill)
        {
            int w = left + right + width;
            int h = top + bottom + height;
            GenericPosition2D<int> s = new GenericPosition2D<int>(pos.x + left, pos.y + top);
            Map m = new Map(w, h, s, fill);
            for (int y = 0; y < height; y++)
                for (int x = 0; x < width; x++)
                    m.data[x + left, y + top] = data[x, y];
            width = m.width;
            height = m.height;
            pos = m.pos;
            data = m.data;
        }

        public string PrintToString()
        {
            string s = "";
            for (int y = 0; y < height; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < width; x++)
                {
                    sb.Append(data[x, y]);
                }
                s += sb.ToString() + "\r\n";
            }
            return s;
        }

        public void Print()
        {
            Console.WriteLine(PrintToString());
        }
    }

    public static class Algorithms
    {
        public static List<List<T>> HeapPermutation<T>(List<T> a)
        {
            List<List<T>> result = new List<List<T>>();
            void Swap(ref List<T> b, int i1, int i2)
            {
                T temp = b[i1];
                b[i1] = b[i2];
                b[i2] = temp;
            }
            void Permute(ref List<T> b, int size)
            {
                if (size == 1)
                {
                    result.Add(new List<T>(b));
                }
                else
                {
                    for (int i = 0; i < size - 1; i++)
                    {
                        Permute(ref b, size - 1);
                        Swap(ref b, (size % 2 == 0) ? i : 0, size - 1);
                    }
                    Permute(ref b, size - 1);
                }
            }
            List<T> copy = new List<T>(a);
            Permute(ref copy, copy.Count);
            return result;
        }

        public static List<List<T>> GetCombinations<T>(List<T> input)
        {
            return GetCombinations(input, input.Count);
        }
        public static List<List<T>> GetCombinations<T>(List<T> input, int maxLength)
        {
            List<List<T>> results = new List<List<T>>();
            for (int i = 0; i < input.Count; i++)
            {
                List<T> current = new List<T>() { input[i] };
                int size = results.Count;
                for (int r = 0; r < size; r++)
                {
                    List<T> list = current.Concat(results[r]).ToList();
                    if (list.Count <= maxLength)
                        results.Add(list);
                }
                results.Add(current);
            }
            return results;
        }
    }

    public static class Extensions
    {
        public static T Add<T>(T number1, T number2)
        {
            dynamic a = number1;
            dynamic b = number2;
            return a + b;
        }
        public static T Add<T>(T number1, T number2, T number3)
        {
            dynamic a = number1;
            dynamic b = number2;
            dynamic c = number3;
            return a + b + c;
        }
        public static T Subtract<T>(T number1, T number2)
        {
            dynamic a = number1;
            dynamic b = number2;
            return a - b;
        }
        public static T Multiply<T>(T number1, T number2)
        {
            dynamic a = number1;
            dynamic b = number2;
            return a * b;
        }
        public static T Divide<T>(T number1, T number2)
        {
            dynamic a = number1;
            dynamic b = number2;
            return a / b;
        }
        public static T Abs<T>(T number)
        {
            dynamic a = number;
            return Math.Abs(a);
        }

        public static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> elements, int k)
        {
            return k == 0 ? new[] { new T[0] } :
              elements.SelectMany((e, i) =>
                elements.Skip(i + 1).Combinations(k - 1).Select(c => (new[] { e }).Concat(c)));
        }
    }

    public static class CircularLinkedList
    {
        public static LinkedListNode<T> NextOrFirst<T>(this LinkedListNode<T> current)
        {
            return current.Next ?? current.List.First;
        }

        public static LinkedListNode<T> PreviousOrLast<T>(this LinkedListNode<T> current)
        {
            return current.Previous ?? current.List.Last;
        }
    }

    public static class Graph
    {
        public class Node<T>
        {
            public T t;
            public HashSet<Node<T>> edges;
            public Node(T tp = default(T))
            {
                t = tp;
                edges = new HashSet<Node<T>>();
            }
        };

        public static HashSet<T> Union<T>(HashSet<T> a, HashSet<T> b)
        {
            HashSet<T> c = new HashSet<T>(a);
            c.UnionWith(b);
            return c;
        }

        public static HashSet<T> Intersection<T>(HashSet<T> a, HashSet<T> b)
        {
            HashSet<T> c = new HashSet<T>(a);
            c.IntersectWith(b);
            return c;
        }

        public static HashSet<T> Difference<T>(HashSet<T> a, HashSet<T> b)
        {
            HashSet<T> c = new HashSet<T>(a);
            c.ExceptWith(b);
            return c;
        }

        public static void BronKerbosch1<T>(HashSet<Node<T>> R, HashSet<Node<T>> P, HashSet<Node<T>> X, ref List<HashSet<Node<T>>> cliques)
        {
            if (P.Count == 0 && X.Count == 0)
            {
                cliques.Add(R);
                //Console.WriteLine("BK1: Found clique with {0} nodes.", R.Count);
            }
            else
            {
                HashSet<Node<T>> Pc = new HashSet<Node<T>>(P.OrderByDescending(x => x.edges.Count));
                foreach (Node<T> n in Pc)
                {
                    HashSet<Node<T>> s = new HashSet<Node<T>>() { n };
                    BronKerbosch1(Union<Node<T>>(R, s), Intersection<Node<T>>(P, n.edges), Intersection<Node<T>>(X, n.edges), ref cliques);
                    P = Difference<Node<T>>(P, s);
                    X = Union<Node<T>>(X, s);
                }
            }
        }

        public static void BronKerbosch2<T>(HashSet<Node<T>> R, HashSet<Node<T>> P, HashSet<Node<T>> X, ref List<HashSet<Node<T>>> cliques)
        {
            if (P.Count == 0 && X.Count == 0)
            {
                cliques.Add(R);
                //Console.WriteLine("BK2: Found clique with {0} nodes.", R.Count);
            }
            else
            {
                Node<T> pivot = Union<Node<T>>(P, X).OrderBy(x => x.edges.Count).Last();
                HashSet<Node<T>> Pu = Difference<Node<T>>(P, pivot.edges);
                foreach (Node<T> n in Pu)
                {
                    HashSet<Node<T>> s = new HashSet<Node<T>>() { n };
                    BronKerbosch2(Union<Node<T>>(R, s), Intersection<Node<T>>(P, n.edges), Intersection<Node<T>>(X, n.edges), ref cliques);
                    P = Difference<Node<T>>(P, s);
                    X = Union<Node<T>>(X, s);
                }
            }
        }
    }

    public static class Tree
    {
        public class Node<T>
        {
            public Node<T> parent;
            public T t;
            public HashSet<Node<T>> children;
            public Node(T v = default(T), Node<T> p = default(Node<T>))
            {
                t = v;
                parent = p;
                children = new HashSet<Node<T>>();
            }
        };
    }

}

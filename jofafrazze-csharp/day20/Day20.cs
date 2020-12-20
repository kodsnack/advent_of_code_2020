using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day20
{
    public class Day20
    {
        readonly static string nsname = typeof(Day20).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static Dictionary<int, List<string>> GetTiles(List<string> list)
        {
            var dict = new Dictionary<int, List<string>>();
            int id = 0;
            var tile = new List<string>();
            foreach (string s in list)
            {
                var m = s.Split(" :".ToCharArray());
                if (s == "")
                    dict[id] = tile;
                else if (m.Count() > 1)
                {
                    id = int.Parse(m[1]);
                    tile = new List<string>();
                }
                else
                    tile.Add(s);
            }
            dict[id] = tile;
            return dict;
        }

        static List<string> FlipHz(List<string> list)
        {
            var a = new List<string>();
            foreach (string s in list)
            {
                string b = new string(s.Reverse().ToArray());
                a.Add(b);
            }
            return a;
        }

        static List<string> FlipVc(List<string> list)
        {
            var a = new List<string>(list);
            a.Reverse();
            return a;
        }

        static List<string> Rot(List<string> list)
        {
            int k = list.Count;
            char[][] arr = list.Select(l => l.ToArray()).ToArray();
            for (int y = 0; y < k; y++)
                for (int x = 0; x < k; x++)
                    arr[y][x] = list[x][k - y - 1];
            var a = new List<string>();
            for (int y = 0; y < k; y++)
                a.Add(new string(arr[y]));
            return a;
        }

        static Dictionary<int, List<List<string>>> GetAllVariants(Dictionary<int, List<string>> tiles)
        {
            var dict = new Dictionary<int, List<List<string>>>();
            foreach (var (k, v) in tiles)
            {
                var a = tiles[k];
                var hz = FlipHz(tiles[k]);
                var vc = FlipVc(tiles[k]);
                List<List<string>> w = new List<List<string>>() { a, vc };
                var all = new List<List<string>>(w);
                foreach (var t in w)
                {
                    var r1 = Rot(t);
                    var r2 = Rot(r1);
                    var r3 = Rot(r2);
                    var q = new List<List<string>>() { r1, r2, r3 };
                    foreach (var h in q)
                    {
                        bool unique = true;
                        foreach (var g in all)
                            if (g.SequenceEqual(h))
                                unique = false;
                        if (unique)
                            all.Add(h);
                    }
                }
                dict[k] = new List<List<string>>(all);
            }
            return dict;
        }

        //  0 1 2  A B C
        //  3 4 5  D E F
        //  6 7 8  G H I

        enum Edge { Top, Right, Bottom, Left };

        // A's left to B's right
        static bool CanTilesMatchLeft(List<string> a, List<string> b)
        {
            int z = a[0].Length - 1;
            for (int r = 0; r < a.Count; r++)
                if (a[r][0] != b[r][z])
                    return false;
            return true;
        }

        // A's top to B's bottom
        static bool CanTilesMatchTop(List<string> a, List<string> b)
        {
            return a[0] == b.Last();
        }

        static (bool match, int idx) CanTilesMatch(int id, List<(int pos, Edge edge)> edgesToCheck)
        {
            for (int idx = 0; idx < 8; idx++)
            {
                bool match = true;
                foreach (var (pos, edge) in edgesToCheck)
                {
                    (int id, int idx) other = tilesUsed[pos];
                    match &= (edge == Edge.Left) ?
                        CanTilesMatchLeft(allTiles[id][idx], allTiles[other.id][other.idx]) :
                        CanTilesMatchTop(allTiles[id][idx], allTiles[other.id][other.idx]);
                }
                if (match)
                    return (true, idx);
            }
            return (false, -1);
        }

        static List<(int id, int idx)> tilesUsed = new List<(int, int)>();

        static bool TilesMatch(int topLeftId, List<int> ids)
        {
            int nTiles = allTiles.Count;
            var atry = allTiles[topLeftId];
            for (int a = 0; a < atry.Count; a++)
            {
                tilesUsed = new List<(int, int)>() { (topLeftId, a) };
                List<int> idsLeft = new List<int>(ids);
                for (int ourPos = 1; (ourPos < nTiles) && tilesUsed.Count == ourPos; ourPos++)
                {
                    for (int i = 0; i < idsLeft.Count; i++)
                    {
                        int nextId = idsLeft[i];
                        List<(int pos, Edge edge)> edgesToCheck = new List<(int, Edge)>();
                        if (ourPos % imageSide != 0)
                            edgesToCheck.Add((ourPos - 1, Edge.Left));
                        if (ourPos >= imageSide)
                            edgesToCheck.Add((ourPos - imageSide, Edge.Top));
                        var (match, idx) = CanTilesMatch(nextId, edgesToCheck);
                        if (match)
                        {
                            tilesUsed.Add((nextId, idx));
                            idsLeft.Remove(nextId);
                            break;
                        }
                    }
                }
                if (tilesUsed.Count == nTiles)
                    return true;
            }
            return false;
        }

        static void MatchTiles()
        {
            List<int> ids = allTiles.Keys.ToList();
            foreach (int id in ids)
            {
                Console.Write('.');
                List<int> idsLeft = new List<int>(ids);
                idsLeft.Remove(id);
                if (TilesMatch(id, idsLeft))
                    return;
            }
            throw new Exception();
        }

        static void PrintTiles()
        {
            for (int r = 0; r < 3; r++)
            {
                for (int i = -1; i < allTiles.First().Value[0].Count; i++)
                {
                    for (int c = 0; c < 3; c++)
                    {
                        int n = r * 3 + c;
                        var (id, idx) = tilesUsed[n];
                        List<string> strs = allTiles[id][idx];
                        if (i == -1)
                            Console.Write("{0,-10} ", id);
                        else
                            Console.Write("{0} ", strs[i]);
                    }
                    Console.WriteLine();
                }
                Console.WriteLine();
            }
        }

        static Dictionary<int, List<List<string>>> allTiles;
        static int imageSide = 0;

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            var tiles = GetTiles(input);
            allTiles = GetAllVariants(tiles);
            imageSide = (int) Math.Sqrt(allTiles.Count);
            MatchTiles();
            //PrintTiles();
            List<long> config = tilesUsed.Select(x => (long)x.id).ToList();
            long a = config[0];
            long b = config[imageSide - 1];
            long c = config[imageSide * (imageSide - 1)];
            long d = config.Last();
            long ans = a * b * c * d;
            Console.WriteLine("Part A: Result is {1} * {2} * {3} * {4} = {0}", ans, a, b, c, d);
            return ans;
        }

        static List<string> image = new List<string>();
        static List<List<string>> images = new List<List<string>>();

        static void CreateImages()
        {
            for (int r = 0; r < imageSide; r++)
            {
                for (int i = 1; i < 9; i++)
                {
                    string s = "";
                    for (int c = 0; c < imageSide; c++)
                    {
                        var (id, idx) = tilesUsed[r * imageSide + c];
                        s += allTiles[id][idx][i].Substring(1, 8);
                    }
                    image.Add(s);
                }
            }
            var vc = FlipVc(image);
            List<List<string>> w = new List<List<string>>() { image, vc };
            images = new List<List<string>>(w);
            foreach (var t in w)
            {
                var r1 = Rot(t);
                var r2 = Rot(r1);
                var r3 = Rot(r2);
                images.Add(r1);
                images.Add(r2);
                images.Add(r3);
            }
        }

        static readonly List<string> seaMonster = new List<string>()
        {
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   "
        };

        static void CreateSeaMonsterOffs()
        {
            for (int r = 0; r < seaMonster.Count; r++)
            {
                for (int c = 0; c < seaMonster[0].Length; c++)
                {
                    if (seaMonster[r][c] == '#')
                        seaMonsterOffs.Add((r, c));
                }
            }
        }

        static readonly List<(int pr, int pc)> seaMonsterOffs = new List<(int, int)>();

        static readonly HashSet<(int pr, int pc)> imageOffs = new HashSet<(int, int)>();

        static void CreateImageOffs(List<string> img)
        {
            for (int r = 0; r < img.Count; r++)
            {
                for (int c = 0; c < img[0].Length; c++)
                {
                    if (img[r][c] == '#')
                        imageOffs.Add((r, c));
                }
            }
        }

        static bool ContainSeaMonster(List<string> img)
        {
            for (int r = 0; r < img.Count - seaMonster.Count; r++)
            {
                for (int c = 0; c < img[0].Length - seaMonster[0].Length; c++)
                {
                    bool found = true;
                    foreach (var (pr, pc) in seaMonsterOffs)
                    {
                        found &= img[r + pr][c + pc] == '#';
                        if (!found)
                            break;
                    }
                    if (found)
                        return true;
                }
            }
            return false;
        }

        static void ClearAllSeaMonsterOffs(List<string> img)
        {
            for (int r = 0; r < img.Count - seaMonster.Count; r++)
            {
                for (int c = 0; c < img[0].Length - seaMonster[0].Length; c++)
                {
                    bool found = true;
                    foreach (var (pr, pc) in seaMonsterOffs)
                    {
                        found &= img[r + pr][c + pc] == '#';
                        if (!found)
                            break;
                    }
                    if (found)
                    {
                        foreach (var (pr, pc) in seaMonsterOffs)
                        {
                            imageOffs.Remove((r + pr, c + pc));
                        }
                    }
                }
            }
        }

        static Object PartB()
        {
            CreateImages();
            CreateSeaMonsterOffs();
            int ans = 0;
            foreach (var img in images)
            {
                if (ContainSeaMonster(img))
                {
                    CreateImageOffs(img);
                    ClearAllSeaMonsterOffs(img);
                    ans = imageOffs.Count;
                    break;
                }
            }
            Console.WriteLine("Part B: Result is {0}", ans);
            return ans;
        }

        static void Main()
        {
            Console.WriteLine("AoC 2020 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            long a = 27798062994017;
            int b = 2366;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}

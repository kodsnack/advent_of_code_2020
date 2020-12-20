using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day20
{
    public class Day20
    {
        readonly static string nsname = typeof(Day20).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 20: Jurassic Jigsaw - Rotate tiles, match edges, find patterns etc

        enum Edge { Top, Left };
        static Dictionary<int, List<List<string>>> allTiles;
        static List<(int id, int idx)> tilesMatched = new List<(int, int)>();
        static int side = 0;

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

        // For future needs: To reverse a string when Linq is enabled: 
        // string b = new string(s.Reverse().ToArray());

        static List<string> FlipVertical(List<string> list)
        {
            var a = new List<string>(list);
            a.Reverse();
            return a;
        }

        static List<string> Rot(List<string> list)
        {
            int k = list.Count;
            char[][] arr = list.Select(l => l.ToArray()).ToArray();
            for (int r = 0; r < k; r++)
                for (int c = 0; c < k; c++)
                    arr[r][c] = list[c][k - r - 1];
            return arr.Select(a => new string(a)).ToList();
        }

        static Dictionary<int, List<List<string>>> GetAllVariants(Dictionary<int, List<string>> tiles)
        {
            var dict = new Dictionary<int, List<List<string>>>();
            foreach (var (k, v) in tiles)
            {
                var a = tiles[k];
                var vc = FlipVertical(tiles[k]);
                var w = new List<List<string>>() { a, vc };
                var all = new List<List<string>>(w);
                foreach (var t in w)
                {
                    var r1 = Rot(t);
                    var r2 = Rot(r1);
                    var r3 = Rot(r2);
                    all.AddRange(new List<List<string>>() { r1, r2, r3 });
                }
                dict[k] = all;
            }
            return dict;
        }

        // A's left matched against B's right
        static bool CanTilesMatchLeft(List<string> a, List<string> b)
        {
            for (int r = 0; r < a.Count; r++)
                if (a[r][0] != b[r][9])
                    return false;
            return true;
        }

        // A's top matched against B's bottom
        static bool CanTilesMatchTop(List<string> a, List<string> b)
        {
            return a[0] == b.Last();
        }

        static bool CanTilesMatch(int id, List<(int pos, Edge edge)> edgesToCheck, out int idx)
        {
            for (idx = 0; idx < 8; idx++)
            {
                bool match = true;
                foreach (var (pos, edge) in edgesToCheck)
                {
                    (int id, int idx) other = tilesMatched[pos];
                    match &= (edge == Edge.Left) ?
                        CanTilesMatchLeft(allTiles[id][idx], allTiles[other.id][other.idx]) :
                        CanTilesMatchTop(allTiles[id][idx], allTiles[other.id][other.idx]);
                }
                if (match)
                    return true;
            }
            return false;
        }

        static bool MatchTilesTopLeft(int tlId, List<int> ids)
        {
            int nTiles = allTiles.Count;
            var atry = allTiles[tlId];
            for (int a = 0; a < atry.Count; a++)
            {
                tilesMatched = new List<(int, int)>() { (tlId, a) };
                var idsRemaining = new List<int>(ids);
                for (int p = 1; (p < nTiles) && tilesMatched.Count == p; p++)
                {
                    for (int i = 0; i < idsRemaining.Count; i++)
                    {
                        int nextId = idsRemaining[i];
                        var edgesToCheck = new List<(int, Edge)>();
                        if (p % side != 0)
                            edgesToCheck.Add((p - 1, Edge.Left));
                        if (p >= side)
                            edgesToCheck.Add((p - side, Edge.Top));
                        if (CanTilesMatch(nextId, edgesToCheck, out int idx))
                        {
                            tilesMatched.Add((nextId, idx));
                            idsRemaining.Remove(nextId);
                            break;
                        }
                    }
                }
                if (tilesMatched.Count == nTiles)
                    return true;
            }
            return false;
        }

        static void MatchTiles()
        {
            var ids = allTiles.Keys.ToList();
            foreach (int id in ids)
            {
                var idsRemaining = new List<int>(ids);
                idsRemaining.Remove(id);
                if (MatchTilesTopLeft(id, idsRemaining))
                    return;
            }
            throw new InvalidProgramException();
        }

        static void PrintTiles3x3()
        {
            for (int r = 0; r < 3; r++)
            {
                for (int i = -1; i < 10; i++)
                {
                    for (int c = 0; c < 3; c++)
                    {
                        var (id, idx) = tilesMatched[r * 3 + c];
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

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            allTiles = GetAllVariants(GetTiles(input));
            side = (int) Math.Sqrt(allTiles.Count);
            MatchTiles();
            //PrintTiles3x3();
            var ids = tilesMatched.Select(x => (long)x.id).ToList();
            long ans = ids[0] * ids[side - 1] * ids[side * (side - 1)] * ids.Last();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        // Part 2 below

        static List<string> image = new List<string>();
        static List<List<string>> images = new List<List<string>>();
        static readonly List<string> seaMonster = new List<string>()
        {
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   "
        };
        static readonly List<(int pr, int pc)> seaMonsterOffs = new List<(int, int)>();
        static readonly HashSet<(int pr, int pc)> imageOffs = new HashSet<(int, int)>();

        static void CreateImages()
        {
            for (int r = 0; r < side; r++)
                for (int i = 1; i < 9; i++)
                {
                    string s = "";
                    for (int c = 0; c < side; c++)
                    {
                        var (id, idx) = tilesMatched[r * side + c];
                        s += allTiles[id][idx][i].Substring(1, 8);
                    }
                    image.Add(s);
                }
            var vc = FlipVertical(image);
            var w = new List<List<string>>() { image, vc };
            images = new List<List<string>>(w);
            foreach (var t in w)
            {
                var r1 = Rot(t);
                var r2 = Rot(r1);
                var r3 = Rot(r2);
                images.AddRange(new List<List<string>>() { r1, r2, r3 });
            }
        }

        static void CreateOffs(IList<string> list, ICollection<(int,int)> offs)
        {
            for (int r = 0; r < list.Count; r++)
                for (int c = 0; c < list[0].Length; c++)
                    if (list[r][c] == '#')
                        offs.Add((r, c));
        }
        static void CreateSeaMonsterOffs()
        {
            CreateOffs(seaMonster, seaMonsterOffs);
        }
        static void CreateImageOffs(List<string> img)
        {
            imageOffs.Clear();
            CreateOffs(img, imageOffs);
        }

        static bool ClearAllSeaMonsterOffs(List<string> img)
        {
            bool anyFound = false;
            for (int r = 0; r < img.Count - seaMonster.Count; r++)
                for (int c = 0; c < img[0].Length - seaMonster[0].Length; c++)
                {
                    bool found = true;
                    foreach (var (pr, pc) in seaMonsterOffs)
                        found &= imageOffs.Contains((r + pr, c + pc));
                    anyFound |= found;
                    if (found)
                        foreach (var (pr, pc) in seaMonsterOffs)
                            imageOffs.Remove((r + pr, c + pc));
                }
            return anyFound;
        }

        static Object PartB()
        {
            CreateImages();
            CreateSeaMonsterOffs();
            int ans = 0;
            foreach (var img in images)
            {
                CreateImageOffs(img);
                if (ClearAllSeaMonsterOffs(img))
                {
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

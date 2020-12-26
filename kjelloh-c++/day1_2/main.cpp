//
//  main.cpp
//  AdventOfCode201201_2
//
//  Created by Kjell-Olov Högdal on 2020-12-26.
//

#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>

const char* pData = R"(1789
1818
1729
1578
1927
751
1772
1521
1850
1438
1855
1334
1878
1290
1678
1847
1495
1538
1403
1797
1906
1770
1963
1370
1684
1328
1544
1528
1871
2010
1999
1347
1760
1903
1860
1468
1511
1477
1668
1979
1358
1298
1493
1459
1382
2001
1394
1681
1515
1948
1991
1775
1661
1786
1966
1506
1853
1373
1454
1462
1830
1964
1442
1455
2008
1854
1763
1758
1751
1460
1630
1487
1360
1793
1590
1940
1388
1313
1408
1429
1725
1397
1941
1974
1788
1473
1913
664
1989
1490
1778
1726
1366
2005
1449
1924
1926
1769
1314
1636
1780
1546
1647
1856
320
396
1595
1867
1602
1699
1367
1876
1662
1686
1581
1697
1938
1400
720
1808
1625
1439
1734
2003
1718
1879
1864
1811
1309
721
1607
1814
1484
1869
1736
1507
1437
1894
1561
2004
269
1942
1915
1767
1562
1364
1783
1863
1601
1323
182
1985
1722
1545
1774
1552
1742
1790
1874
1583
1308
1441
1463
1503
1447
1540
1953
1371
1331
1688
1905
1815
1799
811
1446
1374
1936
1665
1433
1551
1806
1674
1784
1596
1704
1393
1691
1567
1335
593
1509
1986
1297
1419
1418
1339
1745
1930
1514
1706)";

// log_2(10 pow 12) = 12 / log_2(12) = 40 bits worst case.
// But our sum is to be 2020 which means our terms fits in double the 10 pow 3 = 10 bits, say 11 bits :)
using Result = std::uint32_t; // best largets candidate :)

int main(int argc, const char * argv[]) {
    const auto magic_sum = 2020;
    std::istringstream in {pData};
    std::vector<int> v {};
    int n;
    while (in >> n) {
        v.push_back(n);
    }
    auto N = v.size();
    Result result {0};
    // Brute Force :)
    for (int i = 0; i<N; ++i) {
        for (int j = 0; j<N; ++j) {
            if (j != i) {
                for (int k = 0; k<N; ++k) {
                    if (v[i]+v[j]+v[k] == magic_sum) {
                        result = v[i]*v[j]*v[k];
                        break;
                    }
                }
            }
        }
    }
    std::cout << "\nResult = " << result;
    
    std::cout << "\n\n";
    return 0;
}

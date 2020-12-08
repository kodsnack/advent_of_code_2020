//
//  main.cpp
//  CodeOfAdvent201201_1
//
//  Created by Kjell-Olov Högdal on 2020-12-01.
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


int main(int argc, const char * argv[]) {
    const auto magic_sum = 2020;
    std::istringstream in {pData};
    std::vector<int> v {};
    int n;
    while (in >> n) {
        v.push_back(n);
        std::cout << "\n" << n;
    }
    std::sort(v.begin(),v.end());
    for (auto n : v) {
        std::cout << "\n" << n;
    }
    auto up_it = std::find_if(v.begin(),v.end(),[](auto n) {return n >= magic_sum/2;});
    auto down_it = std::make_reverse_iterator(up_it-1); // Assume up_it has elements preceeding it!
    struct Match {bool matched; int i; int j;} match {false,0,0};
    while ((up_it != v.end()) and (down_it != v.rend())) {
        if (*up_it + *down_it > magic_sum) { ++down_it;} // next lower member
        else if (*up_it + *down_it < magic_sum) {++up_it;} // next higher element
        else {
            match.matched = true;
            match.i = *down_it;
            match.j = *up_it;
            break;
        }
    }
    if (match.matched) {
        std::cout << "\nMATCH :) " << match.i << " + " << match.j << " = " << (match.i + match.j) << " and " << match.i << " * " << match.j << " = " << (match.i * match.j);
    }
    else {
        std::cout << "\nNO MATCH :(";
    }
    std::cout << "\n";
    return 0;
}

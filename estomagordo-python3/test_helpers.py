from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def test_distance_2d():
    a = [2, 3]
    b = [5, 7]

    d = distance(a, b)
    expected = 5

    assert(expected == d) 
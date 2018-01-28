import numpy as np
from unittest import TestCase
from primitives.line import Line
from primitives.point import Point

class TestLine(TestCase):
    @classmethod
    def setUpClass(cls):
        p1 = Point(0, 0)
        p2 = Point(1, 1)
        l = Line([p1, p2])

    def test_coords(self):
        np.allclose(l.coords, np.array([[0, 0, np.nan], [1, 1, np.nan]]))
    #
    # def test_setcoords(self):
    #     # self.fail()

    def add_nonpoint(self):
        self.assertRaises(TypeError, addPoint, "a;lskdjf")

if __name__ == '__main__':
    unittest.main(verbosity = 2)
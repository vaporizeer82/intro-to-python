#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt


# ---------------------------- Class Example (walls) ---------------------------------
#
# It is customary to put each class in it's own file with the file name matching the class name
#   - at least for classes that are of any decent size or complexity
#
# TODO (Lecture activity) This is the wall class from the tutorial. Extend it to do two things
#  - Reflect off of the wall [vertical/horizontal, general is optional]
#  - Plot self (optional, but you'll need it for the homework
#
#  Notice the method named collided - we'll see why when you do the homework
class PinballWall:
    def __init__(self, wall_type="Vertical", intercept_value=None, a_b_c=None):
        """Create a horizontal, vertical, or general half-plane wall
         if vertical/horizontal, use intercept value
         if general, use a_b_c
        @param wall_type - Vertical, Horizontal or General (ax + by + c = 0)
        @param intercept_value - number, where to place the wall on the x or y axis (Vertical or Horizontal)
        @param a_b_c - for General wall, list of 3 values"""

        # Save what kind of wall it is, then save as a general ax + by + c equation
        # What this does:
        #  - checks that the input is at least somewhat correct - eg, if they asked for a vertical wall they
        #    need to specify the intercept value
        # Lets the user "special case" a vertical or horizontal wall, rather than having to convert to the
        #   ax + by + c format
        if wall_type == "Vertical":
            self.wall_type = "Vertical"
            if intercept_value is None:
                raise ValueError("If vertical wall, need to specify intercept value")
            if intercept_value > 0:
                self.abc = [1.0, 0.0, -intercept_value]
            else:
                self.abc = [-1.0, 0.0, intercept_value]
        elif wall_type == "Horizontal":
            self.wall_type = "Horizontal"
            if intercept_value is None:
                raise ValueError("If horizontal wall, need to specify intercept value")
            if intercept_value > 0:
                self.abc = [0.0, 1.0, -intercept_value]
            else:
                self.abc = [0.0, -1.0, intercept_value]
        elif wall_type == "General":
            self.wall_type = "General"
            if a_b_c is None:
                raise ValueError("If general wall, need to specify a, b, and c a_b_c")
            if a_b_c[0] * 0 + a_b_c[1] * 0 + a_b_c[2] < 0:
                self.abc = [a_b_c[0], a_b_c[1], a_b_c[2]]
            else:
                self.abc = [-a_b_c[0], -a_b_c[1], -a_b_c[2]]
        else:
            raise ValueError("Wall type is not one of Vertical, Horizontal, or General")

    def evaluate_halfplane(self, x_y):
        """ Evaluate ax + by + c
        @param x_y numpy array/tuple for the current location
        @return ax + by + c"""
        # self is the same self we set in the __init__ function
        return self.abc[0] * x_y[0] + self.abc[1] * x_y[1] + self.abc[2]

    def outside(self, x_y):
        """ Is the point outside of this half/plane representing the wall?
        @param x_y numpy array/tuple for the current location
        @return True or False"""
        # We can call any of the methods defined in the class
        return self.evaluate_halfplane(x_y) >= 0.0

    def inside(self, x_y):
        """ We can call class methods from within class methods
        @param x_y numpy array/tuple for the current location
        @return True or False"""
        return not self.outside(x_y)

    def collided(self, x_y):
        """Has the pinball passed through the wall?
        @param x_y numpy array/tuple for the current location
        @return True or False"""
        return self.outside(x_y)

    def reflect(self, x_y, vx_vy):
        """ Reflect off of the wall
        @param x_y - the point
        @param vx_vy - the current velocity
        @return pt, vec - reflected point and vector"""
        # TODO - put your wall reflection code here
        #  Remember that you have what type of wall (vertical/horizontal) stored
        #  This should just be moving the code you did last week into this method
        if self.wall_type == "Vertical":
            vx_vy[1] *= -1
        if self.wall_type == "Horizontal":
            vx_vy[0] *= -1
        return x_y, vx_vy
        

    def __str__(self):
        """
        The string representation of this class.
        :return: a string
        """
        # f"" creates a string; in this case, instead of printing it, just return it
        return f"{self.abc[0]:0.3f}x + {self.abc[1]:0.3f}y + {self.abc[2]:0.3f}"

    def test_on_wall(self, x_y):
        """ Pass in a point that should be on the wall; should return 0
         @param x_y - xy point
         @preturn True or False"""
        return np.isclose(self.evaluate_halfplane(x_y), 0.0)

    def test_origin_inside(self):
        """ Test that we correctly oriented the wall so that (0,0) is inside"""
        # Notice call to self
        assert(self.inside([0, 0]))
        return True

    def test_reflect(self):
        """ Put any tests for reflection here"""
        return True

    def plot(self, axs, left, right, height):
        """ Plot the wall inside the box defined by left, right, 0-height
        @param axs - the plot axes
        @param left left side of box
        @param right right side of box
        @param height height of box"""
        # TODO: Add plot code
        #   Note - you're free to save more information in the class init function to make this easier (eg, the y
        #    intercept)
        # Plotting the general wall is optional
        #   Hint: get x values between the left and right walls, solve for y = (-ax -c)/b and keep only the points
        #    inside the box
        #   Hint: you can use the plt.plot function to plot the line
        #   Hint: you can use the plt.fill_between function to fill the area
        #   Hint: you can use the plt.scatter function to plot the intercept
        axs.plot([left, right], [0, 0], 'k')
        axs.plot([left, right], [height, height], 'k')
        axs.plot([left,left], [0,height], 'b')
        axs.plot([right,right], [0,height], 'r')
        axs.fill_between([left, right], [0, 0], [height, height], color='lightgrey')
        return axs



if __name__ == '__main__':
    # TODO Create instances and call test functions
    pw = ...

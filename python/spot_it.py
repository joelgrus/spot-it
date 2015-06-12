# this is code to generate "Spot It!" cards
# using the finite projective plane.
# 
# The finite projective plane of order n (where n is prime)
# contains n^2 + n + 1 points and n^2 + n + 1 lines.
# Each line goes through n + 1 points
# Each point lies on n + 1 lines
# Any two lines intersect in exactly one point
#
# Now think:
#  point = picture
#  line = card

# for more details, see the blog post:
#
#

#
# POINTS
#

def ordinary_points(n):
    """ordinary points are just pairs (x, y) where x and y
    are both between 0 and n - 1"""
    return [(x, y) for x in range(n) for y in range(n)]
    
def points_at_infinity(n):
    """infinite points are just the numbers 0 to n - 1
    (corresponding to the infinity where lines with that slope meet)
    and infinity infinity (where vertical lines meet)"""
    return range(n) + [u"∞"]

def all_points(n):
    return ordinary_points(n) + points_at_infinity(n)

def ordinary_line(m, b, n):
    """returns the ordinary line through (0, b) with slope m
    in the finite projective plan of degree n
    includes 'infinity m'"""
    return [(x, (m * x + b) % n) for x in range(n)] + [m]

def vertical_line(x, n):
    """returns the vertical line with the specified x-coordinate
    in the finite projective plane of degree n
    includes 'infinity infinity'"""
    return [(x, y) for y in range(n)] + [u"∞"]
    
def line_at_infinity(n):
    """the line at infinity just contains the points at infinity"""
    return points_at_infinity(n)

def all_lines(n):
    return ([ordinary_line(m, b, n) for m in range(n) for b in range(n)] +
            [vertical_line(x, n) for x in range(n)] +
            [line_at_infinity(n)])

import random

# get a list of animals we can use to name the cards
with open("animals.txt", "r") as f:
    animals = [line.strip() for line in f]

def make_deck(n, pics=None):
    points = all_points(n)

    # if no pics specified, start with empty list
    if pics is None: pics = []

    # if fewer pics than points, add indexes
    if len(pics) < len(points):
        pics.extend(range(len(pics), len(points)))

    # then take a random sample        
    pics = random.sample(pics, len(points))

    # create a mapping from point to pic
    mapping = { point : pic 
                for point, pic in zip(points, pics) }

    # and return the remapped cards
    return [map(mapping.get, line) for line in all_lines(n)]
    
def play_game(deck):
    # make a copy so as not to much with the original deck,
    # and then shuffle it
    deck = deck[:]
    random.shuffle(deck)

    # keep playing until fewer than 2 cards are left    
    while len(deck) >= 2:
        card1 = deck.pop()
        card2 = deck.pop()
        random.shuffle(card1)
        random.shuffle(card2)

        # find the matching element
        match, = [pic for pic in card1 if pic in card2]  

        print card1
        print card2
        
        guess = raw_input("Match? ")
        if guess == match:
            print "correct!"
        else:
            print "incorrect!"

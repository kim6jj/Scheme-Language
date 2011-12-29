#!/usr/bin/python
import sys
import urllib
import xml.parsers.expat

# Joung Kim - CS430 PA1 Python
# This function takes a file name, opens it, and reads the cities on each line to form a query
# to Google Maps to obtain a distance matrix. It assumes that the file is in an appropriate format.

def make_distance_query(file_name):
    """ Read the cities in a file and use them to create a distance matrix query for the Google maps API
        - the file should be text with one city per line """
    try:
        file = open(file_name)
        cities = ""
        allcities = "(define cities (list "
        for line in file:
            line = line.strip()
            allcities = allcities + "'" + line + " "
            line = line.replace(' ', '+')
            line = line.replace(',', '')
            cities += line + '|'
        allcities = allcities.strip() + "))\n"
        print allcities
        cities = cities[0:-1]
        header = "http://maps.googleapis.com/maps/api/distancematrix/xml?"
        return header + "origins=" + cities + "&destinations=" + cities + "&sensor=false"
        file.close
    except IOError:
        return None

# This class transforms an xml string into a dictionary of dictionaries of distances in meters
# between cities. It assumes the xml string is in the format produced by the Google maps API for
# a distance matrix.

class Distances:
    # instance variables for parsing and building the distance matrix
    __parser = None            # the expat parser object
    __origins = []             # the origin cities (rows) in the matrix
    __destinations = []        # the destination cities (columns) in the matrix
    __tag = ''                 # the current xml tag of interest
    __row = 0                  # origin cities counter for making dictionaries
    __col = 0                  # destination cities counter for making dictionaries
    __distances = {}           # dictionary of origin cities whose values are dictionaries
                               # of destination cities with distances in meters

    def __init__(self, xml_string):
        """ Create the expat parser and call it to parse the xml_string parameter """
        self.__parser = xml.parsers.expat.ParserCreate()
        self.__parser.StartElementHandler = self.start_element
        self.__parser.EndElementHandler = self.end_element
        self.__parser.CharacterDataHandler = self.char_data
        self.__parser.Parse(xml_string, True)

    def start_element(self, name, attributes):
        """ Parser callback when an element is found
             - the record tag is needed to distinguish between value elements for times and distances """
        if name == 'value' and self.__tag == 'distance':
             self.__tag = 'record'
        elif name == 'row':
             self.__col = 0
             self.__distances[self.__origins[self.__row]] = {}
        else:
             self.__tag = name

    def end_element(self, name):
        """" Parser callback when an element ends
             - we only need to increment the row at the end of a row element """
        if name == 'row':
            self.__row += 1

    def char_data(self, data):
        """ Parser callback for the text of an element
            - the only elements we are interested in are the origin and destination cities
              for building the origin and desination lists, and the distances in the body of
              the matrix
            - whitespace is treated as data and must be filtered out
            - if the matrix contains elements for the distance from a city to itself, this is
              also filtered out """
        if data.isspace():
            return
        if self.__tag == 'origin_address':
            self.__origins.append(data.partition(',')[0])
        elif self.__tag == 'destination_address':
            self.__destinations.append(data.partition(',')[0])
        elif self.__tag == 'record':
            if data != '0':
                origin_map = self.__distances[self.__origins[self.__row]]
                origin_map[self.__destinations[self.__col]] = int(data)
            self.__col += 1

    def display(self):
        """ Print the distances disctionary with distances in kilometers """
        distance_matrix = "(define distance-matrix (list"
        for source, targets in self.__distances.iteritems():
            for target, meters in targets.iteritems():
                distance_matrix = distance_matrix + "\n    (list '" + source + " '" + target + " " + str(meters/1000) + ")"
        distance_matrix = distance_matrix + "))\n"
        print distance_matrix

# end Distances class

# main script
if len(sys.argv) == 1:
    print "Missing cities file."
else:
    print "\n#lang racket"
    print "\n(provide cities distance-matrix)\n"
    query = make_distance_query(sys.argv[1])         # make a Google maps distance matrix query
    distances_xml = urllib.urlopen(query).read()     # goto google maps and submit the query
    distances = Distances(distances_xml)             # parse query result and make a distance matrix
    distances.display()                              # display the data in the distance matrix

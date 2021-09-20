#include "Node.h"

#include <cmath>

// ------- constructors -------
Node::Node() {
    children = std::vector<std::shared_ptr<Node>>(4);
    neighbors = std::vector<std::weak_ptr<Node>>();
}

Node::Node(double _xMin, double _xMax, double _yMin, double _yMax, double _value, int _id, int _level)
    : Node{} {
    xMin = _xMin;
    xMax = _xMax;
    yMin = _yMin;
    yMax = _yMax;
    value = _value;
    id = _id;
    level = _level;

    smallestChildSideLength = xMax-xMin;
}

Node::Node(double _xMin, double _xMax, double _yMin, double _yMax, double _value, int _id, int _level, double _smallestChildSideLength, bool _hasChildren)
    : Node{_xMin, _xMax, _yMin, _yMax, _value, _id, _level} {
    smallestChildSideLength = _smallestChildSideLength;
    hasChildren = _hasChildren;
}

// ------- getChildIndex -------
// given an x and a y coordinate, returns the index of the child that contains
// the point. Based on the assumption that the first element is lower left corner. 
// Indexing then proceeds by row. Returns -1 if point is outside the node.
int Node::getChildIndex(const Point pt) const {
    if( (pt.x < xMin) | (pt.x > xMax) | (pt.y < yMin) | (pt.y > yMax) ){ // check to make sure the point falls within our extent
        return -1; // if not, return -1
    }
    int col = (pt.x < (xMin + xMax)/2) ? 0 : 1; 
    int row = (pt.y < (yMin + yMax)/2) ? 0 : 1;
    int index = row*2 + col;
    return index;
}

// ------- toString -------
std::string Node::toString() const{
    std::string str = "x: [" + std::to_string(xMin) + ", " + std::to_string(xMax) + "] | y: [" + std::to_string(yMin) + ", " + std::to_string(yMax) + "]";
    str = str + " | value: " + std::to_string(value) + " | hasChildren: " + std::to_string(hasChildren) + " | smallestChildSideLength: " +
        std::to_string(smallestChildSideLength)+ " | size(children): " + std::to_string(children.size()) + " | size(neighbors): " + std::to_string(neighbors.size()) +
        " | level: " + std::to_string(level) + " | id: " + std::to_string(id);
    return str;
}

#!/usr/bin/python
# -*- coding: utf-8 -*-

import math
import cmath
import weakref
import cairo
from gi.repository import Pango
from gi.repository import PangoCairo
from gi.repository import Gtk
from gi.repository import Gdk


EPSILON = 10
NARY_DIAMOND_RADIUS = 10
ANCHOR_RADIUS = 5
DIAMOND_SIZE = 10
DIAMOND_ANGLE = math.pi / 6
TRIANGLE_SIZE = 10
TRIANGLE_ANGLE = math.pi / 6
ARROW_SIZE = 10
ARROW_ANGLE = math.pi / 6
DASH_SIZE = 5
COMPARTMENT_SEPARATOR = u'\u2029'.encode('utf-8')


def projection(a, b, c):
    l2 = abs(b - a) ** 2
    if l2 == 0.0:
        return a
    t = ((c - a) * (b - a).conjugate()).real / l2
    if t < 0.0:
        return a
    if t > 1.0:
        return b
    return a + t * (b - a)


def distance(a, b, c):
    return abs(c - projection(a, b, c))


def snap_coord(x):
    return 0.5 + int(x)


def snap_point(p):
    return complex(snap_coord(p.real), snap_coord(p.imag))


class View:
    def __init__(self):
        pass

    def draw(self, context):
        pass

    def wants(self, x, y):
        return False

    def press(self, window, event):
        pass

    def new_compartment(self):
        return False


class Node:

    class Kind:
        (CLASS,
         INTERFACE,
         PACKAGE,
         NODE,
         COMPONENT,
         ACTOR,
         USECASE) = range(7)

    def __init__(self):
        self.compartments = [Compartment(self)]
        self.kind = Node.Kind.CLASS

    def new_compartment_after(self, compartment, text):
        new_compartment = Compartment(self, text)
        self.compartments.insert(self.compartments.index(compartment) + 1, new_compartment)
        return new_compartment

    def join_compartments(self, first, second):
        first.text += second.text
        self.compartments.remove(second)


class Compartment:
    def __init__(self, node, text=''):
        self.node = weakref.ref(node)
        self.text = text


class EdgeEnd:
    class Aggregation:
        (NONE,
         AGGREGATE,
         COMPOSITE) = range(3)

    class Kind:
        (ASSOCIATION,
         ASSOCIATION_CLASS,
         GENERALIZATION,
         REALIZATION,
         DEPENDENCY,
         NESTING) = range(6)

    def __init__(self, node):
        self.node = node
        self.kind = EdgeEnd.Kind.ASSOCIATION
        self.aggregation = EdgeEnd.Aggregation.NONE
        self.navigable = False
        self.caption = None

class Edge:
    def __init__(self, nodes):
        self.ends = [EdgeEnd(node) for node in nodes]


class CompartmentView:
    def __init__(self, node_view, compartment):
        self.node_view = weakref.ref(node_view)
        self.compartment = compartment
        self.h = 16
        self.y = 0 # relative to node_view

    def new_compartment(self, window, top_text, bottom_text):
        window.finish_edit(top_text)
        self.compartment.text = top_text
        self.node_view().new_compartment_after(self, bottom_text).edit(window, None)
        buf = window.entry.get_buffer()
        buf.place_cursor(buf.get_start_iter())
        return True

    def next(self):
        index = self.node_view().compartments.index(self)
        return self.node_view().compartments[index + 1] if index != len(self.node_view().compartments) else None

    def previous(self):
        index = self.node_view().compartments.index(self)
        return self.node_view().compartments[index - 1] if index != 0 else None


class NodeView(View):
    def __init__(self, diagram, node, cx, cy):
        View.__init__(self)
        self.diagram = weakref.ref(diagram)
        self.node = node
        self.cx = snap_coord(cx)
        self.cy = snap_coord(cy)
        self.is_new = True
        self.changed = DoNothing

    def clamp(self, point):
        "Return the point of this view's border closest to given point"
        pass

    def wants(self, x, y):
        "Return true if (x, y) is within EPSILON from this view"
        pass

    def distance(self, x, y):
        "Return the distance from (x, y) to the closest point of this view"
        pass

    def press(self, window, event):
        if event.button == 3: # right button: pop up contex menu
            print 'Node popup menu'
            window.context = self
            window.node_popup.popup(None, None, None, None, 3, event.time)
        elif event.button == 2: # middle button: initiate node move (with all incident edges)
            print 'Move node'
            self.drag_point = complex(event.x - self.cx, event.y - self.cy)
            views = [self]
            for edge_view in self.diagram().views:
                if isinstance(edge_view, EdgeView) and edge_view.grab_node(self, event):
                    views.append(edge_view)
            window.grab(views)
        elif event.button == 1: # left button: start an edge or edit node
            print 'Create edge'
            drag_point = complex(event.x, event.y)
            start_point = self.clamp(drag_point)
            edge_view = self.diagram().new_edge_view([self.node, None], [start_point, drag_point])
            edge_view.grace_node_view = self
            window.grab([edge_view])
            edge_view.grab(EdgeView.DragCase.BINARY_END, [(0, 1)], event)

    def edit(self, window, event):
        "Start editing"
        pass

    def edited_text(self, window, text):
        "Called after the user finishes editing. Parse the text and modify the model. If needed, reroute edges"
        pass

    def edited(self, window, text, context=None):
        if not text and self.is_new:
            print 'Cancel node'
            self.diagram().model().delete_node(self.node)
        else:
            self.edited_text(window, text)
            self.changed(self)

    def delete(self):
        print 'Delete node'
        self.diagram().model().delete_node(self.node)

    def motion(self, window, event):
        self.cx = event.x - self.drag_point.real
        self.cy = event.y - self.drag_point.imag
        self.changed(self)

    def release(self, window, event):
        print 'Moved node'
        self.cx = snap_coord(event.x - self.drag_point.real)
        self.cy = snap_coord(event.y - self.drag_point.imag)
        self.changed(self)
        window.ungrab()
        self.drag_start = None
        self.drag_start_event = None

    def draw(self, context):
        "Draw this view"
        pass


class ClassifierNodeView(NodeView):
    def __init__(self, diagram, node, cx, cy, w, h):
        NodeView.__init__(self, diagram, node, cx, cy)
        self.w = int(w)
        self.h = int(h)
        self.compartments = [CompartmentView(self, c) for c in node.compartments]

    def corners(self):
        top, bottom, left, right = [self.cy - self.h / 2, self.cy + self.h / 2, self.cx - self.w / 2, self.cx + self.w / 2]
        return [complex(left, top), complex(left, bottom), complex(right, bottom), complex(right, top), complex(left, top)]

    def sides(self):
        corners = self.corners()
        return zip(corners[:-1], corners[1:])

    def clamp(self, point):
        projections = [projection(a, b, point) for a, b in self.sides()]
        return snap_point(min(projections, key=lambda(p): abs(p - point)))

    def within(self, epsilon, x, y):
        return (self.cx - self.w / 2 - epsilon <= x < self.cx + self.w / 2 + epsilon
                and self.cy - self.h / 2 - epsilon <= y < self.cy + self.h / 2 + epsilon)

    def wants(self, x, y):
        return self.within(EPSILON, x, y)

    def distance(self, x, y):
        p = complex(x, y)
        return (0 if self.within(0, x, y)
                else min(distance(a, b, p)
                         for a, b in self.sides()))

    def edit(self, window, event):
        print 'Edit node'
        window.edit(self, COMPARTMENT_SEPARATOR.join(c.compartment.text for c in self.compartments),
                    self.cx - self.w / 2 + 1, self.cy - self.h / 2 + 1, self.w - 2, self.h - 2)
        if event:
            x, y = event.x - (self.cx - self.w / 2), event.y - (self.cy - self.h / 2)
            bx, by = window.entry.window_to_buffer_coords(Gtk.TextWindowType.TEXT, x, y)
            it = window.entry.get_iter_at_location(bx, by)
            window.entry.get_buffer().place_cursor(it)

    def edited_text(self, window, text):
        print 'Edited node'
        ts = text.split(COMPARTMENT_SEPARATOR)

        for c, t in zip(self.compartments, ts):
            c.compartment.text = t
        if len(ts) < len(self.compartments):
            del self.compartments[len(ts):]
        elif len(ts) > len(self.compartments):
            for t in ts[len(self.compartments):]:
                compartment = self.node.new_compartment_after(self.node.compartments[-1], t)
                self.compartments.append(CompartmentView(self, compartment))

        self.is_new = False
        self.resize(window)

    def resize(self, window):
        layout = Pango.Layout(window.get_pango_context())
        layout.set_font_description(Pango.FontDescription('sans 10'))

        width, height = 0, 0
        for c in self.compartments:
            layout.set_indent(-16384)
            layout.set_text(c.compartment.text, -1)
            ink_r, logical_r = layout.get_pixel_extents()
            w, h = 4 + abs(ink_r.x) + ink_r.width, 6 + logical_r.height
            if height != c.y:
                c.y = height
            if h != c.h:
                c.h = h
            width = max(width, w)
            height += h

        if width != self.w or height != self.h:
            for e in self.diagram().views:
                if isinstance(e, EdgeView) and self.node in [end.node for end in e.edge.ends]:
                    e.node_resizing(self, width, height)
            self.w = width
            self.h = height

    def draw(self, context):
        context.set_source_rgb(0, 0, 0)
        context.set_line_width(1)
        context.rectangle(self.cx - self.w / 2, self.cy - self.h / 2, self.w, self.h)

        for c in self.compartments[1:]:
            context.move_to(self.cx - self.w / 2, self.cy - self.h / 2 + c.y)
            context.rel_line_to(self.w, 0)

        context.stroke()

        context.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        layout = PangoCairo.create_layout(context)
        font = Pango.FontDescription('sans 10')
        layout.set_font_description(font)

        for c in self.compartments:
            layout.set_indent(-16384)
            layout.set_text(c.compartment.text, -1)
            PangoCairo.update_layout(context, layout)
            context.translate(self.cx - self.w / 2 + 2, self.cy - self.h / 2 + c.y + 3)
            PangoCairo.show_layout(context, layout)
            context.identity_matrix()


class EdgeView(View):
    def __init__(self, diagram, edge, path):
        View.__init__(self)
        self.diagram = weakref.ref(diagram)
        self.edge = edge
        self.paths = [path] # If two ends, contains one path. If N-ary, contains a path from diamond to each end.
        self.diamond = None # complex(x, y) of the center of the diamond (if any)
        self.changed = DoNothing

    def in_diamond(self, p):
        return (self.diamond
                and abs(p.real - self.diamond.real) + abs(p.imag - self.diamond.imag) < NARY_DIAMOND_RADIUS)

    def wants(self, x, y):
        return EPSILON > self.distance(x, y)

    def distance(self, x, y):
        p = complex(x, y)
        return 0 if self.in_diamond(p) else min(distance(a, b, p)
                                                for path in self.paths
                                                for a, b in zip(path[:-1], path[1:]))

    class DragCase:
        (NONE,
         BINARY_WHOLE,
         BINARY_START_NODE,
         BINARY_END_NODE,
         NARY_END_NODE,
         DIAMOND,
         BINARY_START,
         BINARY_END,
         NARY_END,
         DIAMOND_CONNECTION,
         INNER_VERTEX) = range(11)

    def grab(self, drag_case, points, event):
        self.drag_case = drag_case
        self.grabbed = points
        self.drag_points = ([complex(event.x, event.y) - self.paths[path_index][vertex_index] for path_index, vertex_index in points]
                            + ([complex(event.x, event.y) - self.diamond] if self.drag_case == EdgeView.DragCase.DIAMOND else []))

    # When a node is grabbed, its incident edges must move with it.
    def grab_node(self, node_view, event):
        # Node is unrelated to this edge. Grab nothing
        if node_view.node not in [end.node for end in self.edge.ends]:
            return False
        # Edge is binary, both ends at the given node. Grab all of the edge
        if [node_view.node] * 2 == [end.node for end in self.edge.ends]:
            self.grab(EdgeView.DragCase.BINARY_WHOLE, [(0, i) for i in range(len(self.paths[0]))], event)
        # Edge is binary, starting at the given node. Grab start of the path
        elif not self.diamond and node_view.node is self.edge.ends[0].node:
            self.grab(EdgeView.DragCase.BINARY_START_NODE, [(0, 0)], event)
        # Edge is binary, ending at the given node. Grab end of the path
        elif not self.diamond and node_view.node is self.edge.ends[1].node:
            self.grab(EdgeView.DragCase.BINARY_END_NODE, [(0, len(self.paths[0]) - 1)], event)
        # Otherwise, grab end of each path to node
        else:
            self.grab(EdgeView.DragCase.NARY_END_NODE,
                      [(i, len(self.paths[i]) - 1)
                       for i in range(len(self.edge.ends))
                       if self.edge.ends[i].node is node_view.node],
                      event)
        return True

    def grab_diamond(self, event):
        points = [(i, 0) for i, path in enumerate(self.paths)]
        self.grab(EdgeView.DragCase.DIAMOND, points, event)

    class HitTest:
        class Vertex:
            def __init__(self, path_index, vertex_index):
                self.path_index = path_index
                self.vertex_index = vertex_index
        class End(Vertex):
            def __init__(self, path_index, vertex_index, end_index):
                EdgeView.HitTest.Vertex.__init__(self, path_index, vertex_index)
                self.end_index = end_index
        class Segment:
            def __init__(self, path_index, vertex_index):
                self.path_index = path_index
                self.vertex_index = vertex_index
        class Diamond:
            def __init__(self):
                pass

    def hittest(self, point, excluded_path_index=None):
        if self.in_diamond(point):
            return EdgeView.HitTest.Diamond()
        diamond_points = [self.diamond + complex(NARY_DIAMOND_RADIUS, 0),
                          self.diamond + complex(0, NARY_DIAMOND_RADIUS),
                          self.diamond + complex(-NARY_DIAMOND_RADIUS, 0),
                          self.diamond + complex(0, -NARY_DIAMOND_RADIUS),
                          self.diamond + complex(NARY_DIAMOND_RADIUS, 0)] if self.diamond else None
        for path_index, path in enumerate(self.paths):
            if path_index == excluded_path_index:
                continue
            for i in range(len(path)):
                if abs(point - path[i]) < EPSILON: # on a vertex: move vertex
                    if i == len(path) - 1 or i == 0 and not self.diamond:
                        return EdgeView.HitTest.End(path_index, i, path_index if self.diamond or i == 0 else 1)
                    else:
                        return EdgeView.HitTest.Vertex(path_index, i)
            if (self.diamond
                and distance(path[0], path[1], point) < EPSILON
                and min(distance(a, b, point) for a, b in zip(diamond_points[:-1], diamond_points[1:])) < EPSILON):
                return EdgeView.HitTest.Vertex(path_index, 0)
            for i in range(len(path) - 1):
                if distance(path[i], path[i + 1], point) < EPSILON: # on a segment: add vertex
                    return EdgeView.HitTest.Segment(path_index, i)

    def snap(self, point):
        hittest = self.hittest(point)
        if isinstance(hittest, EdgeView.HitTest.Diamond):
            return self.diamond
        elif isinstance(hittest, EdgeView.HitTest.Segment):
            return projection(self.paths[hittest.path_index][hittest.vertex_index],
                              self.paths[hittest.path_index][hittest.vertex_index + 1],
                              point)
        else:
            return self.paths[hittest.path_index][hittest.vertex_index]

    def set_aggregation(self, end, value):
        print 'Set aggregation at', end, 'to', value
        self.edge.ends[end].aggregation = value
        self.changed(self)

    def set_navigability(self, end, value):
        print 'Set navigability at', end, 'to', value
        self.edge.ends[end].navigable = value
        self.changed(self)

    def set_kind(self, end, value):
        print 'Set kind at', end, 'to', value
        self.edge.ends[end].kind = value
        self.changed(self)

    def press(self, window, event):
        if window.grabbed:
            return
        if event.button == 1:
            hittest = self.hittest(complex(event.x, event.y))
            if isinstance(hittest, EdgeView.HitTest.End):
                print 'Edit end'
                end = self.paths[hittest.path_index][hittest.vertex_index]
                caption = self.edge.ends[hittest.end_index].caption or ''
                window.edit(self, caption, end.real, end.imag, 80, 22, hittest)
                buf = window.entry.get_buffer()
                buf.place_cursor(buf.get_end_iter())
                return
            else:
                print 'Add path'
                point = complex(event.x, event.y)
                self.ensure_diamond(point)
                self.edge.ends.append(EdgeEnd(None))
                self.paths.append([self.diamond, point])
                self.grab(EdgeView.DragCase.NARY_END, [(len(self.paths) - 1, 1)], event)
                window.grab([self])
                return
        elif event.button == 2:
            point = complex(event.x, event.y)
            hittest = self.hittest(point)
            if isinstance(hittest, EdgeView.HitTest.Vertex):
                print 'Move vertex'
                if self.diamond and hittest.vertex_index == 0:
                    self.paths[hittest.path_index][hittest.vertex_index] = complex(event.x, event.y)
                self.grab(EdgeView.DragCase.BINARY_START if not self.diamond and hittest.vertex_index == 0
                          else EdgeView.DragCase.BINARY_END if not self.diamond and hittest.vertex_index == len(self.paths[0]) - 1
                          else EdgeView.DragCase.NARY_END if hittest.vertex_index == len(self.paths[hittest.path_index]) - 1
                          else EdgeView.DragCase.DIAMOND_CONNECTION if hittest.vertex_index == 0
                          else EdgeView.DragCase.INNER_VERTEX,
                          [(hittest.path_index, hittest.vertex_index)], event)
                window.grab([self])
                return
            if isinstance(hittest, EdgeView.HitTest.Segment):
                print 'Add vertex'
                self.paths[hittest.path_index].insert(hittest.vertex_index + 1, point)
                self.grab(EdgeView.DragCase.INNER_VERTEX, [(hittest.path_index, hittest.vertex_index + 1)], event)
                window.grab([self])
                return
            if isinstance(hittest, EdgeView.HitTest.Diamond):
                print 'Move diamond'
                self.grab_diamond(event)
                window.grab([self])
                return
        elif event.button == 3:
            hittest = self.hittest(complex(event.x, event.y))
            if isinstance(hittest, EdgeView.HitTest.End):
                print 'End context menu'
                aggregation = self.edge.ends[hittest.end_index].aggregation
                window.aggregate_action.set_active(aggregation == EdgeEnd.Aggregation.AGGREGATE)
                window.composite_action.set_active(aggregation == EdgeEnd.Aggregation.COMPOSITE)
                window.navigability_action.set_active(self.edge.ends[hittest.end_index].navigable)
                kind = self.edge.ends[hittest.end_index].kind
                window.generalization_action.set_active(kind == EdgeEnd.Kind.GENERALIZATION)
                window.realization_action.set_active(kind == EdgeEnd.Kind.REALIZATION)
                window.dependency_action.set_active(kind == EdgeEnd.Kind.DEPENDENCY)
                window.scope_action.set_active(kind == EdgeEnd.Kind.NESTING)
                window.association_class_action.set_active(kind == EdgeEnd.Kind.ASSOCIATION_CLASS)
                window.association_class_action.set_visible(self.diamond is not None)
                window.edge_path_delete_action.set_visible(self.diamond is not None)
                window.context = (self, hittest)
                window.edge_end_popup.popup(None, None, None, None, 3, event.time)
            elif isinstance(hittest, EdgeView.HitTest.Diamond) or not self.diamond:
                print 'Edge context menu'
                window.edge_path_delete_action.set_visible(False)
                window.context = (self, hittest)
                window.edge_popup.popup(None, None, None, None, 3, event.time)
            else:
                print 'Path context menu'
                window.edge_path_delete_action.set_visible(True)
                window.context = (self, hittest)
                window.edge_popup.popup(None, None, None, None, 3, event.time)
            return

    def edited(self, window, text, hittest):
        print 'Edited end'
        self.edge.ends[hittest.end_index].caption = text
        self.changed(self)

    def delete(self):
        print 'Delete edge'
        self.diagram().model().delete_edge(self.edge)

    def delete_path(self, path_index):
        assert self.diamond
        self.paths.pop(path_index)
        self.edge.ends.pop(path_index)
        self.check_diamond_necessity()
        self.changed(self)

    def delete_paths(self, node):
        if not self.diamond and node in (end.node for end in self.edge.ends):
            self.delete()
        else:
            for path, end in zip(self.paths[:], self.edge.ends[:]):
                if end.node is node:
                    print 'Remove path'
                    self.edge.ends.remove(end)
                    self.paths.remove(path)
            if len(self.edge.ends) < 2:
                self.delete()
            self.check_diamond_necessity()
            self.changed(self)

    def motion(self, window, event):
        if self.grabbed is not None:
            for (path_index, vertex_index), drag_point in zip(self.grabbed, self.drag_points):
                self.paths[path_index][vertex_index] = complex(event.x, event.y) - drag_point
            if self.drag_case == EdgeView.DragCase.DIAMOND:
                self.diamond = complex(event.x, event.y) - self.drag_points[-1]
            self.changed(self)

    def ensure_diamond(self, point):
        if not self.diamond:
            print 'Create diamond'
            # Create a diamond at the attachment point
            hittest = self.hittest(point)
            self.diamond = snap_point(self.snap(point))
            if isinstance(hittest, EdgeView.HitTest.Segment):
                self.paths = [[self.diamond] + list(reversed(self.paths[0][:hittest.vertex_index + 1])),
                              [self.diamond] + self.paths[0][hittest.vertex_index + 1:]]
            else: # put diamond instead of vertex
                assert not isinstance(hittest, EdgeView.HitTest.End)
                self.paths = [[self.diamond] + list(reversed(self.paths[0][:hittest.vertex_index])),
                              [self.diamond] + self.paths[0][hittest.vertex_index + 1:]]

    def check_diamond_necessity(self):
        if self.diamond and len(self.edge.ends) == 2:
            self.paths = ([list(reversed(self.paths[0][1:]))
                           + ([self.diamond] if distance(self.paths[0][1], self.paths[1][1], self.diamond) >= EPSILON else [])
                           + self.paths[1][1:]])
            self.diamond = None

    def release(self, window, event):
        points = [complex(event.x, event.y) - point for point in self.drag_points]
        if self.drag_case in (EdgeView.DragCase.BINARY_WHOLE,
                              EdgeView.DragCase.BINARY_START_NODE,
                              EdgeView.DragCase.BINARY_END_NODE,
                              EdgeView.DragCase.NARY_END_NODE,
                              EdgeView.DragCase.DIAMOND): # No topology change, just snap points to grid
            print 'Moved edge'
            for (path_index, vertex_index), p in zip(self.grabbed, points):
                self.paths[path_index][vertex_index] = snap_point(p)
            self.changed(self)
            self.grabbed = None
            window.ungrab()
        elif self.drag_case in (EdgeView.DragCase.BINARY_START,
                                EdgeView.DragCase.BINARY_END,
                                EdgeView.DragCase.NARY_END):
            point = points[0]
            path_index, vertex_index = self.grabbed[0]
            assert path_index == 0 or self.drag_case == EdgeView.DragCase.NARY_END
            assert vertex_index == (0 if self.drag_case == EdgeView.DragCase.BINARY_START else len(self.paths[path_index]) - 1)
            next_vertex, vertex_after = (1, 2) if vertex_index == 0 else (-2, -3)
            end_index = (0 if self.drag_case == EdgeView.DragCase.BINARY_START
                         else 1 if self.drag_case == EdgeView.DragCase.BINARY_END
                         else path_index)
            # Binary edge end can reattach to another node or a different edge. N-ary edge end can only reattach to a node
            attach_to = (self.diagram().attachable_at(event.x, event.y, self)
                         if self.drag_case != EdgeView.DragCase.NARY_END
                         else self.diagram().node_view_at(event.x, event.y))
            if attach_to is self.grace_node_view and self.grace_node_view is not None:
                self.delete()
                window.ungrab()
                self.grace_node_view.edit(window, event)
                return
            if isinstance(attach_to, NodeView): # at a node: (re)attach to it
                self.grace_node_view = None
                self.edge.ends[end_index].node = attach_to.node
                self.paths[path_index][vertex_index] = snap_point(attach_to.clamp(point))
                self.changed(self)
                self.grabbed = None
                window.ungrab()
            elif isinstance(attach_to, EdgeView):
                print 'Attach to edge'
                attach_to.ensure_diamond(point)
                attach_to.edge.ends.append(self.edge.ends[1 - end_index])
                attach_to.paths.append(self.paths[0] if self.drag_case == EdgeView.DragCase.BINARY_START else list(reversed(self.paths[0])))
                attach_to.paths[-1][0] = attach_to.diamond
                attach_to.changed(self)
                print 'Delete old edge'
                self.delete()
                window.ungrab()
            elif (len(self.paths[path_index]) > 2
                  and distance(self.paths[path_index][next_vertex],
                               self.paths[path_index][vertex_after], point) < EPSILON):
                print 'Delete segment'
                self.paths[path_index].pop(next_vertex)
                if vertex_index != 0:
                    self.grabbed[0] = (path_index, vertex_index - 1)
                self.changed(self)
            else:
                # nowhere: add segment at end
                print 'Add segment'
                self.grace_node_view = None
                self.paths[path_index][vertex_index] = snap_point(point)
                if vertex_index == 0:
                    self.paths[path_index] = points + self.paths[path_index]
                else:
                    self.paths[path_index] = self.paths[path_index] + points
                    self.grabbed[0] = (self.grabbed[0][0], self.grabbed[0][1] + 1)
                self.changed(self)
        elif self.drag_case == EdgeView.DragCase.DIAMOND_CONNECTION:
            point = points[0]
            path_index, vertex_index = self.grabbed[0]
            assert vertex_index == 0
            next_vertex, vertex_after = (1, 2)
            end_index = path_index
            # Can reattach to a node (detaching path into a new edge), to the remainder of the same edge (canceling detachment) or to a different edge
            attach_to = self.diagram().attachable_at(event.x, event.y, self)
            if isinstance(attach_to, NodeView): # at a node: reattach to it
                new_edge_view = self.diagram().new_edge_view([self.edge.ends[end_index].node, attach_to.node], list(reversed(self.paths[path_index])))
                new_edge_view.paths[0][-1] = snap_point(attach_to.clamp(point))
                new_edge_view.edge.ends[0].aggregation = self.edge.ends[end_index].aggregation
                new_edge_view.edge.ends[0].navigable = self.edge.ends[end_index].navigable
                new_edge_view.edge.ends[0].caption = self.edge.ends[end_index].caption
                self.edge.ends.pop(end_index)
                self.paths.pop(path_index)
                self.check_diamond_necessity()
                new_edge_view.changed(self)
                self.changed(self)
                self.grabbed = None
                window.ungrab()
            elif isinstance(attach_to, EdgeView): # other edge
                attach_to.ensure_diamond(point)
                attach_to.edge.ends.append(self.edge.ends[end_index])
                attach_to.paths.append(self.paths[path_index])
                attach_to.paths[-1][0] = attach_to.diamond
                attach_to.changed(self)
                self.edge.ends.pop(end_index)
                self.paths.pop(path_index)
                self.check_diamond_necessity()
                self.changed(self)
                self.grabbed = None
                window.ungrab()
            elif self.hittest(point, path_index): # reattach to the same edge
                print 'Reattach back'
                self.paths[path_index][vertex_index] = self.diamond
                self.changed(self)
                self.grabbed = None
                window.ungrab()
            elif len(self.paths[path_index]) > 2 and distance(self.paths[path_index][next_vertex], self.paths[path_index][vertex_after], point) < EPSILON:
                print 'Delete segment'
                self.paths[path_index].pop(next_vertex)
                self.changed(self)
            else:
                # nowhere: add segment at end
                print 'Add segment'
                self.paths[path_index][vertex_index] = snap_point(point)
                self.paths[path_index] = points + self.paths[path_index]
                self.changed(self)
        elif self.drag_case == EdgeView.DragCase.INNER_VERTEX:
            point = points[0]
            path_index, vertex_index = self.grabbed[0]
            if (vertex_index >= 1
                and distance(self.paths[path_index][max(0, vertex_index - 2)],
                             self.paths[path_index][vertex_index - 1], point) < EPSILON):
                print 'Delete segment'
                self.paths[path_index].pop(vertex_index - 1)
                self.grabbed[0] = (path_index, vertex_index - 1)
                self.changed(self)
            elif (vertex_index + 2 < len(self.paths[path_index])
                  and distance(self.paths[path_index][min(len(self.paths[path_index]) - 1, vertex_index + 2)],
                               self.paths[path_index][vertex_index + 1], point) < EPSILON):
                print 'Delete segment'
                self.paths[path_index].pop(vertex_index + 1)
                self.changed(self)
            else:
                print 'Move vertex'
                self.paths[path_index][vertex_index] = snap_point(point)
                self.changed(self)
                self.grabbed = None
                window.ungrab()
        else:
            assert False, 'Unhandled drag case'

    def draw_nary_diamond(self, context):
        z = self.diamond
        context.move_to(z.real - NARY_DIAMOND_RADIUS, z.imag)
        context.line_to(z.real, z.imag + NARY_DIAMOND_RADIUS)
        context.line_to(z.real + NARY_DIAMOND_RADIUS, z.imag)
        context.line_to(z.real, z.imag - NARY_DIAMOND_RADIUS)
        context.close_path()
        context.set_source_rgb(255, 255, 255)
        context.fill_preserve()
        context.set_source_rgb(0, 0, 0)
        context.stroke()

    def draw_anchor(self, context, a, b):
        phi = cmath.phase(b - a)
        center = a + cmath.rect(ANCHOR_RADIUS, phi)
        context.arc(center.real, center.imag, ANCHOR_RADIUS, 0, 2 * math.pi)
        context.move_to(center.real - ANCHOR_RADIUS, center.imag)
        context.rel_line_to(2 * ANCHOR_RADIUS, 0)
        context.rel_move_to(-ANCHOR_RADIUS, -ANCHOR_RADIUS)
        context.rel_line_to(0, 2 * ANCHOR_RADIUS)
        context.stroke()
        return 2 * center - a

    def draw_diamond(self, context, a, b, fill):
        phi = cmath.phase(b - a)
        rels = [cmath.rect(DIAMOND_SIZE, phi + DIAMOND_ANGLE),
                cmath.rect(DIAMOND_SIZE, phi - DIAMOND_ANGLE),
                cmath.rect(-DIAMOND_SIZE, phi + DIAMOND_ANGLE)]
        context.move_to(a.real, a.imag)
        for rel in rels:
            context.rel_line_to(rel.real, rel.imag)
        context.close_path()
        if (fill == 1):
            context.stroke()
        elif (fill == 2):
            context.fill()
        return a + rels[0] + rels[1]

    def draw_triangle(self, context, a, b):
        phi = cmath.phase(b - a)
        points = [a + cmath.rect(TRIANGLE_SIZE, phi + TRIANGLE_ANGLE),
                  a + cmath.rect(TRIANGLE_SIZE, phi - TRIANGLE_ANGLE)]
        context.move_to(a.real, a.imag)
        for point in points:
            context.line_to(point.real, point.imag)
        context.close_path()
        context.stroke()
        return (points[0] + points[1]) / 2

    def draw_arrow(self, context, a, b):
        phi = cmath.phase(b - a)
        c, d = [a + cmath.rect(ARROW_SIZE, phi + ARROW_ANGLE), a + cmath.rect(ARROW_SIZE, phi - ARROW_ANGLE)]
        context.move_to(c.real, c.imag)
        context.line_to(a.real, a.imag)
        context.line_to(d.real, d.imag)
        context.stroke()

    def draw_caption(self, context, a, b, text):
        context.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        layout = PangoCairo.create_layout(context)
        font = Pango.FontDescription('sans 10')
        layout.set_font_description(font)
        layout.set_text(text, len(text))
        PangoCairo.update_layout(context, layout)
        width, height = layout.get_pixel_size()
        phi = cmath.phase(b - a)
        if b.real > a.real or b.real >= a.real and b.imag < a.imag:
            context.translate(a.real, a.imag)
            context.rotate(phi)
            context.translate(height / 4, -height)
        else:
            context.translate(a.real, a.imag)
            context.rotate(math.pi + phi)
            context.translate(-width - height / 4, -height)
        PangoCairo.show_layout(context, layout)
        context.identity_matrix()

    def draw(self, context):
        if (self.grace_node_view
            and self.grace_node_view.wants(self.paths[0][1].real, self.paths[0][1].imag)):
            return
        context.set_source_rgb(0, 0, 0)
        context.set_line_width(1)
        context.set_dash([], 0)

        if not self.diamond:
            path = self.paths[0][:]

            if self.edge.ends[0].kind == EdgeEnd.Kind.NESTING:
                path[0] = self.draw_anchor(context, path[0], path[1])
            if self.edge.ends[1].kind == EdgeEnd.Kind.NESTING:
                path[-1] = self.draw_anchor(context, path[-1], path[-2])

            if self.edge.ends[0].aggregation:
                path[0] = self.draw_diamond(context, path[0], path[1], self.edge.ends[0].aggregation)
            if self.edge.ends[1].aggregation:
                path[-1] = self.draw_diamond(context, path[-1], path[-2], self.edge.ends[1].aggregation)

            if self.edge.ends[0].kind in (EdgeEnd.Kind.GENERALIZATION, EdgeEnd.Kind.REALIZATION):
                path[0] = self.draw_triangle(context, path[0], path[1])
            if self.edge.ends[1].kind in (EdgeEnd.Kind.GENERALIZATION, EdgeEnd.Kind.REALIZATION):
                path[-1]= self.draw_triangle(context, path[-1], path[-2])

            if self.edge.ends[0].navigable or self.edge.ends[0].kind == EdgeEnd.Kind.DEPENDENCY:
                self.draw_arrow(context, path[0], path[1])
            if self.edge.ends[1].navigable or self.edge.ends[1].kind == EdgeEnd.Kind.DEPENDENCY:
                self.draw_arrow(context, path[-1], path[-2])

            if {end.kind for end in self.edge.ends} & {EdgeEnd.Kind.REALIZATION, EdgeEnd.Kind.DEPENDENCY, EdgeEnd.Kind.ASSOCIATION_CLASS}:
                context.set_dash([DASH_SIZE], 0)

            context.move_to(path[0].real, path[0].imag)
            for z in path[1:]:
                context.line_to(z.real, z.imag)
            context.stroke()
            context.set_dash([], 0)

            if self.edge.ends[0].caption:
                self.draw_caption(context, path[0], path[1], self.edge.ends[0].caption or '')
            if self.edge.ends[1].caption:
                self.draw_caption(context, path[-1], path[-2], self.edge.ends[1].caption or '')
        else:
            for p, end in zip(self.paths, self.edge.ends):
                path = p[:]

                if end.kind == EdgeEnd.Kind.NESTING:
                    path[-1] = self.draw_anchor(context, path[-1], path[-2])
                if end.aggregation:
                    path[-1] = self.draw_diamond(context, path[-1], path[-2], end.aggregation)
                if end.kind in (EdgeEnd.Kind.GENERALIZATION, EdgeEnd.Kind.REALIZATION):
                    path[-1] = self.draw_triangle(context, path[-1], path[-2])
                if end.navigable or end.kind == EdgeEnd.Kind.DEPENDENCY:
                    self.draw_arrow(context, path[-1], path[-2])

                if (end.kind == EdgeEnd.Kind.ASSOCIATION_CLASS
                    or {end.kind for end in self.edge.ends} & {EdgeEnd.Kind.REALIZATION, EdgeEnd.Kind.DEPENDENCY}):
                    context.set_dash([DASH_SIZE], 0)
                else:
                    context.set_dash([], 0)

                context.move_to(path[0].real, path[0].imag)
                for z in path[1:]:
                    context.line_to(z.real, z.imag)
                context.stroke()
                context.set_dash([], 0)

                self.draw_caption(context, path[-1], path[-2], end.caption or '')
            self.draw_nary_diamond(context)

    def node_resizing(self, node_view, new_width, new_height):
        if self.diamond:
            for path, end in zip(self.paths, self.edge.ends):
                if end.node is node_view.node:
                    path[-1] = node_view.scale(path[-1], new_width, new_height)
        else:
            if self.edge.ends[0].node is node_view.node:
                self.paths[0][0] = node_view.scale(self.paths[0][0], new_width, new_height)
            if self.edge.ends[1].node is node_view.node:
                self.paths[0][-1] = node_view.scale(self.paths[0][-1], new_width, new_height)
        self.changed(self)


def DoNothing(o):
    pass


class Diagram:
    def __init__(self, model):
        self.views = []
        self.model = weakref.ref(model)
        self.changed = DoNothing

    def draw(self, context):
        for view in self.views:
            view.draw(context)

    def node_views_at(self, x, y):
        return [view for view in self.views
                if isinstance(view, NodeView) and view.wants(x, y)]

    def node_view_at(self, x, y):
        node_views = self.node_views_at(x, y)
        if len(node_views) > 1:
            print 'Cannot decide which node to attach to'
            return None
        if node_views:
            print 'Attach to node'
            return node_views[0]
        return None

    def attachable_at(self, x, y, excluded):
        node_view = self.node_view_at(x, y)
        if node_view:
            return node_view
        edge_view = self.edge_view_at(x, y, excluded)
        if edge_view:
            print 'Attach to edge'
            return edge_view
        return None

    def edge_view_at(self, x, y, excluded=None):
        candidates = [(view.distance(x, y), view)
                      for view in self.views
                      if isinstance(view, EdgeView) and view is not excluded and view.wants(x, y)]
        return min(candidates)[1] if candidates else None

    def new_node_view(self, x, y, w=50, h=22):
        node = self.model().new_node()
        view = ClassifierNodeView(self, node, x, y, w, h)
        view.changed = self.changed
        self.views.append(view)
        self.changed(self)
        return view

    def new_edge_view(self, nodes, path):
        edge = self.model().new_edge(nodes)
        view = EdgeView(self, edge, path)
        view.changed = self.changed
        self.views.append(view)
        self.changed(self)
        return view

    def delete_node(self, node):
        for view in self.views[:]:
            if isinstance(view, NodeView) and view.node is node:
                self.delete_view(view)
        self.changed(self)

    def delete_edge(self, edge):
        for view in self.views[:]:
            if isinstance(view, EdgeView) and view.edge is edge:
                self.delete_view(view)
        self.changed(self)

    def delete_paths(self, node):
        for view in self.views[:]:
            if isinstance(view, EdgeView):
                view.delete_paths(node)
        self.changed(self)

    def delete_view(self, view):
        self.views.remove(view)
        self.changed(self)


class Model:
    def __init__(self):
        self.nodes = []
        self.edges = []
        self.diagram = Diagram(self)
        self.changed = DoNothing

    def new_node(self):
        node = Node()
        self.nodes.append(node)
        self.changed(self)
        return node

    def new_edge(self, nodes):
        edge = Edge(nodes)
        self.edges.append(edge)
        self.changed(self)
        return edge

    def delete_node(self, node):
        self.delete_paths(node)
        self.diagram.delete_node(node)
        self.nodes.remove(node)
        self.changed(self)

    def delete_paths(self, node):
        self.diagram.delete_paths(node)
        self.changed(self)

    def delete_edge(self, edge):
        self.diagram.delete_edge(edge)
        self.edges.remove(edge)
        self.changed(self)


class MainWindow(Gtk.Window):

    def __init__(self):
        Gtk.Window.__init__(self, title='JotUML')
        self.connect('delete_event', self.delete_event)
        self.connect('destroy', self._destroy)

        builder = Gtk.Builder()
        builder.add_from_file('jotuml.ui')
        self.add(builder.get_object('main_box'))
        self.drawing_area = builder.get_object('drawing_area')
        self.edit_action_group = builder.get_object('edit_action_group')
        self.edit_cut_action = builder.get_object('edit_cut_action')
        self.edit_copy_action = builder.get_object('edit_copy_action')
        self.edit_paste_action = builder.get_object('edit_paste_action')
        self.edit_delete_action = builder.get_object('edit_delete_action')
        self.aggregate_action = builder.get_object('edge_end_aggregate_action')
        self.composite_action = builder.get_object('edge_end_composite_action')
        self.navigability_action = builder.get_object('edge_end_navigable_action')
        self.generalization_action = builder.get_object('edge_end_generalization_action')
        self.realization_action = builder.get_object('edge_end_realization_action')
        self.dependency_action = builder.get_object('edge_end_dependency_action')
        self.scope_action = builder.get_object('edge_end_scope_action')
        self.association_class_action = builder.get_object('edge_path_association_class_action')
        self.edge_path_delete_action = builder.get_object('edge_path_delete_action')
        self.node_popup = builder.get_object('compartment_popup')
        self.edge_popup = builder.get_object('edge_popup')
        self.edge_end_popup = builder.get_object('edge_end_popup')
        self.node_class_action = builder.get_object('node_class_action')
        self.node_interface_action = builder.get_object('node_interface_action')
        self.node_package_action = builder.get_object('node_package_action')
        self.node_node_action = builder.get_object('node_node_action')
        self.node_component_action = builder.get_object('node_component_action')
        self.node_actor_action = builder.get_object('node_actor_action')
        self.node_usecase_action = builder.get_object('node_usecase_action')
        builder.connect_signals(self)

        clipboard = Gtk.Clipboard.get(Gdk.SELECTION_CLIPBOARD)
        clipboard.connect('owner-change', self.clipboard_owner_change)

        self.model = Model()
        self.diagram = self.model.diagram
        self.diagram.changed = self.redraw

        self.entry = None

    def clipboard_owner_change(self, clipboard, event, data=None):
        print 'Clipboard owner change'
        if self.entry:
            self.edit_paste_action.set_sensitive(clipboard.wait_is_text_available())

    def edit_node_command(self, data=None):
        self.context.edit(self, None)
        self.context = None

    def delete_node_command(self, data=None):
        self.context.delete()
        self.context = None

    def delete_edge_command(self, data=None):
        self.context[0].delete()
        self.context = None

    def delete_edge_path_command(self, data=None):
        self.context[0].delete_path(self.context[1].path_index)
        self.context = None

    def aggregate_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_aggregation(self.context[1].end_index, EdgeEnd.Aggregation.AGGREGATE if action.get_active() else EdgeEnd.Aggregation.NONE)
        self.context = None

    def composite_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_aggregation(self.context[1].end_index, EdgeEnd.Aggregation.COMPOSITE if action.get_active() else EdgeEnd.Aggregation.NONE)
        self.context = None

    def navigability_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_navigability(self.context[1].end_index, action.get_active())
        self.context = None

    def generalization_toggled_command(self, action, data=None):
        print 'Hello'
        if not self.context: return
        self.context[0].set_kind(self.context[1].end_index, EdgeEnd.Kind.GENERALIZATION if action.get_active() else EdgeEnd.Kind.ASSOCIATION)
        self.context = None

    def realization_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_kind(self.context[1].end_index, EdgeEnd.Kind.REALIZATION if action.get_active() else EdgeEnd.Kind.ASSOCIATION)
        self.context = None

    def dependency_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_kind(self.context[1].end_index, EdgeEnd.Kind.DEPENDENCY if action.get_active() else EdgeEnd.Kind.ASSOCIATION)
        self.context = None

    def scope_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_kind(self.context[1].end_index, EdgeEnd.Kind.NESTING if action.get_active() else EdgeEnd.Kind.ASSOCIATION)
        self.context = None

    def association_class_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_kind(self.context[1].end_index, EdgeEnd.Kind.ASSOCIATION_CLASS if action.get_active() else EdgeEnd.Kind.ASSOCIATION)
        self.context = None

    def redraw(self, what):
        self.drawing_area.queue_draw_area(0, 0, self.drawing_area.get_allocated_width(), self.drawing_area.get_allocated_height())

    def draw(self, widget, context, data=None):
        self.diagram.draw(context)
        return False

    def grab(self, views):
        self.drawing_area.grab_add()
        self.grabbed = views

    def ungrab(self):
        self.drawing_area.grab_remove()
        self.grabbed = None

    def finish_edit(self, text=None):
        buf = self.entry.get_buffer()
        self.edited.edited(self, text if text is not None else buf.get_text(*buf.get_bounds(), include_hidden_chars=True), self.edited_context)
        self.entry.destroy()
        self.entry = None
        self.edit_action_group.set_sensitive(False)

    def cancel_edit(self):
        self.entry.destroy()
        self.entry = None
        self.edit_action_group.set_sensitive(False)

    def button_press_event(self, widget, event, data=None):
        if self.entry:
            self.finish_edit()
            return

        edge_view = self.diagram.edge_view_at(event.x, event.y)
        if edge_view:
            return edge_view.press(self, event)
        node_views = self.diagram.node_views_at(event.x, event.y)
        if len(node_views) > 1:
            print 'Cannot choose which node to activate'
            return False
        if node_views:
            return node_views[0].press(self, event)
        print 'Create node'
        self.diagram.new_node_view(event.x, event.y).edit(self, event)
        return False

    def button_release_event(self, widget, event, data=None):
        if not self.drawing_area.has_grab():
            return False
        if not self.grabbed:
            self.ungrab()
            return False
        for o in self.grabbed:
            o.release(self, event)
        return False

    def motion_notify_event(self, widget, event, data=None):
        if not self.drawing_area.has_grab():
            return False
        if not self.grabbed:
            return False
        for o in self.grabbed:
            o.motion(self, event)
        return False

    def delete_event(self, widget, event, data=None):
        return False

    def file_quit_command(self, data=None):
        self.emit('destroy')

    def _destroy(self, widget, data=None):
        Gtk.main_quit()

    def edit(self, subject, text, x, y, w, h, context=None):
        self.entry = Gtk.TextView()
        self.entry.get_buffer().set_text(text or '')
        self.entry.set_size_request(w, h)
        self.entry.set_indent(-16)
        self.entry.set_right_margin(16)
        self.entry.connect('draw', self.edit_draw)
        self.entry.connect('key_press_event', self.edit_key_press_event)
        self.entry.show()
        self.drawing_area.put(self.entry, x, y)
        self.entry.grab_focus()
        self.edited = subject
        self.edited_context = context
        self.edit_action_group.set_sensitive(True)
        buf = self.entry.get_buffer()
        buf.connect('notify::has-selection', self.edit_has_selection)
        self.edit_has_selection(buf, buf.get_has_selection())

    def edit_cut_command(self, data=None):
        self.entry.emit('cut-clipboard')
        return True

    def edit_copy_command(self, data=None):
        self.entry.emit('copy-clipboard')
        return True

    def edit_paste_command(self, data=None):
        self.entry.emit('paste-clipboard')
        return True

    def edit_delete_command(self, data=None):
        self.entry.emit('delete-from-cursor', Gtk.DeleteType.CHARS, 0)
        return True

    def edit_has_selection(self, buf, param):
        value = buf.get_has_selection()
        self.edit_cut_action.set_sensitive(value)
        self.edit_copy_action.set_sensitive(value)
        self.edit_delete_action.set_sensitive(value)

    def edit_draw(self, edit, context, data=None):
        context.set_source_rgb(0, 0, 0)
        context.set_line_width(1)

        buf = edit.get_buffer()
        it = buf.get_start_iter()
        end = buf.get_end_iter()
        it.forward_to_line_end()
        while not it.equal(end):
            if it.get_char() == COMPARTMENT_SEPARATOR:
                r = edit.get_iter_location(it)
                context.move_to(0, r.y + r.height + 0.5)
                context.rel_line_to(edit.get_allocated_width(), 0)
            it.forward_to_line_end()

        context.stroke()

        return False

    def edit_key_press_event(self, edit, event, data=None):
        print 'keypress %x' % event.keyval
        buf = self.entry.get_buffer()
        cursor = buf.get_iter_at_mark(buf.get_insert())
        if (event.keyval == Gdk.KEY_Escape):
            self.finish_edit()
            return True
        if (event.keyval == Gdk.KEY_Return
            and Gdk.ModifierType.SHIFT_MASK & event.state
            and not Gdk.ModifierType.CONTROL_MASK & event.state):
            buf.insert_interactive_at_cursor(u'\u2028', -1, self.entry.get_editable()) # Line separator
            return True
        if (event.string == '-' and cursor.starts_line()):
            buf.insert_interactive_at_cursor(u'\u2212', -1, self.entry.get_editable()) # Minus sign
            return True
        if (event.keyval == Gdk.KEY_Return
            and not Gdk.ModifierType.SHIFT_MASK & event.state
            and Gdk.ModifierType.CONTROL_MASK & event.state):
            buf.insert_interactive_at_cursor(COMPARTMENT_SEPARATOR, -1, self.entry.get_editable())
            return True
        return False


def main():
    main_window = MainWindow()
    main_window.show()
    Gtk.main()


if __name__ == '__main__':
    main()

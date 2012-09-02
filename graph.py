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
    def __init__(self):
        self.compartments = [Compartment(self)]

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


class Edge:
    class Aggregation:
        NONE = 0
        AGGREGATE = 1
        COMPOSITE = 2

    def __init__(self, from_node, to_node):
        self.from_node = from_node
        self.from_aggregation = Edge.Aggregation.NONE
        self.from_navigable = False
        self.from_caption = None
        self.to_node = to_node
        self.to_aggregation = Edge.Aggregation.NONE
        self.to_navigable = False
        self.to_caption = None


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
    def __init__(self, diagram, node, x, y, w, h):
        View.__init__(self)
        self.diagram = weakref.ref(diagram)
        self.node = node
        self.x = snap_coord(x)
        self.y = snap_coord(y)
        self.w = int(w)
        self.h = int(h)
        self.compartments = [CompartmentView(self, c) for c in node.compartments]
        self.is_new = True
        self.changed = DoNothing

    def clamp(self, point):
        top, bottom, left, right = [self.y, self.y + self.h, self.x, self.x + self.w]
        vertices = [complex(left, top), complex(left, bottom), complex(right, bottom), complex(right, top), complex(left, top)]
        projections = [projection(a, b, point) for a, b in zip(vertices[:-1], vertices[1:])]
        return snap_point(min(projections, key=lambda(p): abs(p - point)))

    def wants(self, x, y):
        return (self.x - EPSILON <= x < self.x + self.w + EPSILON
                and self.y - EPSILON <= y < self.y + self.h + EPSILON)

    def press(self, window, event):
        if event.button == 3: # right button: pop up contex menu
            print 'Compartment popup menu'
            window.context = self.compartment_at(event.x, event.y)
            window.node_popup.popup(None, None, None, None, 3, event.time)
        elif event.button == 2: # middle button: initiate node move (with all incident edges)
            print 'Move node'
            self.drag_point = complex(event.x - self.x, event.y - self.y)
            views = [self]
            for edge_view in self.diagram().views:
                if isinstance(edge_view, EdgeView) and edge_view.grab_node(self, event):
                    views.append(edge_view)
            window.grab(views)
        elif (event.x < self.x or event.x >= self.x + self.w
              or event.y < self.y or event.y >= self.y + self.h): # on edge: start a new edge
            print 'Create edge'
            drag_point = complex(event.x, event.y)
            start_point = self.clamp(drag_point)
            edge_view = self.diagram().new_edge_view(self.node, None, [start_point, drag_point])
            window.grab([edge_view])
            edge_view.grab([1], event)
        else:
            self.edit(window, event)

    def edit(self, window, event):
        print 'Edit node'
        window.edit(self, COMPARTMENT_SEPARATOR.join(c.compartment.text for c in self.compartments),
                    self.x + 1, self.y + 1, self.w - 2, self.h - 2)
        if event:
            x, y = event.x - self.x, event.y - self.y
            bx, by = window.entry.window_to_buffer_coords(Gtk.TextWindowType.TEXT, x, y)
            it = window.entry.get_iter_at_location(bx, by)
            window.entry.get_buffer().place_cursor(it)

    def edited(self, window, text, context=None):
        if not text and self.is_new:
            print 'Cancel node'
            self.diagram().model().delete_node(self.node)
        else:
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
            self.changed(self)

    def compartment_at(self, x, y):
        if not (self.x <= x < self.x + self.w):
            return None
        for c in self.compartments:
            if self.y + c.y <= y < self.y + c.y + c.h:
                return c
        return self.compartments[-1]

    def delete(self):
        print 'Delete node'
        self.diagram().model().delete_node(self.node)

    def motion(self, window, event):
        self.x = event.x - self.drag_point.real
        self.y = event.y - self.drag_point.imag
        self.changed(self)

    def release(self, window, event):
        print 'Moved node'
        self.x = snap_coord(event.x - self.drag_point.real)
        self.y = snap_coord(event.y - self.drag_point.imag)
        self.changed(self)
        window.ungrab()
        self.drag_start = None
        self.drag_start_event = None

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
                if isinstance(e, EdgeView) and self.node in [e.edge.from_node, e.edge.to_node]:
                    e.node_resizing(self, width, height)
            self.w = width
            self.h = height

    def draw(self, context):
        context.set_source_rgb(0, 0, 0)
        context.set_line_width(1)
        context.rectangle(self.x, self.y, self.w, self.h)

        for c in self.compartments[1:]:
            context.move_to(self.x, self.y + c.y)
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
            context.translate(self.x + 2, self.y + c.y + 3)
            PangoCairo.show_layout(context, layout)
            context.identity_matrix()


class EdgeView(View):
    def __init__(self, diagram, edge, path):
        View.__init__(self)
        self.diagram = weakref.ref(diagram)
        self.edge = edge
        self.path = path
        self.changed = DoNothing

    def wants(self, x, y):
        if not self.path:
            return False
        p = complex(x, y)
        return EPSILON > min(distance(a, b, p)
                             for a, b in zip(self.path[:-1], self.path[1:]))

    def grab(self, points, event):
        self.grabbed = points
        self.drag_points = [complex(event.x, event.y) - self.path[point] for point in points]

    def grab_node(self, node_view, event):
        if node_view.node is self.edge.from_node:
            if node_view.node is self.edge.to_node:
                points = range(len(self.path))
            else:
                points = [0]
        elif node_view.node is self.edge.to_node:
            points = [len(self.path) - 1]
        else:
            return None
        self.grab(points, event)
        return points

    class HitTest:
        class Vertex:
            def __init__(self, index):
                self.index = index
        class End(Vertex):
            def __init__(self, index):
                EdgeView.HitTest.Vertex.__init__(self, index)
        class Segment:
            def __init__(self, index):
                self.index = index

    def hittest(self, point):
        for i in range(len(self.path)):
            if abs(point - self.path[i]) < EPSILON: # on a vertex: move vertex
                if i in [0, len(self.path) - 1]:
                    return EdgeView.HitTest.End(i)
                else:
                    return EdgeView.HitTest.Vertex(i)
        for i in range(len(self.path) - 1):
            if distance(self.path[i], self.path[i + 1], point) < EPSILON: # on a segment: add vertex
                return EdgeView.HitTest.Segment(i)

    def set_aggregation(self, end, value):
        print 'Set aggregation at', end, 'to', value
        if end == 0:
            self.edge.from_aggregation = value
        else:
            self.edge.to_aggregation = value
        self.changed(self)

    def set_navigability(self, end, value):
        print 'Set navigability at', end, 'to', value
        if end == 0:
            self.edge.from_navigable = value
        else:
            self.edge.to_navigable = value
        self.changed(self)

    def press(self, window, event):
        if event.button == 1:
            hittest = self.hittest(complex(event.x, event.y))
            if isinstance(hittest, EdgeView.HitTest.End):
                print 'Edit end'
                end = self.path[hittest.index]
                caption = (self.edge.from_caption if hittest.index == 0 else self.edge.to_caption) or ''
                window.edit(self, caption, end.real, end.imag, 80, 22, hittest.index)
                buf = window.entry.get_buffer()
                buf.place_cursor(buf.get_end_iter())
                return
        elif event.button == 2:
            point = complex(event.x, event.y)
            hittest = self.hittest(point)
            if isinstance(hittest, EdgeView.HitTest.Vertex):
                print 'Move vertex'
                self.grab([hittest.index], event)
                window.grab([self])
                return
            if isinstance(hittest, EdgeView.HitTest.Segment):
                print 'Add vertex'
                self.path.insert(hittest.index + 1, point)
                self.grab([hittest.index + 1], event)
                window.grab([self])
                return
        elif event.button == 3:
            hittest = self.hittest(complex(event.x, event.y))
            if isinstance(hittest, EdgeView.HitTest.End):
                print 'End context menu'
                aggregation = self.edge.from_aggregation if hittest.index == 0 else self.edge.to_aggregation
                window.aggregate_action.set_active(aggregation == Edge.Aggregation.AGGREGATE)
                window.composite_action.set_active(aggregation == Edge.Aggregation.COMPOSITE)
                window.navigability_action.set_active(self.edge.from_navigable if hittest.index == 0 else self.edge.to_navigable)
                window.context = (self, hittest)
                window.edge_end_popup.popup(None, None, None, None, 3, event.time)
            else:
                print 'Edge context menu'
                window.context = (self, hittest)
                window.edge_popup.popup(None, None, None, None, 3, event.time)
            return

    def edited(self, window, text, index):
        print 'Edited end'
        if index == 0:
            self.edge.from_caption = text
        else:
            self.edge.to_caption = text
        self.changed(self)

    def delete(self):
        print 'Delete edge'
        self.diagram().model().delete_edge(self.edge)

    def motion(self, window, event):
        if self.grabbed is not None:
            for i in range(len(self.grabbed)):
                self.path[self.grabbed[i]] = complex(event.x, event.y) - self.drag_points[i]
            self.changed(self)

    def release(self, window, event):
        points = [complex(event.x, event.y) - point for point in self.drag_points]
        if self.grabbed == [0] or self.grabbed == [len(self.path) - 1]:
            node_views = self.diagram().node_views_at(event.x, event.y)
            if len(node_views) > 1: # ambiguous: continue
                print 'Cannot choose which node to attach to'
                return
            if node_views: # at a node: (re)attach to it
                print 'Attach to node'
                if self.grabbed == [0]:
                    self.edge.from_node = node_views[0].node
                else:
                    self.edge.to_node = node_views[0].node
                self.path[self.grabbed[0]] = snap_point(node_views[0].clamp(points[0]))
                self.changed(self)
                window.ungrab()
            else: # nowhere: add segment at end
                print 'Add segment'
                self.path[self.grabbed[0]] = snap_point(points[0])
                if self.grabbed == [0]:
                    self.path = points + self.path
                else:
                    self.path = self.path + points
                    self.grabbed[0] += 1
                self.changed(self)
        elif len(self.grabbed) == 1: # dragging an inner vertex
            print 'Moved vertex'
            self.path[self.grabbed[0]] = snap_point(points[0])
            if min(abs(points[0] - self.path[self.grabbed[0] + i]) for i in [-1, 1]) < EPSILON: # near an adjacent vertex: meld with it
                print 'Deleted segment'
                self.path.pop(self.grabbed[0])
            self.changed(self)
            window.ungrab()
        else: # dragging the whole edge
            print 'Moved edge'
            for i in range(len(self.grabbed)):
                self.path[self.grabbed[i]] = [snap_point(p) for p in points[i]]
            self.changed(self)
            window.ungrab()

    def draw_diamond(self, context, a, b, fill):
        phi = cmath.phase(b - a)
        DIAMOND_SIZE = 10
        DIAMOND_ANGLE = math.pi / 6
        rels = [cmath.rect(DIAMOND_SIZE, phi + DIAMOND_ANGLE), cmath.rect(DIAMOND_SIZE, phi - DIAMOND_ANGLE), cmath.rect(-DIAMOND_SIZE, phi + DIAMOND_ANGLE)]
        context.move_to(a.real, a.imag)
        for rel in rels:
            context.rel_line_to(rel.real, rel.imag)
        context.close_path()
        if (fill == 1):
            context.stroke()
        elif (fill == 2):
            context.fill()
        return a + rels[0] + rels[1]

    def draw_arrow(self, context, a, b):
        phi = cmath.phase(b - a)
        ARROW_SIZE = 10
        ARROW_ANGLE = math.pi / 6
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
        if not self.path:
            return
        context.set_source_rgb(0, 0, 0)
        context.set_line_width(1)

        path = self.path[:]

        if self.edge.from_aggregation:
            path[0] = self.draw_diamond(context, path[0], path[1], self.edge.from_aggregation)
        if self.edge.to_aggregation:
            path[-1] = self.draw_diamond(context, path[-1], path[-2], self.edge.to_aggregation)

        if self.edge.from_navigable:
            self.draw_arrow(context, path[0], path[1])
        if self.edge.to_navigable:
            self.draw_arrow(context, path[-1], path[-2])

        context.move_to(path[0].real, path[0].imag)
        for z in path[1:]:
            context.line_to(z.real, z.imag)
        context.stroke()

        if self.edge.from_caption:
            self.draw_caption(context, path[0], path[1], self.edge.from_caption or '')
        if self.edge.to_caption:
            self.draw_caption(context, path[-1], path[-2], self.edge.to_caption or '')

    def node_resizing(self, node_view, new_width, new_height):
        end = 0 if node_view.node is self.edge.from_node else -1
        self.path[end] = complex(node_view.x + new_width * (self.path[end].real - node_view.x) / node_view.w,
                                 node_view.y + new_height * (self.path[end].imag - node_view.y) / node_view.h)
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

    def edge_view_at(self, x, y):
        for view in self.views:
            if isinstance(view, EdgeView) and view.wants(x, y):
                return view
        return None

    def new_node_view(self, x, y, w=50, h=22):
        node = self.model().new_node()
        view = NodeView(self, node, x, y, w, h)
        view.changed = self.changed
        self.views.append(view)
        self.changed(self)
        return view

    def new_edge_view(self, from_node, to_node, path):
        edge = self.model().new_edge(from_node, to_node)
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

    def new_edge(self, from_node, to_node=None):
        edge = Edge(from_node, to_node)
        self.edges.append(edge)
        self.changed(self)
        return edge

    def delete_node(self, node):
        self.delete_edges(node)
        self.diagram.delete_node(node)
        self.nodes.remove(node)
        self.changed(self)

    def delete_edges(self, node):
        for edge in self.edges[:]:
            if node in [edge.from_node, edge.to_node]:
                self.delete_edge(edge)

    def delete_edge(self, edge):
        self.diagram.delete_edge(edge)
        self.edges.remove(edge)
        self.changed(self)


class MainWindow(Gtk.Window):

    UI_INFO = '''
<ui>
  <popup name="CompartmentMenu">
    <menuitem action="EditNode"/>
    <menuitem action="DeleteNode"/>
  </popup>
  <popup name="EdgeMenu">
    <menuitem action="DeleteEdge"/>
  </popup>
  <popup name="EdgeEndMenu">
    <menuitem action="Aggregate"/>
    <menuitem action="Composite"/>
    <separator/>
    <menuitem action="Navigable"/>
    <separator/>
    <menuitem action="DeleteEdge"/>
  </popup>
</ui>'''

    def __init__(self):
        Gtk.Window.__init__(self, title='Hello World')
        self.connect('delete_event', self.delete_event)
        self.connect('destroy', self._destroy)

        self.model = Model()
        self.diagram = self.model.diagram
        self.diagram.changed = self.redraw

        self.drawing_area = Gtk.Layout()
        self.drawing_area.add_events(Gdk.EventMask.ALL_EVENTS_MASK)
        self.drawing_area.connect('draw', self.draw)
        self.drawing_area.connect('button-press-event', self.button_press_event)
        self.drawing_area.connect('button-release-event', self.button_release_event)
        self.drawing_area.connect('motion-notify-event', self.motion_notify_event)
        self.add(self.drawing_area)
        self.drawing_area.show()
        self.drawing_area.set_can_focus(True)
        self.drawing_area.grab_focus()

        self.entry = None

        action_group = Gtk.ActionGroup('actions')
        action_group.add_actions([('EditNode', Gtk.STOCK_EDIT, None, None, None, self.edit_node_command),
                                  ('DeleteNode', Gtk.STOCK_DELETE, None, None, None, self.delete_node_command),
                                  ('DeleteEdge', Gtk.STOCK_DELETE, None, None, None, self.delete_edge_command)])

        self.aggregate_action = Gtk.ToggleAction('Aggregate', '_Aggregate', None, None)
        self.composite_action = Gtk.ToggleAction('Composite', '_Composite', None, None)
        self.navigability_action = Gtk.ToggleAction('Navigable', '_Navigable', None, None)
        self.aggregate_action.connect('toggled', self.aggregation_toggled_command, Edge.Aggregation.AGGREGATE)
        self.composite_action.connect('toggled', self.aggregation_toggled_command, Edge.Aggregation.COMPOSITE)
        self.navigability_action.connect('toggled', self.navigability_toggled_command)
        action_group.add_action(self.aggregate_action)
        action_group.add_action(self.composite_action)
        action_group.add_action(self.navigability_action)

        self.ui_manager = Gtk.UIManager()
        self.ui_manager.add_ui_from_string(MainWindow.UI_INFO)
        self.ui_manager.insert_action_group(action_group)
 
        self.node_popup = self.ui_manager.get_widget('/CompartmentMenu')
        self.edge_popup = self.ui_manager.get_widget('/EdgeMenu')
        self.edge_end_popup = self.ui_manager.get_widget('/EdgeEndMenu')

    def edit_node_command(self, data=None):
        self.context.node_view().edit(self, None)
        self.context = None

    def delete_node_command(self, data=None):
        self.context.node_view().delete()
        self.context = None

    def delete_edge_command(self, data=None):
        self.context[0].delete()
        self.context = None

    def aggregation_toggled_command(self, action, data):
        if not self.context: return
        self.context[0].set_aggregation(self.context[1].index, data if action.get_active() else Edge.Aggregation.NONE)
        self.context = None

    def navigability_toggled_command(self, action, data=None):
        if not self.context: return
        self.context[0].set_navigability(self.context[1].index, action.get_active())
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

    def cancel_edit(self):
        self.entry.destroy()
        self.entry = None

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

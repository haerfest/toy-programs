import pyglet

# Example from https://pyglet.readthedocs.io/

window = pyglet.window.Window()

label = pyglet.text.Label('Hello, world',
                          font_name='Timew New Roman',
                          font_size=36,
                          x=window.width//2, y=window.height//2,
                          anchor_x='center', anchor_y='center')


@window.event
def on_draw():
    window.clear()
    label.draw()

pyglet.app.run()

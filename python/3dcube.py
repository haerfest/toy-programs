# Shows a rotating 3D cube using OpenGL.
# https://youtu.be/R4n4NyDG2hI

import pygame
from pygame.locals import *

from OpenGL.GL import *
from OpenGL.GLU import *

import random


# These vertices describe the points of a cube.
vertices = (
    (1, -1, -1),                # vertex 0
    (1, 1, -1),                 # vertex 1
    (-1, 1, -1),                # vertex 2
    (-1, -1, -1),               # vertex 3
    (1, -1, 1),                 # vertex 4
    (1, 1, 1),                  # vertex 5
    (-1, -1, 1),                # vertex 6
    (-1, 1, 1)                  # vertex 7
)

# These edges connect the vertices.
edges = (
    (0, 1),                     # vertex 0 => vertex 1
    (0, 3),                     # vertex 0 => vertex 3
    (0, 4),                     # and so on
    (2, 1),
    (2, 3),
    (2, 7),
    (6, 3),
    (6, 4),
    (6, 7),
    (5, 1),
    (5, 4),
    (5, 7)
)

surfaces = (
    (0, 1, 2, 3),               # surface between vertices 0, 1, 2 and 3
    (3, 2, 7, 6),               # and so on
    (6, 7, 5, 4),
    (4, 5, 1, 0),
    (1, 5, 7, 2),
    (4, 0, 3, 6)
)


# Each surface will get a random color.
colors = [(random.uniform(0.5, 1),
           random.uniform(0.5, 1),
           random.uniform(0.5, 1)) for _ in range(len(surfaces))]


def Cube():
    # Tell OpenGL that it should draw quads between the vertices we give it.
    glBegin(GL_QUADS)

    for i, surface in enumerate(surfaces):
        glColor3fv(colors[i])
        for vertex in surface:
            glVertex3fv(vertices[vertex])
    glEnd()


def main():
    pygame.init()
    resolution = (800, 600)
    pygame.display.set_mode(resolution, DOUBLEBUF | OPENGL)

    fovy = 45
    aspect = resolution[0] / resolution[1]
    zNear, zFar = 0.1, 50
    gluPerspective(fovy, aspect, zNear, zFar)

    # Place ourselves at some z distance, or we'd end up inside the cube.
    glTranslatef(0.0, 0.0, -5)

    # Without this some surfaces will not appear solid.
    glEnable(GL_DEPTH_TEST)

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()

        # Rotate 1 degree around the (3,1,1) vector for a nice spinning effect.
        angle = 1
        glRotatef(angle, 3, 1, 1)

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        Cube()
        pygame.display.flip()
        pygame.time.wait(10)

main()

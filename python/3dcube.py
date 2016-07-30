# Shows a rotating 3D cube using OpenGL.
# https://youtu.be/R4n4NyDG2hI

import pygame
from pygame.locals import *

from OpenGL.GL import *
from OpenGL.GLU import *


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


def Cube():
    # Tell OpenGL that it should draw lines between the vertices we give it.
    glBegin(GL_LINES)

    # Give it the vertices in the order as specified by the edges.
    for edge in edges:
        for vertex in edge:
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

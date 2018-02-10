"""
Small Python decorator example.
"""

from __future__ import print_function

def verbose(function):
    """
    Decorator to print function arguments and results after calling.
    """
    def wrapper(*args, **kwargs):
        """
        Calls function, then prints arguments and results.
        """
        result = function(*args, **kwargs)

        print('{}('.format(function.__name__), end='')
        print(', '.join(map(str, args)), end='')
        if args and kwargs:
            print(', ', end='')
        print(', '.join('{0}={1}'.format(k,v) for k,v in kwargs.items()), end='')
        print(') -> {}'.format(result))

        return result

    return wrapper

@verbose
def square(x):
    """
    Squares its argument.
    """
    return x * x

# >>> square(3)
# square(3) -> 9
# 9

@verbose
def greet(name, **_kwargs):
    """
    Test function.
    """
    print('Hello, {}!'.format(name))

# >>> greet('John Doe', age=100)
# Hello, John Doe!
# greet(John Doe, age=100) -> None
from functools import wraps


def printer(fkn):
    @wraps(fkn)
    def wrapped(*args, **kwargs):
        print(fkn.__name__, f"args: {args[1:]}", f"kwargs: {kwargs}")
        return fkn(*args, **kwargs)

    return wrapped
